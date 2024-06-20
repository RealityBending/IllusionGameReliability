library(tidyverse)
library(brms)

path <- "/mnt/lustre/users/psych/dmm56/IllusionGameReliability/models/"
# path <- "./models/"
iter <- 1000
cores <- parallel::detectCores(logical = FALSE)
# cores <- 8

options(mc.cores = cores,
        brms.backend = "cmdstanr")

log <- c("Cores" = cores)
write.csv(as.data.frame(log), paste0(path, "log.csv"))

# Data
df <- read.csv("https://raw.githubusercontent.com/RealityBending/IllusionGameReliability/main/data/preprocessed_illusion1.csv") |>
  mutate(
    Block = as.factor(Block),
    Illusion_Side = as.factor(Illusion_Side),
    Illusion_Effect = fct_relevel(as.factor(Illusion_Effect), "Incongruent", "Congruent")
  )


logmod <- function(x) sign(x) * log(1 + abs(x))
sqrtmod <- function(x) sign(x) * sqrt(abs(x))
cbrtmod <- function(x) sign(x) * (abs(x)**(1 / 3))

log <- c(log, "Data" = TRUE)
write.csv(as.data.frame(log), paste0(path, "log.csv"))


# Session 1 ===============================================================
# Ebbinghaus --------------------------------------------------------------

t0 <- Sys.time()

formula <- brms::bf(
  Error ~ Illusion_Effect / (logmod(Illusion_Difference) * abs(Illusion_Strength)) +
    (1 + Illusion_Effect / (logmod(Illusion_Difference) * abs(Illusion_Strength)) | Participant),
  family = "bernoulli",
  decomp = "QR"
)

illusion1_ebbinghaus_err <- brms::brm(formula,
                                      data = data,
                                      refresh = 100,
                                      normalize = FALSE,
                                      init = 0,
                                      seed=123,
                                      iter=iter,
                                      chains=cores,
                                      cores=cores,
                                      stan_model_args = list(stanc_options = list("O1"))
                                      )



t1 <- Sys.time()
log <- c(log, "m1" = as.numeric(difftime(t1, t0, units = "min")))
write.csv(as.data.frame(log), paste0(path, "log.csv"))
save(illusion1_ebbinghaus_err,
     file = paste0(path, "illusion1_ebbinghaus_err.Rdata"))
rm(illusion1_ebbinghaus_err)
