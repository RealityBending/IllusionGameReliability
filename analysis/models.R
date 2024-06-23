library(tidyverse)
library(brms)

path <- "/mnt/lustre/users/psych/dmm56/IllusionGameReliability/"
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

# t0 <- Sys.time()
#
# formula <- brms::bf(
#   Error ~ Illusion_Effect / (logmod(Illusion_Difference) * abs(Illusion_Strength)) +
#     (1 + Illusion_Effect / (logmod(Illusion_Difference) * abs(Illusion_Strength)) | Participant),
#   family = "bernoulli",
#   decomp = "QR"
# )
#
# illusion1_ebbinghaus_err <- brms::brm(formula,
#                                       data = filter(df, Illusion_Type == "Ebbinghaus"),
#                                       refresh = 100,
#                                       normalize = FALSE,
#                                       init = 0,
#                                       seed=123,
#                                       iter=iter,
#                                       chains=cores,
#                                       cores=cores,
#                                       stan_model_args = list(stanc_options = list("O1"))
#                                       )
#
#
#
# t1 <- Sys.time()
# log <- c(log, "m1" = as.numeric(difftime(t1, t0, units = "min")))
# write.csv(as.data.frame(log), paste0(path, "log.csv"))
# save(illusion1_ebbinghaus_err,
#      file = paste0(path, "models/illusion1_ebbinghaus_err.Rdata"))
# rm(illusion1_ebbinghaus_err)



t0 <- Sys.time()

formula <- brms::bf(
  RT ~ Illusion_Effect / (Illusion_Difference * abs(Illusion_Strength)) + poly(ISI, 2) +
    (1 + Illusion_Effect / (Illusion_Difference * abs(Illusion_Strength)) | Participant),
  sigma ~ Illusion_Effect / (Illusion_Difference * abs(Illusion_Strength)) + poly(ISI, 2) +
    (1 | Participant),
  beta ~ Illusion_Effect / (Illusion_Difference * abs(Illusion_Strength)) + poly(ISI, 2) +
    (1 | Participant),
  family = "exgaussian",
  decomp = "QR"
)

illusion1_ebbinghaus_rt <- brms::brm(formula,
                                     data = filter(df, Illusion_Type == "Ebbinghaus", Error == 0),
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
log <- c(log, "m2" = as.numeric(difftime(t1, t0, units = "min")))
write.csv(as.data.frame(log), paste0(path, "log.csv"))
save(illusion1_ebbinghaus_rt,
     file = paste0(path, "models/illusion1_ebbinghaus_rt.Rdata"))
rm(illusion1_ebbinghaus_rt)


# MullerLyer --------------------------------------------------------------

t0 <- Sys.time()

formula <- brms::bf(
  Error ~ Illusion_Effect / (logmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) +
    (1 + Illusion_Effect / (logmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) | Participant),
  family = "bernoulli",
  decomp = "QR"
)

illusion1_mullerlyer_err <- brms::brm(formula,
                                      data = filter(df, Illusion_Type == "MullerLyer"),
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
log <- c(log, "m3" = as.numeric(difftime(t1, t0, units = "min")))
write.csv(as.data.frame(log), paste0(path, "log.csv"))
save(illusion1_mullerlyer_err,
     file = paste0(path, "models/illusion1_mullerlyer_err.Rdata"))
rm(illusion1_mullerlyer_err)


t0 <- Sys.time()

formula <- brms::bf(
  RT ~ Illusion_Effect / (sqrtmod(Illusion_Difference) * abs(Illusion_Strength)) + poly(ISI, 2) +
    (1 + Illusion_Effect / (sqrtmod(Illusion_Difference) * abs(Illusion_Strength)) | Participant),
  sigma ~ Illusion_Effect / (sqrtmod(Illusion_Difference) * abs(Illusion_Strength)) + poly(ISI, 2) +
    (1 | Participant),
  beta ~ Illusion_Effect / (sqrtmod(Illusion_Difference) * abs(Illusion_Strength)) + poly(ISI, 2) +
    (1 | Participant),
  family = "exgaussian",
  decomp = "QR"
)


illusion1_mullerlyer_rt <- brms::brm(formula,
                                     data = filter(df, Illusion_Type == "MullerLyer", Error == 0),
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
log <- c(log, "m4" = as.numeric(difftime(t1, t0, units = "min")))
write.csv(as.data.frame(log), paste0(path, "log.csv"))
save(illusion1_mullerlyer_rt,
     file = paste0(path, "models/illusion1_mullerlyer_rt.Rdata"))
rm(illusion1_mullerlyer_rt)


# Vertical-Horizontal --------------------------------------------------------------

t0 <- Sys.time()

formula <- brms::bf(
  Error ~ Illusion_Effect / (sqrtmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) +
    (1 + Illusion_Effect / (sqrtmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) | Participant),
  family = "bernoulli",
  decomp = "QR"
)

illusion1_verticalhorizontal_err <- brms::brm(formula,
                                      data = filter(df, Illusion_Type == "VerticalHorizontal"),
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
log <- c(log, "m5" = as.numeric(difftime(t1, t0, units = "min")))
write.csv(as.data.frame(log), paste0(path, "log.csv"))
save(illusion1_verticalhorizontal_err,
     file = paste0(path, "models/illusion1_verticalhorizontal_err.Rdata"))
rm(illusion1_verticalhorizontal_err)


t0 <- Sys.time()

formula <- brms::bf(
  RT ~ Illusion_Effect / (cbrtmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) + poly(ISI, 2) +
    (1 + Illusion_Effect / (cbrtmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) | Participant),
  sigma ~ Illusion_Effect / (cbrtmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) + poly(ISI, 2) +
    (1 | Participant),
  beta ~ Illusion_Effect / (cbrtmod(Illusion_Difference) * sqrtmod(abs(Illusion_Strength))) + poly(ISI, 2) +
    (1 | Participant),
  family = "exgaussian",
  decomp = "QR"
)


illusion1_verticalhorizontal_rt <- brms::brm(formula,
                                     data = filter(df, Illusion_Type == "VerticalHorizontal", Error == 0),
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
log <- c(log, "m6" = as.numeric(difftime(t1, t0, units = "min")))
write.csv(as.data.frame(log), paste0(path, "log.csv"))
save(illusion1_verticalhorizontal_rt,
     file = paste0(path, "models/illusion1_verticalhorizontal_rt.Rdata"))
rm(illusion1_verticalhorizontal_rt)

