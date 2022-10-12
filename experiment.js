/* ----------------- Internal Functions ----------------- */
function get_results(illusion_mean, illusion_sd, illusion_type) {
    if (typeof illusion_type != "undefined") {
        var trials = jsPsych.data
            .get()
            .filter({ screen: "Trial", type: illusion_type }) // results by block
    } else {
        var trials = jsPsych.data.get().filter({ screen: "Trial" }) // overall results
    }
    var correct_trials = trials.filter({ correct: true })
    var proportion_correct = correct_trials.count() / trials.count()
    var rt_mean = trials.select("rt").mean()
    if (correct_trials.count() > 0) {
        var rt_mean_correct = correct_trials.select("rt").mean()
        var ies = rt_mean_correct / proportion_correct // compute inverse efficiency score
        var score_to_display = 100 - ies / 50
        if (score_to_display < 0) {
            score_to_display = 0
        }
        var percentile =
            100 - cumulative_probability(ies, illusion_mean, illusion_sd) * 100
    } else {
        var rt_mean_correct = ""
        var ies = ""
        var percentile = 0
        var score_to_display = 0
    }
    return {
        accuracy: proportion_correct,
        mean_reaction_time: rt_mean,
        mean_reaction_time_correct: rt_mean_correct,
        inverse_efficiency: ies,
        percentage: percentile,
        score: score_to_display,
    }
}

function get_debrief_display(results, type = "Block") {
    if (type === "Block") {
        // Debrief at end of each block
        var score =
            "<p>Your score for this illusion is " +
            '<p style="color: black; font-size: 48px; font-weight: bold;">' +
            Math.round(results.score * 10) / 10 +
            " %</p>"
    } else if (type === "Final") {
        // Final debriefing at end of game
        var score =
            "<p><strong>Your final score is</strong> " +
            '<p style="color: black; font-size: 48px; font-weight: bold;">&#127881; ' +
            Math.round(results.score) +
            " &#127881;</p>"
    }

    return {
        display_score: score,
        display_accuracy:
            "<p style='color:rgb(76,175,80);'>You responded correctly on <b>" +
            round_digits(results.accuracy * 100) +
            "" +
            "%</b> of the trials.</p>",
        display_rt:
            "<p style='color:rgb(233,30,99);'>Your average response time was <b>" +
            round_digits(results.mean_reaction_time) +
            "</b> ms.</p>",
        display_comparison:
            "<p style='color:rgb(76,175,80);'>You performed better than <b>" +
            round_digits(results.percentage) +
            "</b>% of the population.</p>",
    }
}

// Set fixation cross to jitter
function fixation_cross() {
    var fixation = {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: function () {
            return (
                '<p style="color: black; font-size: 80px; padding-left: ' +
                randomInteger(0, 50) +
                "%; padding-right: " +
                randomInteger(0, 50) +
                "%; padding-top: " +
                randomInteger(0, 50) +
                "%; padding-bottom: " +
                randomInteger(0, 50) +
                '%">+</p>'
            )
        },
        choices:
            "NO_KEYS" /* no responses will be accepted as a valid response */,
        // trial_duration: 0, // (for testing)
        trial_duration: function () {
            return randomInteger(500, 1000)
        },
        save_trial_parameters: {
            trial_duration: true,
        },
        data: { screen: "fixation" },
    }
    return fixation
}

// Break
var make_break1 = {
    type: jsPsychHtmlButtonResponse,
    choices: ["I am ready to continue!"],
    stimulus:
        "<p><b>CONGRATULATIONS!</b></p>" +
        "<p>You have finished half of the game. We know it's long and challenging, so we appreciate you staying focused until the end!</p>" +
        "<p>Before you see all the illusions once again, let's take a break by answering a few questions about yourself.</p>",
    save_trial_parameters: {
        trial_duration: true,
    },
    data: { screen: "break1" },
}
var make_break2 = {
    type: jsPsychHtmlButtonResponse,
    choices: ["I am ready to continue!"],
    stimulus:
        "<p><b>Back to the illusions</b></p>" +
        "<p>Try to improve your previous score!</p>",
    save_trial_parameters: {
        trial_duration: true,
    },
    data: { screen: "break2" },
}

// Marker
var marker_position = [0, 0, 0, 0] // [0, 0, 100, 100]
function create_marker(marker_position, color = "black") {
    const html = `<div id="marker" style="position: absolute; background-color: ${color};\
    left:${marker_position[0]}; top:${marker_position[1]}; \
    width:${marker_position[2]}px; height:${marker_position[3]}px";></div>`
    document.querySelector("body").insertAdjacentHTML("beforeend", html)
}

// Trial
function create_trial(illusion_name = "Ponzo", type = "updown") {
    if (type == "updown") {
        var trial = {
            type: jsPsychImageKeyboardResponse,
            stimulus: jsPsych.timelineVariable("stimulus"),
            choices: ["arrowup", "arrowdown"],
            data: jsPsych.timelineVariable("data"),
            on_load: function () {
                create_marker(marker_position)
            },
            on_finish: function (data) {
                document.querySelector("#marker").remove()
                data.prestimulus_duration =
                    jsPsych.data.get().last(2).values()[0].time_elapsed -
                    jsPsych.data.get().last(3).values()[0].time_elapsed
                // Score the response as correct or incorrect.
                if (data.response != -1) {
                    if (
                        jsPsych.pluginAPI.compareKeys(
                            data.response,
                            data.correct_response
                        )
                    ) {
                        data.correct = true
                    } else {
                        data.correct = false
                    }
                } else {
                    // code mouse clicks as correct or wrong
                    if (data.click_x < window.innerHeight / 2) {
                        // use window.innerHeight for up vs down presses
                        data.response = "arrowdown"
                    } else {
                        data.response = "arrowup"
                    }
                    if (
                        jsPsych.pluginAPI.compareKeys(
                            data.response,
                            data.correct_response
                        )
                    ) {
                        data.correct = true
                    } else {
                        data.correct = false
                    }
                }
                // track block and trial numbers
                data.type = illusion_name
                data.illusion_strength =
                    jsPsych.timelineVariable("Illusion_Strength")
                data.illusion_difference =
                    jsPsych.timelineVariable("Difference")
                data.block_number = block_number
                data.trial_number = trial_number
                trial_number += 1
            },
        }
    } else {
        var trial = {
            type: jsPsychImageKeyboardResponse,
            stimulus: jsPsych.timelineVariable("stimulus"),
            choices: ["arrowleft", "arrowright"],
            data: jsPsych.timelineVariable("data"),
            on_load: function () {
                create_marker(marker_position)
            },
            on_finish: function (data) {
                document.querySelector("#marker").remove()
                data.prestimulus_duration =
                    jsPsych.data.get().last(2).values()[0].time_elapsed -
                    jsPsych.data.get().last(3).values()[0].time_elapsed
                // Score the response as correct or incorrect.
                if (data.response != -1) {
                    if (
                        jsPsych.pluginAPI.compareKeys(
                            data.response,
                            data.correct_response
                        )
                    ) {
                        data.correct = true
                    } else {
                        data.correct = false
                    }
                } else {
                    // code mouse clicks as correct or wrong
                    if (data.click_x < window.innerWidth / 2) {
                        // use window.innerHeight for up vs down presses
                        data.response = "arrowleft"
                    } else {
                        data.response = "arrowright"
                    }
                    if (
                        jsPsych.pluginAPI.compareKeys(
                            data.response,
                            data.correct_response
                        )
                    ) {
                        data.correct = true
                    } else {
                        data.correct = false
                    }
                }
                // track block and trial numbers
                data.type = illusion_name
                data.illusion_strength =
                    jsPsych.timelineVariable("Illusion_Strength")
                data.illusion_difference =
                    jsPsych.timelineVariable("Difference")
                data.block_number = block_number
                data.trial_number = trial_number
                trial_number += 1
            },
        }
    }
    return trial
}

// Debrief
function create_debrief(illusion_name = "Ponzo") {
    var debrief = {
        type: jsPsychHtmlButtonResponse,
        choices: ["Continue"],
        on_start: function () {
            ;(document.body.style.cursor = "auto"),
                (document.querySelector(
                    "#jspsych-progressbar-container"
                ).style.display = "inline")
        },
        stimulus: function () {
            var results = get_results(
                1000, // population_scores[illusion_name]["IES_Mean"][0],
                400, // population_scores[illusion_name]["IES_SD"][0],
                illusion_name
            )
            var show_screen = get_debrief_display(results)
            return (
                show_screen.display_score +
                // "<hr>" +
                // // For debugging purposes, show the raw data.
                // show_screen.display_accuracy +
                // "<hr>" +
                // show_screen.display_rt +
                // "<hr>" +
                // //
                // show_screen.display_comparison +
                "<hr><p>Can you do better in the next illusion?</p>"
            )
        },
        data: { screen: "block_results" },
        // Reset trial number and update block number
        on_finish: function () {
            block_number += 1
            trial_number = 1
        },
    }
    return debrief
}

// Debrief
function make_trial(stimuli, instructions, illusion_name, type) {
    var timeline = []

    // Set stimuli (var stimuli is loaded in stimuli/stimuli.js)
    var stim_list = stimuli.filter(
        (stimuli) => stimuli.Illusion_Type === illusion_name
    )

    // Preload images
    timeline.push({
        type: jsPsychPreload,
        images: stim_list.map((a) => a.stimulus),
    })

    // Instructions
    timeline.push({
        type: jsPsychHtmlKeyboardResponse,
        on_start: function () {
            ;(document.body.style.cursor = "none"),
                (document.querySelector(
                    "#jspsych-progressbar-container"
                ).style.display = "none")
        },
        choices: ["enter"],
        stimulus: instructions,
        post_trial_gap: 500,
    })

    // Create Trials timeline
    timeline.push({
        timeline: [
            fixation_cross(),
            create_trial(illusion_name, (type = type)),
        ],
        timeline_variables: stim_list,
        randomize_order: true,
        repetitions: 1,
    })

    // Debriefing Information
    if (stimuli == stimuli_part1 || stimuli == stimuli_part2) {
        timeline.push(create_debrief((illusion_name = illusion_name)))
    } else if (stimuli === stimuli_training) {
        timeline.push({
            type: jsPsychHtmlButtonResponse,
            choices: ["Continue"],
            post_trial_gap: 500,
            on_start: function () {
                ;(document.body.style.cursor = "auto"),
                    (document.querySelector(
                        "#jspsych-progressbar-container"
                    ).style.display = "inline")
            },
            stimulus: "<p><b>Great job!</b></p>",
            data: { screen: "practice_block" },
        })
    } else {
        timeline.push({
            type: jsPsychHtmlButtonResponse,
            choices: ["Continue"],
            post_trial_gap: 500,
            on_start: function () {
                ;(document.body.style.cursor = "auto"),
                    (document.querySelector(
                        "#jspsych-progressbar-container"
                    ).style.display = "inline")
            },
            stimulus: "<p><b>Can you do better in the next round?</b></p>" +
                    "<p>Remember, your goal is still to be as <b>fast</b> and <b>accurate</b> as possible.</p>",
            data: { screen: "perceptual_block_results" },
        // Reset trial number and update block number
            on_finish: function () {
                block_number += 1
                trial_number = 1
            }
        })
    }
    return timeline
}

// Instructions for Illusion Trials

const mullerlyer_instructions =
    "<p>In this part, two horizontal red lines will appear one above the other.</p>" +
    "<p>Your task is to select which <b>line is longer</b> in length as fast as you can, without making errors.</p>" +
    "<p>Don't get distracted by the surrounding black arrows at the end of the red lines!</p>" +
    "<p>Press <b>the UP or the DOWN arrow</b> to indicate where is the longer <b>red line.</b></p>" +
    "<div style='float: center'><img src='materials/instructions/MullerLyer_Demo.png' height='300'></img>" +
    "<p><img src='utils/answer/answer_updown_keyboard.PNG' height='150'></img></p>" +
    "<p class='small'>In this example, the correct answer is the <b>UP arrow</b>.</p></div>" +
    "<p>Are you ready? <b>Press ENTER to start</b></p>"

const ebbinghaus_instructions =
    "<p>In this part, two red circles will appear side by side on the screen.</p>" +
    "<p>Your task is to select which <b>red circle is bigger</b> in size as fast as you can, without making errors.</p>" +
    "<p>Don't get distracted by the surrounding black circles around the red circles!</p>" +
    "<p>Press <b>the LEFT or the RIGHT arrow</b> to indicate which is the bigger <b>red circle.</b></p>" +
    "<div style='float: center'><img src='materials/instructions/Ebbinghaus_Demo.png' height='300'></img>" +
    "<p><img src='utils/answer/answer_leftright_keyboard.PNG' height='150'></img></p>" +
    "<p class='small'>In this example, the correct answer is the <b>LEFT arrow</b>.</p></div>" +
    "<p>Are you ready? <b>Press ENTER to start</b></p>"

const verticalhorizontal_instructions =
    "<p>In this part, two red lines will appear side by side.</p>" +
    "<p>Your task is to tell <b>which line is longer</b> in length as fast as you can, and without making errors.</p>" +
    "<p>Don't get distracted by the orientation of the lines!</p>" +
    "<p>Press <b>the LEFT or the RIGHT arrow</b> to indicate which <b>line is the longer one.</b></p>" +
    "<div style='float: center'><img src='materials/instructions/VerticalHorizontal_Demo.png' height='300'></img>" +
    "<p><img src='utils/answer/answer_leftright_keyboard.PNG' height='150'></img></p>" +
    "<p class='small'>In this example, the correct answer is the <b>LEFT arrow</b>.</p></div>" +
    "<p>Are you ready? <b>Press ENTER to start</b></p>"

// Instructions for Perceptual Trials

const mullerlyer_instructions_perceptual =
    "<p>In this part, two parallel horizontal red lines will appear one above the other.</p>" +
    "<p>Your task is to select which <b>line is longer</b> in length as fast as you can, without making errors.</p>" +
    "<p>Press <b>the UP or the DOWN arrow</b> to indicate where is the longer <b>red line.</b></p>" +
    "<div style='float: center'><img src='materials/instructions/MullerLyer_DemoPerceptual.png' height='300'></img>" +
    "<p><img src='utils/answer/answer_updown_keyboard.PNG' height='150'></img></p>" +
    "<p class='small'>In this example, the correct answer is the <b>UP arrow</b>.</p></div>" +
    "<p>Are you ready? <b>Press ENTER to start</b></p>"

const ebbinghaus_instructions_perceptual =
    "<p>In this part, two red circles will appear side by side on the screen.</p>" +
    "<p>Your task is to select which <b>red circle is bigger</b> in size as fast as you can, without making errors.</p>" +
    "<p>Press <b>the LEFT or the RIGHT arrow</b> to indicate which is the bigger <b>red circle.</b></p>" +
    "<div style='float: center'><img src='materials/instructions/Ebbinghaus_DemoPerceptual.png' height='300'></img>" +
    "<p><img src='utils/answer/answer_leftright_keyboard.PNG' height='150'></img></p>" +
    "<p class='small'>In this example, the correct answer is the <b>LEFT arrow</b>.</p></div>" +
    "<p>Are you ready? <b>Press ENTER to start</b></p>"

const verticalhorizontal_instructions_perceptual =
    "<p>In this part, two parallel red lines will appear side by side.</p>" +
    "<p>Your task is to tell <b>which line is longer</b> in length as fast as you can, and without making errors.</p>" +
    "<p>Press <b>the LEFT or the RIGHT arrow</b> to indicate which <b>line is the longer one.</b></p>" +
    "<div style='float: center'><img src='materials/instructions/VerticalHorizontal_DemoPerceptual.png' height='300'></img>" +
    "<p><img src='utils/answer/answer_leftright_keyboard.PNG' height='150'></img></p>" +
    "<p class='small'>In this example, the correct answer is the <b>LEFT arrow</b>.</p></div>" +
    "<p>Are you ready? <b>Press ENTER to start</b></p>"

/* Psychometric scales---------------------------------------------------------------------*/

// Mini IPIP scale (Big 5 traits)
var IPIP = [
    "<b>I am the life of the party</b><br>",
    "<b>I sympathize with others' feelings</b><br>",
    "<b>I get chores done right away</b><br>",
    "<b>I have frequent mood swings</b><br>",
    "<b>I have a vivid imagination</b><br>",
    "<b>I feel entitled to more of everything</b><br>",
    "<b>I do not talk a lot</b><br>",
    "<b>I am not interested in other people's problems</b><br>",
    "<b>I have difficulty understanding abstract ideas</b><br>",
    "<b>I like order</b><br>",
    "<b>I make a mess of things</b><br>",
    "<b>I deserve more things in life</b><br>",
    "<b>I do not have a good imagination</b><br>",
    "<b>I feel other's emotions</b><br>",
    "<b>I am relaxed most of the time</b><br>",
    "<b>I get upset easily</b><br>",
    "<b>I seldom feel blue</b><br>",
    "<b>I would like to be seen driving around in a very expensive car</b><br>",
    "<b>I keep in the background</b><br>",
    "<b>I am not really interested in others</b><br>",
    "<b>I am not interested in abstract ideas</b><br>",
    "<b>I often forget to put things back in their proper place</b><br>",
    "<b>I talk to a lot of different people at parties</b><br>",
    "<b>I would get a lot of pleasure from owning expensive luxury goods</b><br>",
]

var IPIP_dim = [
    "Extraversion_1",
    "Agreeableness_2",
    "Conscientiousness_3",
    "Neuroticism_4",
    "Openness_5",
    "HonestyHumility_6_R",
    "Extraversion_7_R",
    "Agreeableness_8_R",
    "Openness_9_R",
    "Conscientiousness_10",
    "Conscientiousness_11_R",
    "HonestyHumility_12_R",
    "Openness_13_R",
    "Agreeableness_14",
    "Neuroticism_15_R",
    "Neuroticism_16",
    "Neuroticism_17_R",
    "HonestyHumility_18_R",
    "Extraversion_19_R",
    "Agreeableness_20_R",
    "Openness_21_R",
    "Conscientiousness_22_R",
    "Extraversion_23",
    "HonestyHumility_24_R",
]

// Personality Inventory for DSM-V - Brief (Maladaptive Traits)
var PID = [
    "<b>People would describe me as reckless</b><br>",
    "<b>I feel like I act totally on impulse</b><br>",
    "<b>Even though I know better, I can't stop making rash decisions</b><br>",
    "<b>I often feel like nothing I do really matters</b><br>",
    "<b>Others see me as irresponsible</b><br>",
    "<b>I'm not good at planning ahead</b><br>",
    "<b>My thoughts often don't make sense to others</b><br>",
    "<b>I worry about almost everything</b><br>",
    "<b>I get emotional easily, often for very little reason</b><br>",
    "<b>I fear being alone in life more than anything else</b><br>",
    "<b>I get stuck on one way of doing things, even when it's clear it won't work</b><br>",
    "<b>I have seen things that weren't really there</b><br>",
    "<b>I steer clear of romantic relationships</b><br>",
    "<b>I'm not interested in making friends</b><br>",
    "<b>I get irritated easily by all sorts of things</b><br>",
    "<b>I don't like to get too close to people</b><br>",
    "<b>It's no big deal if I hurt other people's feelings</b><br>",
    "<b>I rarely get enthusiastic about anything</b><br>",
    "<b>I crave attention</b><br>",
    "<b>I often have to deal with people who are less important than me</b><br>",
    "<b>I often have thoughts that make sense to me but that other people say are strange</b><br>",
    "<b>I use people to get what I want</b><br>",
    "<b>I often 'zone out' and then suddenly come to and realize that a lot of time has passed</b><br>",
    "<b>Things around me often feel unreal, or more real than usual</b><br>",
    "<b>It is easy for me to take advantage of others</b><br>",
]

var PID_dim = [
    "Disinhibition_1",
    "Disinhibition_2",
    "Disinhibition_3",
    "Detachment_4",
    "Disinhibition_5",
    "Disinhibition_6",
    "Psychoticism_7",
    "NegativeAffect_8",
    "NegativeAffect_9",
    "NegativeAffect_10",
    "NegativeAffect_11",
    "Psychoticism_12",
    "Detachment_13",
    "Detachment_14",
    "NegativeAffect_15",
    "Detachment_16",
    "Antagonism_17",
    "Detachment_18",
    "Antagonism_19",
    "Antagonism_20",
    "Psychoticism_21",
    "Antagonism_22",
    "Psychoticism_23",
    "Psychoticism_24",
    "Antagonism_25",
]

// Schizotypal Personality Questionnaire - Brief
// Note that some items are taken from a revised version of the schizotypal personality questionnaire  (https://hal.archives-ouvertes.fr/hal-03489508/document) that is adapted to be measured using a 5 point likert scale instead of the original dichotomous yes/no scale
var SPQ = [
    "<b>People sometimes find me aloof or distant</b><br>",
    "<b>I happen to feel an unseen force or presence around me </b><br>",
    "<b>People sometimes comment on my unusual mannerisms and habits</b><br>",
    "<b>I am sometimes convinced that other people are able to guess what I think</b><br>",
    "<b>Certain objects or ordinary situations that happen are special signs for me</b><br>",
    "<b>Some people think that I am a very bizarre person</b><br>",
    "<b>I feel that I have to be on guard even with friends</b><br>",
    "<b>Some people find me a bit vague and elusive during a conversation</b><br>",
    "<b>I often see hidden threats or derogatory remarks in what other people say or do</b><br>",
    "<b>When I go shopping I have the feeling that people notice me</b><br>",
    "<b>I feel very uncomfortable in social situations involving unfamiliar people</b><br>",
    "<b>I have had special experiences with astrology, premonitions, unidentified flying objects, extrasensory perceptions, or the sixth sense</b><br>",
    "<b>I sometimes use words in unusual ways</b><br>",
    "<b>I think it's better that people don't know too much about me</b><br>",
    "<b>I tend to keep in the background on social occasions.</b><br>",
    "<b>Sometimes, I'm suddenly distracted by distant sounds to which usually I don't pay much attention</b><br>",
    "<b>I often must be vigilant for other people not to take advantage of me</b><br>",
    "<b>I have the feeling that I can't get close to people</b><br>",
    "<b>I am an odd, unusual person</b><br>",
    "<b>I find it hard to communicate clearly what I want to say to people</b><br>",
    "<b>I feel very uneasy talking to people I do not know well</b><br>",
    "<b>I tend to keep my feelings to myself</b><br>"
]
// * changed items are 2, 4, 5, 9, 10, 12, 14, 16, 17, 18

var SPQ_dim = [
    "Interpersonal_1",
    "Cognitive-Perceptual_2",
    "Disorganized_3",
    "Cognitive-Perceptual_4",
    "Cognitive-Perceptual_5",
    "Disorganized_6",
    "Interpersonal_7",
    "Disorganized_8",
    "Cognitive-Perceptual_9",
    "Cognitive-Perceptual_10",
    "Interpersonal_11",
    "Cognitive_Perceptual_12",
    "Disorganized_13",
    "Interpersonal_14",
    "Interpersonal_15",
    "Cognitive-Perceptual_16",
    "Cognitive-Perceptual_17",
    "Interpersonal_18",
    "Disorganized_19",
    "Disorganized_20",
    "Interpersonal_21",
    "Interpersonal_22"
]


// Autism-Spectrum Quotient Short (18 items)
var AQ = [
    "<b>I prefer to do things with others rather than on my own</b><br>",
    "<b>I prefer to do things the same way over and over again</b><br>",
    "<b>Trying to imagine something, I find it easy to create a picture in my mind</b><br>",
    "<b>I frequently get strongly absorbed in one thing</b><br>",
    "<b>I usually notice car number plates or similar strings of information </b><br>",
    "<b>Reading a story, I can easily imagine what the characters might look like</b><br>",
    "<b>I am fascinated by dates</b><br>",
    "<b>I can easily keep track of several different people's conversations</b><br>",
    "<b>I find social situations easy</b><br>",
    "<b>I would rather go to a library than to a party</b><br>",
    "<b>I find making up stories easy</b><br>",
    "<b>I find myself drawn more strongly to people than to things</b><br>",
    "<b>I am fascinated by numbers</b><br>",
    "<b>Reading a story, I find it difficult to work out the character's intentions </b><br>",
    "<b>I find it hard to make new friends</b><br>",
    "<b>I notice patterns in things all the time</b><br>",
    "<b>It does not upset my if my daily routine is disturbed</b><br>",
    "<b>I find it easy to do more than one thing at once</b><br>",
    "<b>I enjoy doing things spontaneously</b><br>",
    "<b>I find it easy to work out what someone is thinking or feeling</b><br>",
    "<b>If there is an interruption, I can switch back very quickly</b><br>",
    "<b>I like to collect information about categories of things</b><br>",
    "<b>I find it difficult to imagine what it would be like to be someone else</b><br>",
    "<b>I enjoy social occasions </b><br>",
    "<b>I find it difficult to work out people's intentions</b><br>",
    "<b>New situations make me anxious</b><br>",
    "<b>I enjoy meeting new people</b><br>",
    "<b>I find it easy to play games with children that involve pretending</b><br>"
]

var AQ_dim =[
    "SocialSkills_1",
    "Routine_2",
    "Imagination_3",
    "Switching_4",
    "Patterns_5",
    "Imagination_6",
    "Patterns_7",
    "Switching_8",
    "SocialSkills_9",
    "SocialSkills_10",
    "Imagination_11",
    "SocialSkills_12",
    "Patterns_13",
    "Imagination_14",
    "SocialSkills_15",
    "Patterns_16",
    "Routine_17",
    "Switching_18",
    "Routine_19",
    "Imagination_20",
    "Switching_21",
    "Patterns_22",
    "Imagination_23",
    "SocialSkills_24",
    "Imagination_25",
    "Routine_26",
    "SocialSkills_27",
    "Imagination_28"
]

// Cognitive Flexibility Inventory
var CFI = [
    "<b>I am good at 'sizing up' situations</b><br>",
    "<b>I have a hard time making decisions when faced with difficult situations </b><br>",
    "<b>I consider multiple options before making a decision</b><br>",
    "<b>When I encounter difficult situations, I feel like I am losing control </b><br>",
    "<b>I like to look at difficult situations from many different angles</b><br>",
    "<b>I seek additional information not immediately available before attributing causes to behaviour</b><br>",
    "<b>When encountering difficult situations, I become so stressed that I can not think of a way to resolve the situation</b><br>",
    "<b>I try to think about things from another person's point of view</b><br>",
    "<b>I find it troublesome that there are so many different ways to deal with difficult situations</b><br>",
    "<b>I am good at putting myself in others' shoes</b><br>",
    "<b>When I encounter difficult situations, I just don't know what to do</b><br>",
    "<b>It is important to look at difficult situations from many angles</b><br>",
    "<b>When in difficult situations, I consider multiple options before deciding how to behave </b><br>",
    "<b>I often look at a situation from different viewpoints</b><br>",
    "<b>I am capable of overcoming the difficulties in life that I face</b><br>",
    "<b>I consider all the available facts and information when attributing causes to behaviour</b><br>",
    "<b>I feel I have no power to change things in difficult situations </b><br>",
    "<b>When I encounter difficult situations, I stop and try to think of several ways to resolve it</b><br>",
    "<b>I can think of more than one way to resolve a difficult situation I'm confronted with</b><br>",
    "<b>I consider multiple options before responding to difficult situations</b><br>"
]

var CFI_dim =[
     "Alternatives_1",
     "Control_2_R",
     "Alternatives_3",
     "Control_4_R",
     "Alternatives_5",
     "Alternatives_6",
     "Control_7_R",
     "Alternatives_8",
     "Control_9_R",
     "Alternatives_10",
     "Control_11_R",
     "Alternatives_12",
     "Alternatives_13",
     "Alternatives_14",
     "Control_15",
     "Alternatives_16",
     "Control_17_R",
     "Alternative_18",
     "Alternatives_19",
     "Alternatives_20"
]

// Short form - Five Factor Mindfulness Questionnaire (15 items)
var FFMQ = [
    "<b>When I take a shower or a bath, I stay alert to the sensations of water on my body.</b><br>",
    "<b>I'm good at finding words to describe my feelings</b><br>",
    "<b>I don't pay attention to what I'm doing because I'm daydreaming, worrying, or otherwise distracted</b><br>",
    "<b>I believe some of my thoughts are abnormal or bad and I shouldn't think that way</b><br>",
    "<b>When I have distressing thoughts or images, I 'step back' and am aware of the thought or image without getting taken over by it</b><br>",
    "<b>I notice how foods and drinks affect my thoughts, bodily sensations, and emotions</b><br>",
    "<b>I have trouble thinking of the right words to express how I feel about things</b><br>",
    "<b>I do jobs or tasks automatically without being aware of what I'm doing</b><br>",
    "<b>I think some of my emotions are bad or inappropriate and I shouldn't feel </b><br>",
    "<b>When I have distressing thoughts or images I am able just to notice them without reacting.</b><br>",
    "<b>I pay attention to sensations, such as the wind in my hair or sun on my face</b><br>",
    "<b>Even when I'm feeling terribly upset I can find a way to put it into words</b><br>",
    "<b>I find myself doing things without paying attention</b><br>",
    "<b>I tell myself I shouldn't be feeling the way I'm feeling</b><br>",
    "<b>When I have distressing thoughts or images I just notice them and let them go</b><br>"
]

var FFMQ_dim =[
    "Observation_1",
    "Description_2",
    "Awareness_3_R",
    "NonJudgemental_4_R",
    "NonReactivity_5",
    "Observation_6",
    "Description_7_R",
    "Awareness_8_R",
    "NonJudgemental_9_R",
    "NonReactivity_10",
    "Observation_11",
    "Description_12",
    "Awareness_13_R",
    "NonJudgemental_14_R",
    "NonReactivity_15"
]

// General Conspiracist Beliefs Scale
var GCBS = [
    "<b>The government is involved in the murder of innocent citizens and/or well-known public figures, and keeps this a secret</b><br>",
    "<b>The power held by heads of state is second to that of small unknown groups who really control world politics</b><br>",
    "<b>Secret organizations communicate with extraterrestrials, but keep this fact from the public</b><br>",
    "<b>The spread of certain viruses and/or diseases is the result of the deliberate, concealed efforts of some organization</b><br>",
    "<b>Groups of scientists manipulate, fabricate, or suppress evidence in order to deceive the public</b><br>",
    "<b>The government permits or perpetrates acts of terrorism on its own soil, disguising its involvement</b><br>",
    "<b>A small, secret group of people is responsible for making all major world decisions, such as going to war</b><br>",
    "<b>Evidence of alien contact is being concealed from the public</b><br>",
    "<b>Technology with mind-control capacities is used on people without their knowledge</b><br>",
    "<b>New and advanced technology which would harm current industry is being suppressed</b><br>",
    "<b>The government uses people as patsies to hide its involvement in criminal activity</b><br>",
    "<b>Certain significant events have been the result of the activity of a small group who secretly manipulate world events</b><br>",
    "<b>Some UFO sightings and rumors are planned or staged in order to distract the public from real alien contact</b><br>",
    "<b>Experiments involving new drugs or technologies are routinely carried out on the public without their knowledge or consent</b><br>",
    "<b>A lot of important information is deliberately concealed from the public out of self-interest</b><br>"
]

var GCBS_dim = [
    "GM_1",
    "MF_2",
    "ET_3",
    "PW_4",
    "CI_5",
    "GM_6",
    "MF_7",
    "ET_8",
    "PW_9",
    "CI_10",
    "GM_11",
    "MF_12",
    "ET_13",
    "PW_14",
    "CI_15"
]

// Short UPPS-P Impulsive Behavior Scale
var SUPPS = [
    "<b>I generally like to see things through to the end</b><br>",
    "<b>My thinking is usually careful and purposeful</b><br>",
    "<b>When I am in great mood, I tend to get into situations that could cause me problems</b><br>",
    "<b>Unfinished tasks really bother me</b><br>",
    "<b>I like to stop and think things over before I do them</b><br>",
    "<b>When I feel bad, I will often do things I later regret in order to make myself feel better now</b><br>",
    "<b>Once I get going on something I hate to stop</b><br>",
    "<b>Sometimes when I feel bad, I can't seem to stop what I am doing even though it is making me feel worse.</b><br>",
    "<b>I quite enjoy taking risks</b><br>",
    "<b>I tend to lose control when I am in a great mood</b><br>",
    "<b>I finish what I start</b><br>",
    "<b>I tend to value and follow a rational, 'sensible' approach to things</b><br>",
    "<b>When I am upset I often act without thinking</b><br>",
    "<b>I welcome new and exciting experiences and sensations, even if they are a little frightening and unconventional</b><br>",
    "<b>When I feel rejected, I will often say things that I later regret</b><br>",
    "<b>I would like to learn to fly an airplane</b><br>",
    "<b>Others are shocked or worried about the things I do when I am feeling very excited</b><br>",
    "<b>I would enjoy the sensation of skiing very fast down a high mountain slope</b><br>",
    "<b>I usually think carefully before doing anything</b><br>",
    "<b>I tend to act without thinking when I am really excited</b><br>"
]

var SUPPS_dim = [
    "LackofPerseverance_1_R",
    "LackofPremeditation_2",
    "PositiveUrgency_3",
    "LackofPerseverance_4_R",
    "LackofPremeditation_5_R",
    "NegativeUrgency_6",
    "LackofPerseverance_7_R",
    "NegativeUrgency_8",
    "SensationSeeking_9",
    "PositiveUrgency_10",
    "LackofPerseverance_11",
    "LackofPremeditation_12_R",
    "NegativeUrgency_13",
    "sensationSeeking_14",
    "NegativeUrgency_15",
    "SensationSeeking_16",
    "PositiveUrgency_17",
    "SensationSeeking_18",
    "LackofPremeditation_19_R",
    "PositiveUrgency_20"
]

// Primal Beliefs Inventory - Brief (18 items) + Hierarchical + Changing + Understandable subscales (note that the order of items from the latter 3 subscales are in a validated fixed order, retrieved from www.authentichappiness.org)
var PI = [
    "<b>In life, there's way more beauty than ugliness</b><br>",
    "<b>It often feels like events are happening in order to help me in some way</b><br>",
    "<b>I tend to see the world as pretty safe</b><br>",
    "<b>What happens in the world is meant to happen</b><br>",
    "<b>While some things are worth checking out or exploring further, most things probably aren't worth the effort.</b><br>",
    "<b>Most things in life are kind of boring</b><br>",
    "<b>The world is an abundant place with tons and tons to offer</b><br>",
    "<b>No matter where we are or what the topic might be, the world is fascinating</b><br>",
    "<b>The world is a somewhat dull place where plenty of things are not that interesting</b><br>",
    "<b>On the whole, the world is a dangerous place</b><br>",
    "<b>Instead of being cooperative, the world is a cut-throat and competitive place</b><br>",
    "<b>Events seem to lack any cosmic or bigger purpose</b><br>",
    "<b>Most things have a habit of getting worse</b><br>",
    "<b>The universe needs me for something important</b><br>",
    "<b>Most things in the world are good</b><br>",
    "<b>Everything happens for a reason and on purpose</b><br>",
    "<b>Most things and situations are harmless and totally safe</b><br>",
    "<b>No matter where we are, incredible beauty is always around us</b><br>",
    "<b>The world is a place where most things stay pretty much the same</b><br>",
    "<b>Everything feels like it's constantly moving, changing, and up in the air</b><br>",
    "<b>Everything feels like it's shifting and changing</b><br>",
    "<b>Everything feels like a whirl of constant change</b><br>",
    "<b>I feel like everything changes all the time</b><br>",
    "<b>Most things in the world could be ranked in order of importance</b><br>",
    "<b>Things are rarely equal. Most plants and animals, and even people, are better or worse than one another</b><br>",
    "<b>Most things can be organized into hierarchies, rankings, or pecking orders that reflect true differences among things</b><br>",
    "<b>Humans, animals, plants, and pretty much everything else can be organized by how important or good they are.</b><br>",
    "<b>Most things aren't better or worse. It's hard to organize the world into hierarchies, rankings, or pecking orders that reflect true differences</b><br>",
    "<b>The world is easy enough to understand</b><br>",
    "<b>Most everything is easy enough to understand</b><br>",
    "<b>The world is a confusing place where many skills and subjects are too hard to figure out.</b><br>",
    "<b>Lots of things in the world are too confusing and difficult to understand</b><br>",
]

var PI_dim = [
    "GE_1",
    "GA_2",
    "GS_3",
    "A_4",
    "GE_5_R",
    "GE_6_R",
    "GE_7",
    "GE_8",
    "GE_9_R",
    "GS_10_R",
    "GS_11_R",
    "A_12_R",
    "GS_13_R",
    "GA_14",
    "GS_15",
    "A_16",
    "GS_17",
    "GE_18",
    "Changing_19_R",
    "Changing_20",
    "Changing_21",
    "Changing_22",
    "Changing_23",
    "Hierarchical_24",
    "Hierarchical_25",
    "Hierarchical_26",
    "Hierarchical_27",
    "Hierarchical_28_R",
    "Understandable_29",
    "Understandable_30",
    "Understandable_31_R",
    "Understandable_32_R"
]

// MSI- BPD (adapted from yes-no dichotomous scale to visual analog scale - definitely no/definitely yes; cf Huczewska et al., 2019 - https://doi.org/10.5114/cipp.2019.89674) - modify to Not at all True of me - Extremely true of me?
var MSI = [
    "<b>Have any of your closest relationships been troubled by a lot of arguments or repeated breakups?</b><br>",
    "<b>Have you deliberately hurt yourself physically (punched yourself, cut yourself, burned yourself) or made a suicide attempt?</b><br>",
    "<b>Have your had at least two other problems with impulsivity (e.g., eating binges and spending sprees, drinking too much and verbal outbursts)?</b><br>",
    "<b>Have you been extremely moody?</b><br>",
    "<b>Have you felt very angry a lot of the time or often acted in an angry or sarcastic manner?</b><br>",
    "<b>Have you often been distrustful of other people?</b><br>",
    "<b>Have you frequently felt unreal or as if things around you were unreal? </b><br>",
    "<b>Have you chronically felt empty? </b><br>",
    "<b>Have you often felt that you had no idea of who you are or that you have no identity</b><br>",
    "<b>Have you made desperate efforts to avoid feeling abandoned (e.g., repeatedly called someone to reassure yourself that he or she still cared, begged them not to leave you, clung to them physically)?</b><br>"
]


// Depression-Anxiety
var PHQ = [
    "<b>Feeling nervous, anxious or on edge</b><br>",
    "<b>Not being able to stop or control worrying</b><br>",
    "<b>Feeling down, depressed, or hopeless</b><br>",
    "<b>Little interest or pleasure in doing things</b><br>"
]

var PHQ_dim = [
    "Anxiety_1",
    "Anxiety_2",
    "Depression_3",
    "Depression_4"
]

// MAIA-2
var MAIA = [
    "<b>When I am tense I notice where the tension is located in my body</b><br>",
    "<b>I notice when I am uncomfortable in my body</b><br>",
    "<b>I notice where in my body I am comfortable</b><br>",
    "<b>I notice changes in my breathing, such as whether it slows down or speeds up</b><br>",
    "<b>I ignore physical tension or discomfort until they become more severe</b><br>",
    "<b>I distract myself from sensations of discomfort</b><br>",
    "<b>When I feel pain or discomfort, I try to power through it</b><br>",
    "<b>I try to ignore pain</b><br>",
    "<b>I push feelings of discomfort away by focusing on something</b><br>",
    "<b>When I feel unpleasant body sensations, I occupy myself with something else so I don't have to feel them</b><br>",
    "<b>When I feel physical pain, I become upset.</b><br>",
    "<b>I start to worry that something is wrong if I feel any discomfort</b><br>",
    "<b>I can notice an unpleasant body sensation without worrying about it</b><br>",
    "<b>I can stay calm and not worry when I have feelings of discomfort or pain</b><br>",
    "<b>When I am in discomfort or pain I can't get it out of my mind</b><br>",
    "<b>I can pay attention to my breath without being distracted by things happening around me</b><br>",
    "<b>I can maintain awareness of my inner bodily sensations even when there is a lot going on around me</b><br>",
    "<b>When I am in conversation with someone, I can pay attention to my posture</b><br>",
    "<b>I can return awareness to my body if I am distracted</b><br>",
    "<b>I can refocus my attention from thinking to sensing my body</b><br>",
    "<b>I can maintain awareness of my whole body even when a part of me is in pain or discomfort</b><br>",
    "<b>I am able to consciously focus on my body as a whole</b><br>",
    "<b>I notice how my body changes when I am angry</b><br>",
    "<b>When something is wrong in my life I can feel it in my body</b><br>",
    "<b>I notice that my body feels different after a peaceful experience</b><br>",
    "<b>I notice that my breathing becomes free and easy when I feel comfortable</b><br>",
    "<b>I notice how my body changes when I feel happy / joyful</b><br>",
    "<b>When I feel overwhelmed I can find a calm place inside</b><br>",
    "<b>When I bring awareness to my body I feel a sense of calm</b><br>",
    "<b>I can use my breath to reduce tension</b><br>",
    "<b>When I am caught up in thoughts, I can calm my mind by focusing on my body/breathing</b><br>",
    "<b>I listen for information from my body about my emotional state</b><br>",
    "<b>When I am upset, I take time to explore how my body feels</b><br>",
    "<b>I listen to my body to inform me about what to do</b><br>",
    "<b>I am at home in my body</b><br>",
    "<b>I feel my body is a safe place</b><br>",
    "<b>I trust my body sensations</b><br>"
]

var MAIA_dim = [
    "Noticing_1",
    "Noticing_2",
    "Noticing_3",
    "Noticing_4",
    "NotDistancing_5_R",
    "NotDistancing_6_R",
    "NotDistancing_7_R",
    "NotDistancing_8_R",
    "NotDistancing_9_R",
    "NotDistancing_10_R",
    "NotWorrying_11_R",
    "NotWorrying_12_R",
    "NotWOrrying_13",
    "NotWorrying_14",
    "NotWorrying_15_R",
    "Attention_16",
    "Attention_17",
    "Attention_18",
    "Attention_19",
    "Attention_20",
    "Attention_21",
    "Attention_22",
    "EmotionalAwareness_23",
    "EmotionalAwareness_24",
    "EmotionalAwareness_25",
    "EmotionalAwareness_26",
    "EmotionalAwareness_27",
    "SelfRegulation_28",
    "SelfRegulation_29",
    "SelfRegulation_30",
    "SelfRegulation_31",
    "BodyListening_32",
    "BodyListening_33",
    "BodyListening_34",
    "Trusting_35",
    "Trusting_36",
    "Trusting_37"
]

// Interoception Sensory Questionnaire (unidimensional scale) - Note that this scale has mostly been administered among people with ASD
var ISQ = [
    "<b>I have difficulty making sense of my body's signals unless they are very strong</b><br>",
    "<b>I tend to rely on visual reminders (e.g. times on the clock) to help me know when to eat and drink</b><br>",
    "<b>Even when I know that I am physically uncomfortable, I do not act to change my situation</b><br>",
    "<b>I'm not sure how my body feels when it's a hot day</b><br>",
    "<b>I find it difficult to describe feelings like hunger, thirst, hot or cold</b><br>",
    "<b>Sometimes I don't know how to interpret sensations I feel within my body</b><br>",
    "<b>Even when I know that I am hungry, thirsty, in pain, hot or cold, I don't feel the need to do anything about it</b><br>",
    "<b>If I injure myself badly, even though I can feel it, I don't feel the need to do much about it</b><br>",
    "<b>I only notice I need to eat when I'm in pain or feeling nauseous or weak</b><br>",
    "<b>There are times when I am only aware of changes in my body because of the reactions of other people</b><br>",
    "<b>I find it difficult to read the signs and signals within my own body (e.g. when I have hurt myself or I need to rest)</b><br>",
    "<b>I have difficulty understanding when I am hungry or thirsty</b><br>",
    "<b>I find it difficult to identify some of the signals that my body is telling me (e.g. If I'm about to faint or I've over exerted myself)</b><br>",
    "<b>It is difficult for me to describe what it feels like to be hungry, thirsty, hot, cold or in pain</b><br>",
    "<b>I am confused about my bodily sensations</b><br>",
    "<b>I have difficulty locating injury in my body</b><br>",
    "<b>Sometimes, when my body signals a problem, I have difficulty working out what the problem might be</b><br>",
    "<b>I don't tend to notice feelings in my body until they're very intense</b><br>",
    "<b>I find it difficult to put my internal bodily sensations into words</b><br>"
]

//Body Awareness Questionnaire
var BAQ = [
    "<b>I notice differences in the way my body reacts to various foods</b><br>",
    "<b>I can always tell when I bump myself whether or not it will become a bruise</b><br>",
    "<b>I always know when I've exerted myself to the point where I'll be sore the next day</b><br>",
    "<b>I am always aware of changes in my energy level when I eat certain foods</b><br>",
    "<b>I know in advance when I'm getting the flu</b><br>",
    "<b>I know I'm running a fever without taking my temperature</b><br>",
    "<b>I can distinguish between tiredness because of hunger and tiredness because of lack of sleep</b><br>",
    "<b>I can accurately predict what time of day lack of sleep will catch up with me</b><br>",
    "<b>I am aware of a cycle in my activity level throughout the day</b><br>",
    "<b>I don't notice seasonal rhythms and cycles in the way my body functions</b><br>",
    "<b>As soon as I wake up in the morning, I know how much energy I'll have during the day</b><br>",
    "<b>I can tell when I go to bed how well I will sleep that night</b><br>",
    "<b>I notice distinct body reactions when I am fatigued</b><br>",
    "<b>I notice specific body responses to changes in the weather</b><br>",
    "<b>I can predict how much sleep I will need at night in order to wake up refreshed</b><br>",
    "<b>When my exercise habits change, I can predict very accurately how that will affect my energy level</b><br>",
    "<b>There seems to be a 'best' time for me to go to sleep at night</b><br>",
    "<b>I notice specific bodily reactions to being overhungry</b><br>"
]

var BAQ_dim =[
    "BAQ_1",
    "BAQ_2",
    "BAQ_3",
    "BAQ_4",
    "BAQ_5",
    "BAQ_6",
    "BAQ_7",
    "BAQ_8",
    "BAQ_9",
    "BAQ_10_R",
    "BAQ_11",
    "BAQ_12",
    "BAQ_13",
    "BAQ_14",
    "BAQ_15",
    "BAQ_16",
    "BAQ_17",
    "BAQ_18"
]

// Interoceptive Accuracy Scale
var IAS = [
    "<b>I can always accurately perceive when my heart is beating fast</b><br>",
    "<b>I can always accurately perceive when I am hungry</b><br>",
    "<b>I can always accurately perceive when I am breathing fast</b><br>",
    "<b>I can always accurately perceive when I am thirsty</b><br>",
    "<b>I can always accurately perceive when I need to urinate</b><br>",
    "<b>I can always accurately perceive when I need to defecate</b><br>",
    "<b>I can always accurately perceive when I encounter different tastes</b><br>",
    "<b>I can always accurately perceive when I am going to vomit</b><br>",
    "<b>I can always accurately perceive when I am going to sneeze</b><br>",
    "<b>I can always accurately perceive when I am going to cough</b><br>",
    "<b>I can always accurately perceive when I am hot/cold</b><br>",
    "<b>I can always accurately perceive when I am sexually aroused</b><br>",
    "<b>I can always accurately perceive when I am going to pass wind</b><br>",
    "<b>I can always accurately perceive when I am going to burp</b><br>",
    "<b>I can always accurately perceive when my muscles are tired/sore</b><br>",
    "<b>I can always accurately perceive when I am going to get a bruise</b><br>",
    "<b>I can always accurately perceive when I am in pain</b><br>",
    "<b>I can always accurately perceive when my blood sugar is low</b><br>",
    "<b>I can always accurately perceive when someone is touching me affectionately rather than non-affectionately</b><br>",
    "<b>I can always accurately perceive when something is going to be ticklish</b><br>",
    "<b>I can always accurately perceive when something is going to be itchy</b><br>",

]