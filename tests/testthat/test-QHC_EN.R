context("QHC")
library(psychTestR)

app <- AppTester$new("apps/QHC_EN")

app$expect_ui_text("Please enter your ID Continue")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("With respect to your hearing, please indicate your (dis)agreement to the following statements.")
app$click_next()

app$expect_ui_text("I have a problem hearing over the telephone.")
app$click("btn1_text")

app$expect_ui_text("I have trouble following the conversation when two or more people are talking at the same time.")
app$click("btn2_text")

app$expect_ui_text("I have trouble understanding things on TV.")
app$click("btn3_text")

app$expect_ui_text("I have to strain to understand conversations.")
app$click("btn4_text")

app$expect_ui_text("I have to worry about missing a telephone ring or doorbell.")
app$click("btn5_text")

app$expect_ui_text("I have trouble hearing conversations in a noisy background such as a crowded room or restaurant.")
app$click("btn1_text")

app$expect_ui_text("I get confused about where sounds come from.")
app$click("btn2_text")

app$expect_ui_text("I misunderstand some words in a sentence and need to ask people to repeat themselves.")
app$click("btn3_text")

app$expect_ui_text("I especially have trouble understanding the speech of women and children.")
app$click("btn4_text")

app$expect_ui_text("I have trouble understanding the speaker in a large room such as at a meeting or place of worship.")
app$click("btn5_text")

app$expect_ui_text("Many people I talk to seem to mumble (or don't speak clearly).")
app$click("btn1_text")

app$expect_ui_text("People get annoyed because I misunderstand what they say.")
app$click("btn2_text")

app$expect_ui_text("I misunderstand what others are saying and make inappropriate responses.")
app$click("btn3_text")

app$expect_ui_text("I avoid social activities because I cannot hear well and fear I will reply improperly.")
app$click("btn4_text")

app$expect_ui_text("Family members and friends have told me they think I may have a hearing loss.")
app$click("btn5_text")

app$expect_ui_text("Your results have been saved. You can close the browser window now.")

results <- app$get_results() %>% as.list()
expect_equal(names(results), c("QHC"))
expect_equal(
  results[["QHC"]],
  list(
    q1 = "btn1_text",
    q2 = "btn2_text",
    q3 = "btn3_text",
    q4 = "btn4_text",
    q5 = "btn5_text",
    q6 = "btn1_text",
    q7 = "btn2_text",
    q8 = "btn3_text",
    q9 = "btn4_text",
    q10 = "btn5_text",
    q11 = "btn1_text",
    q12 = "btn2_text",
    q13 = "btn3_text",
    q14 = "btn4_text",
    q15 = "btn5_text",
    General = 30
  )
)

app$stop()
