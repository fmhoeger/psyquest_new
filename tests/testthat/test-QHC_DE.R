context("QHC")
library(psychTestR)

app <- AppTester$new("apps/QHC_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Bitte gib an, inwieweit du den folgenden Aussagen zu deinem Hörvermögen zustimmst. Weiter")
app$click_next()

app$expect_ui_text("Ich habe Probleme beim Verstehen von Telefonaten.")
app$click("btn1_text")

app$expect_ui_text("Ich habe Schwierigkeiten, einer Unterhaltung zu folgen, wenn zwei oder mehr Personen gleichzeitig reden.")
app$click("btn2_text")

app$expect_ui_text("Ich habe Schwierigkeiten, Dinge im Fernsehen zu verstehen.")
app$click("btn3_text")

app$expect_ui_text("Ich muss mich anstrengen, um Gesprächen folgen zu können.")
app$click("btn4_text")

app$expect_ui_text("Es macht mir Sorgen, das Klingeln des Telefons oder der Türklingel zu verpassen.")
app$click("btn5_text")

app$expect_ui_text("Ich habe Schwierigkeiten, Gesprächen bei Hintergrundgeräuschen, wie in einem vollen Raum oder Restaurant, folgen zu können.")
app$click("btn1_text")

app$expect_ui_text("Ich bin mir unsicher, aus welcher Richtung Geräusche kommen.")
app$click("btn2_text")

app$expect_ui_text("Ich verstehe einige Wörter in einem Satz falsch und muss Leute um Wiederholung bitten.")
app$click("btn3_text")

app$expect_ui_text("Es fällt mir besonders schwer, die Sprache von Frauen und Kindern zu verstehen.")
app$click("btn4_text")

app$expect_ui_text("Ich habe Schwierigkeiten, Sprecher in einem großen Raum wie in einem Vortragsraum oder einer Kirche, zu verstehen.")
app$click("btn5_text")

app$expect_ui_text("Viele Leute, mit denen ich rede, scheinen zu nuscheln (oder sprechen nicht klar).")
app$click("btn1_text")

app$expect_ui_text("Leute sind verärgert, weil ich sie falsch verstanden habe.")
app$click("btn2_text")

app$expect_ui_text("Ich verstehe falsch, was andere sagen, und gebe unpassende Antworten.")
app$click("btn3_text")

app$expect_ui_text("Ich vermeide soziale Aktivitäten, weil ich nicht gut höre und befürchte, dass ich unpassend antworte.")
app$click("btn4_text")

app$expect_ui_text("Familienmitglieder und Freunde haben mir gesagt, dass sie glauben, mein Hörvermögen könnte vermindert sein.")
app$click("btn5_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

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
