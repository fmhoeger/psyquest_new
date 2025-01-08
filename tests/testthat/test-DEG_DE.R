context("DEG")
library(psychTestR)

app <- AppTester$new("apps/DEG_DE")

app$expect_ui_text("Bitte gib Deine ID ein Weiter")
app$set_inputs(p_id = "abcde")
app$click_next()

app$expect_ui_text("Wir möchten gerne möglichst präzise Informationen von dir bekommen. Wirst du bei dem nachfolgenden Test und den Fragen dein Bestes geben? Ja Nein")
app$click("btn1_text")

app$expect_ui_text("Hast du Probleme oder Schwierigkeiten mit dem Hören (z. B. krankheitsbedingt)? Nein Ja")
app$click("btn1_text")

app$expect_ui_text("Wenn ja, welche? Weiter")
app$setInputs(text_input = "Tinnitus")
app$click_next()

app$expect_ui_text("Ich bin weiblich männlich divers möchte ich nicht sagen")
app$click("btn1_text")

app$expect_ui_text("Was ist deine Nationalität/Staatsangehörigkeit? (In welchem Land hast du deinen Pass?) Deutsch Afghanisch Algerisch Amerikanisch Britisch Bulgarisch Chinesisch Französisch Griechisch Irakisch Iranisch Italienisch Kanadisch Kosovarisch Kroatisch Polnisch Portugiesisch Rumänisch Russisch Senegalesisch Serbisch Spanisch Syrisch Türkisch Weißrussisch Andere Nationalität Weiter")
app$click_next()

app$expect_ui_text("In welchem Land hast du die meiste Zeit deines Lebens verbracht? Deutschland Afghanistan Algerien Bulgarien China Frankreich Griechenland Großbritannien Irak Iran Italien Kanada Kosovo Kroatien Polen Portugal Rumänien Russland Senegal Serbien Spanien Syrien Türkei USA Weißrussland Anderes Land Weiter")
app$setInputs(dropdown = "TR")
app$click_next()

app$expect_ui_text("Was ist deine Muttersprache? (Welche Sprache sprichst du zu Hause?) Deutsch Albanisch Arabisch Bulgarisch Chinesisch Englisch Französisch Griechisch Italienisch Japanisch Kurdisch Paschtunisch Persisch Polnisch Rumänisch Russisch Serbisch Spanisch Türkisch Ungarisch Andere Sprache Weiter")
app$setInputs(dropdown = "tr")
app$click_next()

app$expect_ui_text("Sprichst du noch eine andere Sprache in deinem Alltag? Keine Deutsch Albanisch Arabisch Bulgarisch Chinesisch Englisch Französisch Griechisch Italienisch Japanisch Kurdisch Paschtunisch Persisch Polnisch Rumänisch Russisch Serbisch Spanisch Türkisch Ungarisch Andere Sprache Weiter")
app$setInputs(dropdown = "de")
app$click_next()

app$expect_ui_text("In welchem Monat und Jahr wurdest du geboren? Monat Bitte Monat auswählen! Januar Februar März April Mai Juni Juli August September Oktober November Dezember Jahr Bitte Jahr auswählen! 2013 2012 2011 2010 2009 2008 2007 2006 2005 2004 2003 2002 2001 2000 1999 1998 1997 1996 1995 1994 1993 1992 1991 1990 1989 1988 1987 1986 1985 1984 1983 1982 1981 1980 1979 1978 1977 1976 1975 1974 1973 1972 1971 1970 1969 1968 1967 1966 1965 1964 1963 1962 1961 1960 1959 1958 1957 1956 1955 1954 1953 1952 1951 1950 1949 1948 1947 1946 1945 1944 1943 1942 1941 1940 1939 1938 1937 1936 1935 1934 1933 1932 1931 1930 Weiter")
app$setInputs(month = "2")
app$setInputs(year = "1999")
app$click_next()

app$expect_ui_text("Bist Du eher rechtshändig linkshändig beidhändig")
app$click("btn1_text")

app$expect_ui_text("Mit welcher Hand schreibst Du am besten? mit der rechten mit der linken mit beiden")
app$click("btn2_text")

app$expect_ui_text("Deine Ergebnisse wurden gespeichert. Du kannst das Browserfenster jetzt schließen.")

results <- app$get_results() %>% as.list()

expect_equal(names(results), c("DEG"))
expect_equal(
  results[["DEG"]],
  list(
    q1 = "btn1_text",
    q2 = "btn1_text",
    q3 = "Tinnitus",
    q4 = "btn1_text",
    q5 = "DE",
    q6 = "TR",
    q7 = "tr",
    q8 = "de",
    q9 = c("2", "1999"),
    q10 = "btn1_text",
    q11 = "btn2_text",
    'Best Shot' = 1,
    'Hearing Impairment' = 1,
    'Type of Hearing Impairment' = "Tinnitus",
    Gender = 1,
    Age = 263,
    Nationality = "DE",
    'Country Formative Years' = "TR",
    'First Language' = "tr",
    'Second Language' = "de",
    Handedness = c(1, 2)
  )
)

app$stop()
