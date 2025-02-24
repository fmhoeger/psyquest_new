#' DEG
#'
#' This function defines a DEG module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the DEG in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the DEG, consider using \code{\link{DEG_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) (i18n_dict) (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are "Best Shot", "Hearing Impairment", "Type of Hearing Impairment", "Gender", "Age", "Nationality", "Country Formative Years", "First Language", "Second Language", and "Handedness".
#' If no subscales are provided all subscales are selected.
#' @param language Language the questionnaire is rendered in.
#' @param year_range  (length 2 int vector) The minimum and maximum year for birth date dropdown boxes.
#' @param ... Further arguments to be passed to \code{\link{DEG}()} (e.g., can be used to set choice of country of residence.)
#' @export
DEG <- function(label = "DEG",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                language = "en",
                year_range = c(1930, 2013),
                ...) {
  stopifnot(purrr::is_scalar_character(label), length(year_range) == 2)

  questionnaire_id <- "DEG"
  if(is.null(subscales) || length(subscales) == 0){
    subscales <- c("Best Shot",
                   "Hearing Impairment",
                   "Age",
                   "Gender",
                   "Nationality",
                   "Country Formative Years",
                   "First Language",
                   "Second Language",
                   "Handedness")
  }
  dots <- list(...)
  if("residence_countries" %in% names(dots)){
    residence_countries <- dots$residence_countries
    if(!is.null(names(residence_countries))){
      country_codes <- names(residence_countries)
    }
    else if ("country_codes" %in% names(dots)){
      country_codes <- dots$country_codes
    }
    else{
      names(residence_countries) <- residence_countries
    }
    stopifnot(
      length(unique(residence_countries)) == length(residence_countries),
      length(unique(names(residence_countries))) == length(names(residence_countries))
    )
  }
  else{
    residence_countries <- NULL
  }
  show_month <- TRUE
  if("show_month" %in% names(dots)){
    show_month <- dots$show_month
  }
  main_test_deg(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    min_year = year_range[1],
    max_year = year_range[2],
    subscales = subscales,
    language = language,
    offset = 1,
    arrange_vertically = TRUE,
    residence_countries = residence_countries,
    formative_countries = residence_countries,
    show_month = show_month,
    dict = dict
  )
}

main_test_deg <- function(questionnaire_id,
                          label,
                          items,
                          subscales,
                          language,
                          min_year = 1930, max_year = 2013,
                          offset = 1,
                          arrange_vertically = TRUE,
                          nationalities = NULL, formative_countries = NULL, residence_countries = NULL,
                          show_month = TRUE,
                          dict) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- c()
  if ("TDEG_0001" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
        NAFC_page("q1",
                psychTestR::i18n("TDEG_0001_PROMPT"),
                sprintf("btn%d_text", 1:2),
                labels = map(sprintf("TDEG_0001_CHOICE%d", 1:2), psychTestR::i18n),
                arrange_vertically = FALSE,
                button_style = "min-width: 60px"
        )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
        NAFC_page("q2",
                  psychTestR::i18n("TDEG_0002_PROMPT"),
                  sprintf("btn%d_text", c(2, 1)),
                  labels = map(sprintf("TDEG_0002_CHOICE%d", c(2, 1)), psychTestR::i18n),
                  arrange_vertically = FALSE,
                  button_style = "min-width: 60px",
                  on_complete = function(answer, state, ...) {
                    set_local("hearing_problems", answer, state)
                  }
        )
      ),
      dict = dict
    ))
  }

  if ("TDEG_0003" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      conditional(function(state, ...) get_local("hearing_problems", state) == "btn1_text",
        text_input_page("q3",
                        psychTestR::i18n("TDEG_0003_PROMPT"),
                        button_text = psychTestR::i18n("CONTINUE"))
        )),
      dict = dict
    ))
  }

  if ("TDEG_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q4",
                psychTestR::i18n("TDEG_0004_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = map(sprintf("TDEG_0004_CHOICE%d", 1:4), psychTestR::i18n),
                button_style = "min-width: 188px"
                )
      ),
      dict = dict
    ))
  }

  if ("TDEG_0006" %in% prompt_ids) {
    countries <- get_countries(nationalities, language, type = "nationalitites")
    #browser()
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q5",
                    psychTestR::i18n("TDEG_0006_PROMPT"),
                    setNames(names(countries), map(countries, psychTestR::i18n)),
                    next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = dict
    ))
  }

  if ("TDEG_0007" %in% prompt_ids) {
    formative_countries <- get_countries(formative_countries, language)
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q6",
                psychTestR::i18n("TDEG_0007_PROMPT"),
                setNames(names(formative_countries), map(formative_countries, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = dict
    ))
  }

  if ("TDEG_0008" %in% prompt_ids) {
    languages <- languages_def[["en1"]]

    if (language[1] == "de" || language[1] == "de_f") {
      languages <- languages_def[["de1"]]
    }

    if (language[1] == "it") {
      languages <- languages_def[["de1"]]
    }
    if (language[1] == "es") {
      languages <- languages_def[["es1"]]
    }
    if (language[1] == "lv") {
      languages <- languages_def[["lv1"]]
    }

    language_codes <- language_codes_def[languages]
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q7",
                psychTestR::i18n("TDEG_0008_PROMPT"),
                setNames(language_codes, map(languages, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = dict
    ))
  }

  if ("TDEG_0009" %in% prompt_ids) {
    languages <- languages_def[["en2"]]
    if (language[1] == "de" || language[1] == "de_f") {
      languages <- languages_def[["de2"]]
    }
    if (language[1] == "it") {
      languages <- languages_def[["it2"]]
    }
    if (language[1] == "es") {
      languages <- languages_def[["es2"]]
    }
    if (language[1] == "lv") {
      languages <- languages_def[["lv2"]]
    }
    language_codes <- language_codes_def[languages]
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q8",
                psychTestR::i18n("TDEG_0009_PROMPT"),
                setNames(language_codes, map(languages, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = dict
    ))
  }

  if ("TDEG_0010" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      month_and_year_select_page("q9",
                psychTestR::i18n("TDEG_0010_PROMPT"),
                min_year = min_year,
                max_year = max_year,
                show_month = show_month)
      ),
      dict = dict
    ))
  }

  if ("TDEG_0011" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q10",
                psychTestR::i18n("TDEG_0011_PROMPT"),
                sprintf("btn%d_text", 1:3),
                labels = map(sprintf("TDEG_0011_CHOICE%d", 1:3), psychTestR::i18n),
                button_style = "min-width: 300px;"
                )
      ),
      dict = dict
    ))
  }

  if ("TDEG_0012" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q11",
                psychTestR::i18n("TDEG_0012_PROMPT"),
                sprintf("btn%d_text", 1:3),
                labels = map(sprintf("TDEG_0012_CHOICE%d", 1:3), psychTestR::i18n),
                button_style = "min-width: 162px;"
                )
      ),
      dict = dict
    ))
  }

  if ("TDEG_0013" %in% prompt_ids) {
    residence_countries <- get_countries(residence_countries, language)
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q13",
                    psychTestR::i18n("TDEG_0013_PROMPT"),
                    setNames(names(residence_countries), map(residence_countries, psychTestR::i18n)),
                    next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = dict
    ))
  }
  if ("TDEG_0014" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q14",
                psychTestR::i18n("TDEG_0014_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = map(sprintf("TDEG_0014_CHOICE%d", 1:4), psychTestR::i18n),
                button_style = "min-width: 500px;"
      )
    ),
    dict = dict
    ))
  }
  if ("TDEG_0015" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q15",
                psychTestR::i18n("TDEG_0015_PROMPT"),
                sprintf("btn%d_text", c(1, 2)),
                labels = map(sprintf("TDEG_0015_CHOICE%d", c(1, 2)), psychTestR::i18n),
                arrange_vertically = FALSE,
                button_style = "min-width: 60px",
                on_complete = NULL
      )
    ),
    dict = dict
    ))
  }



  if ("TDEG_0018" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q18",
                psychTestR::i18n("TDEG_0018_PROMPT"),
                sprintf("btn%d_text", 1:6),
                labels = map(sprintf("TDEG_0018_CHOICE%d", 1:6), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 400px",
                on_complete = NULL
      )
    ),
    dict = dict
    ))
  }

  if ("TDEG_0019" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q19",
                psychTestR::i18n("TDEG_0019_PROMPT"),
                sprintf("btn%d_text", 1:7),
                labels = map(sprintf("TDEG_0019_CHOICE%d", 1:7), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 300px",
                on_complete = NULL
      )
    ),
    dict = dict
    ))
  }

  if ("TDEG_0020" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      # psychTestR::checkbox_page("q20",
      #                           prompt = psychTestR::i18n("TDEG_0020_PROMPT"),
      #                           choices = sprintf("btn%d_text", 1:6),
      #                           labels = map(sprintf("TDEG_0020_CHOICE%d", 1:6), psychTestR::i18n),
      #                           trigger_button_text = psychTestR::i18n("CONTINUE"),
      #                           force_answer = F,
      #                           failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")
      # ),

      NAFC_page("q20",
                psychTestR::i18n("TDEG_0020_PROMPT"),
                sprintf("btn%d_text", 1:6),
                labels = map(sprintf("TDEG_0020_CHOICE%d", 1:6), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 500px",
                on_complete = NULL),
    dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0021" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q21",
                psychTestR::i18n("TDEG_0021_PROMPT"),
                sprintf("btn%d_text", 1:6),
                labels = map(sprintf("TDEG_0021_CHOICE%d", 1:6), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 500px",
                on_complete = NULL
      )
    ),
    dict = psyquest::psyquest_dict
    ))
  }
  if ("TDEG_0016" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q16",
                psychTestR::i18n("TDEG_0016_PROMPT"),
                sprintf("btn%d_text", 1:7),
                labels = map(sprintf("TDEG_0016_CHOICE%d", 1:7), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 400px",
                on_complete = NULL
      )
    ),
    dict = dict
    ))
  }

  if ("TDEG_0017" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q17",
                psychTestR::i18n("TDEG_0017_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = map(sprintf("TDEG_0017_CHOICE%d", 1:4), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 300px",
                on_complete = NULL
      )
    ),
    dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0022" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q22",
                psychTestR::i18n("TDEG_0022_PROMPT"),
                sprintf("btn%d_text", 1:2),
                labels = map(sprintf("TDEG_0022_CHOICE%d", 1:2), psychTestR::i18n),
                arrange_vertically = FALSE,
                button_style = "min-width: 100px",
                on_complete = NULL
      )
    ),
    dict = psyquest::psyquest_dict
    ))
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales),
                   psychTestR::end_module())
}

postprocess_deg <- function(label, subscale, results, scores) {
  #browser()
  if (subscale == "Type of Hearing Impairment") {
    if (results[[label]][["q2"]] == "btn1_text") {
      results[[label]][["q3"]]
    } else {
      ""
    }
  } else if (subscale == "Age") {
    res <- results[[label]][["q9"]]
    cur_date <- Sys.Date()
    cur_year <- get_year(cur_date)
    cur_month <- get_month(cur_date) - 1

    if(length(res) == 2){
      month <- as.integer(res[1]) - 1
      year <- as.numeric(res[2])
      age <- (cur_year - year) * 12 + cur_month - month

    }
    else{
      year <- as.numeric(res[1])
      age <- (cur_year - year) * 12
    }
    age
  } else if (subscale == "Gender") {
    as.numeric(gsub("[^0-9]", "", results[[label]][["q4"]]))
  } else if (subscale == "Nationality") {
    results[[label]][["q5"]]
  } else if (subscale == "Country Formative Years") {
    results[[label]][["q6"]]
  } else if (subscale == "Country of Residence") {
    results[[label]][["q13"]]
  } else if (subscale == "First Language") {
    results[[label]][["q7"]]
  } else if (subscale == "Second Language") {
    results[[label]][["q8"]]
  } else if (subscale == "Qualification") {
    choices <- get_item_choices("DEG", 14)
    choices[as.integer(stringr::str_extract(results[[label]][["q14"]], "[0-9]+"))]
  } else if (subscale == "Employment") {
    c("Yes", "No")[as.integer(stringr::str_extract(results[[label]][["q15"]], "[0-9]+"))]
  } else if (subscale == "Employment Long") {
    choices <- get_item_choices("DEG", 19)
    choices[as.integer(stringr::str_extract(results[[label]][["q19"]], "[0-9]+"))]
  } else if (subscale == "Life Circumstances") {
    stringr::str_extract(results[[label]][["q16"]], "[0-9]+")
  } else if (subscale == "Financial") {
    stringr::str_extract(results[[label]][["q17"]], "[0-9]+")
  } else if (subscale == "School Degree") {
    choices <- get_item_choices("DEG", 20)
    choices[as.integer(stringr::str_extract(results[[label]][["q20"]], "[0-9]+"))]
  } else if (subscale == "Vocational Qualification") {
    choices <- get_item_choices("DEG", 21)
    choices[as.integer(stringr::str_extract(results[[label]][["q21"]], "[0-9]+"))]
  } else if (subscale == "Music Proficiency") {
    7 - as.integer(stringr::str_extract(results[[label]][["q18"]], "[0-9]+"))
  } else if (subscale == "Handedness") {
    c(as.numeric(gsub("[^0-9]", "", results[[label]][["q10"]])),
      as.numeric(gsub("[^0-9]", "", results[[label]][["q11"]])))
  } else if (subscale == "Born Here") {
    c("yes", "no")[as.integer(stringr::str_extract(results[[label]][["q22"]], "[0-9]+"))]
  } else {
    mean(scores)
  }
}

get_countries <- function(countries, language, type = "countries"){
  if(!is.null(countries)){
    return(countries)
  }
  countries <- c("UK", "USA", "BULGARIA", "CHINA", "CUBA", "DOMINICAN_REPUBLIC", "EL_SALVADOR", "FRANCE", "GERMANY", "GUATEMALA", "INDIA", "IRELAND", "ITALY", "LITHUANIA", "MEXICO", "NETHERLANDS", "NIGERIA", "PAKISTAN", "PHILIPPINES", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIAN_FEDERATION", "SOUTH_AFRICA", "SOUTH_KOREA", "SPAIN", "VIETNAM", "OTHER_COUNTRY")
  names(countries) <- c("UK", "US", "BG", "CN", "CU", "DO", "SV", "FR", "DE", "GT", "IN", "IE", "IT", "LT", "MX", "NL", "NG", "PK", "PH", "PL", "PT", "RO", "RU", "SA", "KR", "ES", "VN", "OTHER")

  if (language[1] == "de" || language[1] == "de_f") {
    countries <- c("GERMANY", "AFGHANISTAN", "ALGERIA", "BULGARIA", "CHINA", "FRANCE", "GREECE", "UK", "IRAQ", "IRAN", "ITALY", "CANADA", "KOSOVO", "CROATIA", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIAN_FEDERATION", "SENEGAL", "SERBIA", "SPAIN", "SYRIA", "TURKEY", "USA", "BELARUS", "OTHER_COUNTRY")
    names(countries) <-  c("DE", "AF", "DZ", "BG", "ZH", "FR", "GR", "GB", "IQ", "IR", "IT", "CA", "XK", "HR", "PL", "PT", "RO", "RU", "SN", "RS", "ES", "SY", "TR", "USA", "BY", "OTHER")
  }
  if (language[1] == "it") {
    countries <- c("ITALY", "AFGHANISTAN", "ALGERIA", "BULGARIA", "CHINA", "FRANCE", "GREECE", "UK", "IRAQ", "IRAN", "GERMANY", "CANADA", "KOSOVO", "CROATIA", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIAN_FEDERATION", "SENEGAL", "SERBIA", "SPAIN", "SYRIA", "TURKEY", "USA", "BELARUS", "OTHER_COUNTRY")
    names(countries) <-  c("IT", "AF", "DZ", "BG", "ZH", "FR", "GR", "GB", "IQ", "IR", "DE", "CA", "XK", "HR", "PL", "PT", "RO", "RU", "SN", "RS", "ES", "SY", "TR", "USA", "BY", "OTHER")
  }
  if (language[1] == "lv") {
    if(type == "countries"){
      countries <- c("LATVIA", "UKRAINE", "ESTONIA", "POLAND", "BELARUS", "RUSSIAN_FEDERATION", "BULGARIA",  "UK",  "GERMANY", "USA",  "OTHER_COUNTRY")
      names(countries) <- c("LV", "UK", "ET", "PL", "BE", "RU", "BU", "GB",  "DE",  "USA",  "OTHER")
    }
    else  {
      countries <- c("LATVIAN_NAT", "UKRAINIAN_NAT", "ESTONIAN_NAT", "POLISH_NAT", "BELORUSSIAN_NAT", "RUSSIAN_NAT", "BULGARIAN_NAT",  "ENGLISH_NAT",  "GERMAN_NAT", "AMERICAN_NAT",  "OTHER_NATIONALITY")
      names(countries) <- c("LV", "UK", "ET", "PL", "BE", "RU", "BU", "GB",  "DE",  "USA",  "OTHER")
    }
  }
  countries
}

# get_nationalities <- function(nationalities, language){
#   if(!is.null(nationalities)){
#     return(nationalities)
#   }
#   browser()
#   if (language[1] == "lv") {
#     nationalities <- c("LATVIAN_NAT", "UKRAINAN_NAT", "ESTONIAN_NAT", "POLISH_NAT", "BELORUSSIAN_NAT", "RUSSIAN_NAT", "BULGARIAN_NAT",  "UK_NAT",  "GERMANY_NAT", "USA_NAT",  "OTHER_NATIONALITY")
#     names(nationalities) <- c("LV", "UK", "ET", "PL", "BE", "RU", "BU", "GB",  "DE",  "USA",  "OTHER")
#   }
#   else{
#     nationalities <- get_countries(nationalities, language)
#   }
#   nationalities
#
# }
