#' CHD
#'
#' This function defines a CHD module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CHD in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the CHD, consider using \code{\link{CHD_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) (i18n_dict) (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are "Best Shot", "Hearing Impairment", "Type of Hearing Impairment", "Gender", "Age", "Nationality", "Country Formative Years", "First Language", "Second Language", and "Handedness".
#' If no subscales are provided all subscales are selected.
#' @param language Language the questionnaire is rendered in.
#' @param year_range  (length 2 int vector) The minimum and maximum year for birth date dropdown boxes.
#' @param ... Further arguments to be passed to \code{\link{CHD}()} (e.g., can be used to set choice of country of residence.)
#' @export
CHD <- function(label = "CHD",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                language = "en",
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "CHD"
  if(is.null(subscales) || length(subscales) == 0){
    subscales <- get_subscales("CHD")
  }
  dots <- list(...)
  main_test_chd(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    language = language,
    offset = 2,
    arrange_vertically = TRUE
  )
}

main_test_chd <- function(questionnaire_id,
                          label,
                          items,
                          subscales,
                          language,
                          offset = 1,
                          arrange_vertically = TRUE) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- psychTestR::new_timeline(
      psychTestR::one_button_page(
      body = shiny::div(
        psychTestR::i18n("TCHD_0001_PROMPT"),
        style = "margin-left:20%;margin-right:20%;text-align:justify;margin-bottom:2em"),
      button_text = psychTestR::i18n("CONTINUE")
    ),
    dict = psyquest::psyquest_dict)

  if ("TCHD_0002" %in% prompt_ids) {
    child_ages <- as.character(as.integer(floor(12*c(0, .25, .5, .75, 1:10))))
    child_ages[length(child_ages)] <- "120+"

    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q1",
                    psychTestR::i18n("TCHD_0002_PROMPT"),
                    setNames(child_ages, map(sprintf("TCHD_0002_CHOICE%d", 1:14), psychTestR::i18n)),
                    next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
    ))
  }

  if ("TCHD_0003" %in% prompt_ids) {
    languages <- languages_def[["en1"]]
    if (language[1] == "de" || language[1] == "de_f") {
      languages <- languages_def[["de1"]]
    }
    if (language[1] == "it") {
      languages <- languages_def[["it1"]]
    }
    if (language[1] == "es") {
      languages <- languages_def[["es1"]]
    }
    if (language[1] == "lv") {
      languages <- languages_def[["lv1"]]
    }
    language_codes <- language_codes_def[languages]
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q2",
                    psychTestR::i18n("TCHD_0003_PROMPT"),
                    setNames(language_codes, map(languages, psychTestR::i18n)),
                    next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
    ))
  }

  if ("TCHD_0004" %in% prompt_ids) {
    num_kids <- set_names(as.character(0:10), c(as.character(0:9), "10+"))
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q3",
                    psychTestR::i18n("TCHD_0004_PROMPT"),
                    num_kids,
                    next_button_text = psychTestR::i18n("CONTINUE"),
                    max_width_pixels = 100)
    ),
    dict = psyquest::psyquest_dict
    ))
  }


  if ("TCHD_0005" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q4",
                psychTestR::i18n("TCHD_0005_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = map(sprintf("TCHD_0005_CHOICE%d", 1:4), psychTestR::i18n),
                button_style = "min-width: 188px"
                )
      ),
      dict = psyquest::psyquest_dict
    ))
  }


  if ("TCHD_0006" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q5",
                psychTestR::i18n("TCHD_0006_PROMPT"),
                sprintf("btn%d_text", 1:3),
                labels = map(sprintf("TCHD_0006_CHOICE%d", 1:3), psychTestR::i18n),
                button_style = "min-width: 300px;"
                )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TCHD_0007" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q6",
                psychTestR::i18n("TCHD_0007_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = map(sprintf("TCHD_0007_CHOICE%d", 1:4), psychTestR::i18n),
                button_style = "min-width: 600px;"
                )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TCHD_0008" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q7",
                psychTestR::i18n("TCHD_0008_PROMPT"),
                sprintf("btn%d_text", 1:3),
                labels = map(sprintf("TCHD_0008_CHOICE%d", 1:3), psychTestR::i18n),
                button_style = "min-width: 500px;"
      )
    ),
    dict = psyquest::psyquest_dict
    ))
  }

  if ("TCHD_0009" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q8",
                psychTestR::i18n("TCHD_0009_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = map(sprintf("TCHD_0009_CHOICE%d", 1:4), psychTestR::i18n),
                button_style = "min-width: 200px;"
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

postprocess_chd <- function(label, subscale, results, scores) {
  #browser()
  gender_labels <- c("female", "male", "diverse", "rather not say")
  if (subscale == "Children Age") {
    results[[label]][["q1"]]
  } else if (subscale == "Children Language") {
    results[[label]][["q2"]]
  } else if (subscale == "Household Children ") {
    results[[label]][["q3"]]
  } else if (subscale == "Children Gender") {
    gender_labels[as.numeric(gsub("[^0-9]", "", results[[label]][["q4"]]))]
  } else if (subscale == "Family Situation") {
    stringr::str_extract(results[[label]][["q5"]], "[0-9]+")
  } else if (subscale == "External Care") {
    stringr::str_extract(results[[label]][["q6"]], "[0-9]+")
  } else if (subscale == "Caretaker") {
    stringr::str_extract(results[[label]][["q7"]], "[0-9]+")
  } else if (subscale == "Second Parent") {
    stringr::str_extract(results[[label]][["q8"]], "[0-9]+")
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
