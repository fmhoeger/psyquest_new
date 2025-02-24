#' GMS
#'
#' This function defines a GMS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the GMS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the GMS,
#' consider using \code{\link{GMS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Abilities"}, \code{"Absolute Pitch"}, \code{"Active Engagement"}, \code{"Emotions"}, \code{"General"}, \code{"Instrument"}, \code{"Musical Training"}, \code{"Perceptual Abilities"}, \code{"Singing Abilities"}, and \code{"Start Age"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' Overrides the \code{"short_version"} argument.
#' Overridden by the \code{configuration_filepath} argument.
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#' Overridden by the \code{configuration_filepath} and \code{"subscales"} arguments.
#' @param configuration_filepath (Character scalar) Optional path to a configuration file exported from the \href{https://shiny.gold-msi.org/gmsiconfigurator}{GMSI-Configurator}.
#' Overrides the \code{short_version} and \code{subscales} arguments.
#' @param ... Further arguments to be passed to \code{\link{GMS}()}.
#' @export
GMS <- function(label = "GMS",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                short_version = FALSE,
                configuration_filepath = NULL,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "GMS"

  main_test_gms(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales,
                      short_version = short_version,
                      configuration_filepath = configuration_filepath),
    subscales = subscales,
    dict = dict
  )
}

main_test_gms <- function(questionnaire_id, label, items, subscales, dict) {
  elts <- c()
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)

  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))

  for (counter in seq_along(numeric(length(question_numbers)))) {
    question_label <- sprintf("q%d", question_numbers[counter])
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", questionnaire_id, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", questionnaire_id, question_numbers[counter], 1:num_of_options)

    arrange_vertically <- TRUE
    if (question_numbers[counter] %in% c(2, 12, 17, 18, 21, 22, 31, 32, 40, 41)) {
      arrange_vertically <- FALSE
    }

    button_style <- "margin-bottom: 4px"
    min_width <- ''
    if (!question_numbers[counter] %in% c(2, 12, 17, 18, 21, 22, 31, 32, 40, 41)) {
      button_style <- paste(button_style, "min-width: 236px", sep="; ")
    } else {
      if (question_numbers[counter] %in% c(2, 12, 18, 21)) {
        min_width <- '46px'
      } else if (question_numbers[counter] %in% c(17)) {
        min_width <- '56px'
      } else if (question_numbers[counter] %in% c(22)) {
        min_width <- '38px'
      } else if (question_numbers[counter] %in% c(31)) {
        min_width <- '110px'
      } else if (question_numbers[counter] %in% c(40)) {
        min_width <- '44px'
      } else if (question_numbers[counter] %in% c(41)) {
        min_width <- '60px'
      }
      button_style <- paste(button_style, stringr::str_interp("min-width: ${min_width}"), sep="; ")
    }

    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = question_label,
        prompt = get_prompt(
          counter,
          length(question_numbers),
          sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter])
        ),
        choices = choices,
        arrange_vertically = arrange_vertically,
        button_style = button_style,
        labels = map(choice_ids, psychTestR::i18n)
      ),
      dict = dict
    )
    elts <- psychTestR::join(elts, item_page)
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales),
                   psychTestR::end_module())
}
