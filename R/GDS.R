#' GDS
#'
#' This function defines a GDS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the GDS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the GDS,
#' consider using \code{\link{GDS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) There are 4 subscales, one general subscale
#' and 6 items of 'dance experience observations' to be included in the questionnaire.
#' Possible subscales are \code{"Body Awareness"}, \code{"Social Dancing"},
#' \code{"Urge to Dance"}, \code{"Dance Training"}, \code{"General"},
#' and \code{"Observational Dance Experience"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{GDS}()}.
#' @export
GDS <- function(label = "GDS",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "GDS"

  main_test_gds(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales
                      ),
    subscales = subscales,
    dict = dict
  )
}

main_test_gds <- function(questionnaire_id, label, items, subscales, dict) {
  elts <- c()
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)

  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))

  for (counter in seq_along(numeric(length(question_numbers)))) {
    question_label <- sprintf("q%d", question_numbers[counter])
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", label, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", label, question_numbers[counter], 1:num_of_options)

    arrange_vertically <- TRUE
    if (question_numbers[counter] %in% c(18, 19, 20, 23)) {
      arrange_vertically <- FALSE
    }

    button_style <- "margin-bottom: 4px"
    min_width <- ''
    if (!question_numbers[counter] %in% c(18, 19, 20, 23)) {
      button_style <- paste(button_style, "min-width: 236px", sep="; ")
    } else {
      if (question_numbers[counter] %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                           11, 12, 13, 14, 15, 16, 17, 21, 22)) {
        min_width <- '46px'
      }

      button_style <- paste(button_style,
                            stringr::str_interp("min-width: ${min_width}"),
                            sep="; ")
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
