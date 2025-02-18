#' MUS(MUSIC)
#'
#' This function defines a MUS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the Musical Preferences: Five factor
#' model in a battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MUS,
#' consider using \code{\link{MUS_standalone}()}.
#'
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#'
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' There is a set of posible subscales (MUSIC): \code{"Mellow"}, \code{"Unpretentious"}, \code{"Sophisticated"},
#' \code{"Intense"}, and \code{"Contemporary"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{MUS}()}.
#'
#' @export

MUS <- function(label = "MUS",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  audio_dir <- "https://media.gold-msi.org/test_materials/MUSIC/"
  audio_dir <- gsub("/$", "", audio_dir)
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "MUS"

  main_test_mus(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    audio_dir = audio_dir,
    subscales = subscales,
    dict = dict
  )
}

main_test_mus <- function(questionnaire_id,
                          label,
                          items,
                          subscales,
                          audio_dir,
                          dict) {
  elts <- c()
  audio <- c()
  get_audio <- function(label) {
    audio_file <- NULL
    audio <- psyquest::psyquest_item_bank %>%
      filter(stringr::str_detect(audio_file, "mp3"))
  }
  audio <- get_audio("MUS")
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

    item_page <- psychTestR::new_timeline(
      psychTestR::audio_NAFC_page(
        label = question_label,
        prompt = get_prompt(
          counter,
          length(question_numbers),
          sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter])
        ),
        url = file.path(audio_dir, audio[counter, ]$audio_file),
        choices = choices,
        button_style = "min-width:250px",
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
