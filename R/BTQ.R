#' BTQ
#'
#' This function defines a BTQ module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the BTQ in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the BTQ,
#' consider using \code{\link{BTQ_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{BTQ}()}.
#' @export
BTQ <- function(label = "BTQ",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "BTQ"

  elts <- main_test_btq(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 0,
    arrange_vertically = TRUE,
    dict = dict
  )
}
choices_from_key <- function(keys){
  map_chr(keys, function(k) psyquest_dict$translate(k, language = "en"))
}
main_test_btq <- function(questionnaire_id, label, items, offset = 1, arrange_vertically = TRUE, dict) {

  elts <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::checkbox_page("q1",
                                prompt = psychTestR::i18n("TBTQ_0001_PROMPT"),
                                choices = choices_from_key(sprintf("TBTQ_0001_CHOICE%d", 1:14)),
                                labels = map(sprintf("TBTQ_0002_CHOICE%d", 1:14), psychTestR::i18n),
                                trigger_button_text = psychTestR::i18n("CONTINUE"),
                                force_answer = F,
                                failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")
      ),
      dict = dict
    ),
    psychTestR::new_timeline(
      psychTestR::radiobutton_NAFC_page("q2",
                                        prompt = psychTestR::i18n("TBTQ_0002_PROMPT"),
                                        choices = choices_from_key(sprintf("TBTQ_0002_CHOICE%d", 1:14)),
                                        labels = map(sprintf("TBTQ_0002_CHOICE%d", 1:14), psychTestR::i18n),
                                        trigger_button_text = psychTestR::i18n("CONTINUE"),
                                        failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")
      ),
      dict = psyquest::psyquest_dict
    ),
    psychTestR::new_timeline(
      psychTestR::NAFC_page("q3",
                            prompt = psychTestR::i18n("TBTQ_0003_PROMPT"),
                            choices = choices_from_key(sprintf("TBTQ_0003_CHOICE%d", 1:2)),
                            labels = c(psychTestR::i18n("TBTQ_0003_CHOICE1"),
                                       psychTestR::i18n("TBTQ_0003_CHOICE2")),
                            on_complete = function(answer, state, ...) {
                              psychTestR::set_local("lullaby_singing", answer, state)
                            }),
      dict = psyquest::psyquest_dict
    ),
    psychTestR::conditional(
      test = function(state, ...) {
        psychTestR::get_local("lullaby_singing", state) == "Yes"
      },
      logic =  psychTestR::new_timeline(
        psychTestR::checkbox_page("q4",
                                  prompt = psychTestR::i18n("TBTQ_0004_PROMPT"),
                                  choices = choices_from_key(sprintf("TBTQ_0004_CHOICE%d", 1:15)),
                                  labels = map(sprintf("TBTQ_0004_CHOICE%d", 1:15), psychTestR::i18n),
                                  trigger_button_text = psychTestR::i18n("CONTINUE"),
                                  force_answer = F,
                                  failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
        dict = psyquest::psyquest_dict
      )
    ),
    psychTestR::new_timeline(
      labelled_text_input_page("q5",
                             prompt = psychTestR::i18n("TBTQ_0005_PROMPT"),
                             one_line = F,
                             save_answer = T,
                             button_text = psychTestR::i18n("CONTINUE"),
                             width = "300px",
                             height = "200px"),
    dict = psyquest::psyquest_dict),
    psychTestR::new_timeline(
      psychTestR::NAFC_page("q6",
                            prompt = psychTestR::i18n("TBTQ_0006_PROMPT"),
                            choices = choices_from_key(sprintf("TBTQ_0006_CHOICE%d", 1:2)),
                            labels = c(psychTestR::i18n("TBTQ_0006_CHOICE1"),
                                       psychTestR::i18n("TBTQ_0006_CHOICE2"))),
      dict = psyquest::psyquest_dict
    )
  )

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items),
                   psychTestR::end_module())
}
postprocess_btq <- function(questionnaire_id, subscale, results) {
  value <- results[[questionnaire_id]][[subscale]] %>% paste(collapse = ";")
  value
}
