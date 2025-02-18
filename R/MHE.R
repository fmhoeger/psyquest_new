#' MHE
#'
#' This function defines a MHE module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MHE in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MHE,
#' consider using \code{\link{MHE_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{MHE}()}.
#' @export
MHE <- function(label = "MHE",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "MHE"

  elts <- main_test_mhe(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 1,
    arrange_vertically = TRUE,
    dict = dict
  )
}

main_test_mhe <- function(questionnaire_id,
                          label,
                          items,
                          offset = 1,
                          arrange_vertically = TRUE,
                          dict) {

  elts <- psychTestR::join(psychTestR::new_timeline(c(
    checkbox_page("q1",
              psychTestR::i18n("TMHE_0001_PROMPT"),
              c("choice1", "choice2"),
              subprompt = psychTestR::i18n("TMHE_0002_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0002_CHOICE1"), psychTestR::i18n("TMHE_0002_CHOICE2")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER"))
    ),
    dict = dict
  ))
  elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
    checkbox_page("q2",
              psychTestR::i18n("TMHE_0001_PROMPT"),
              c("choice1", "choice2"),
              subprompt = psychTestR::i18n("TMHE_0003_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0003_CHOICE1"), psychTestR::i18n("TMHE_0003_CHOICE2")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER"))
    ),
    dict = dict
  ))
  elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
    radiobutton_NAFC_page("q3",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              c("choice1", "choice2", "choice3", "choice4"),
              subprompt = psychTestR::i18n("TMHE_0005_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0005_CHOICE1"),
                         psychTestR::i18n("TMHE_0005_CHOICE2"),
                         psychTestR::i18n("TMHE_0005_CHOICE3"),
                         psychTestR::i18n("TMHE_0005_CHOICE4")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = dict
  ))
  elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
    radiobutton_NAFC_page("q4",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              c("choice1", "choice2", "choice3", "choice4"),
              subprompt = psychTestR::i18n("TMHE_0006_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0006_CHOICE1"),
                         psychTestR::i18n("TMHE_0006_CHOICE2"),
                         psychTestR::i18n("TMHE_0006_CHOICE3"),
                         psychTestR::i18n("TMHE_0006_CHOICE4")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = dict
  ))
  elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
    radiobutton_NAFC_page("q5",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              c("choice1", "choice2", "choice3", "choice4"),
              subprompt = psychTestR::i18n("TMHE_0007_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0007_CHOICE1"),
                         psychTestR::i18n("TMHE_0007_CHOICE2"),
                         psychTestR::i18n("TMHE_0007_CHOICE3"),
                         psychTestR::i18n("TMHE_0007_CHOICE4")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = dict
  ))
  elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
    radiobutton_NAFC_page("q6",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              c("choice1", "choice2", "choice3", "choice4"),
              subprompt = psychTestR::i18n("TMHE_0008_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0008_CHOICE1"),
                         psychTestR::i18n("TMHE_0008_CHOICE2"),
                         psychTestR::i18n("TMHE_0008_CHOICE3"),
                         psychTestR::i18n("TMHE_0008_CHOICE4")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = dict
  ))
  elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
    radiobutton_NAFC_page("q7",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              c("choice1", "choice2", "choice3", "choice4"),
              subprompt = psychTestR::i18n("TMHE_0009_PROMPT"),
              labels = c(psychTestR::i18n("TMHE_0009_CHOICE1"),
                         psychTestR::i18n("TMHE_0009_CHOICE2"),
                         psychTestR::i18n("TMHE_0009_CHOICE3"),
                         psychTestR::i18n("TMHE_0009_CHOICE4")),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = dict
  ))

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items),
                   psychTestR::end_module())
}

postprocess_mhe <- function(questionnaire_id, values) {
  #browser()

  # mother_count <- if (is.na(values[1]) || values[1] == 0) { 0 } else { nchar(toString(values[1])) }
  # father_count <- if (is.na(values[2]) || values[2] == 0) { 0 } else { nchar(toString(values[2])) }
  mother_count <- if (is.na(values[1]) || values[1] == 0) { 0 } else { values[1] }
  father_count <- if (is.na(values[2]) || values[2] == 0) { 0 } else { values[2] }

  sum_parents <- mother_count + father_count
  scoring_map <- psyquest::scoring_maps[[questionnaire_id]]
  raws <- list()
  raws[["ability"]] <- scoring_map[scoring_map$score == sum_parents, ]$raw
  raws[["encourage"]] <- values[6]
  raws[["support"]] <- values[7]

  score_stats <- data.frame(id     = c("ability", "encourage", "support"),
                            mean   = c(0.1143363, 3.156951, 2.769058),
                            sd     = c(0.5637453, 1.116791, 1.258868),
                            weight = c(0.57,      0.87,     0.88))

  value <- 0
  for (var in names(raws)) {
    weight <- score_stats[score_stats$id == var, "weight"][1]
    mean <- score_stats[score_stats$id == var, "mean"][1]
    sd <- score_stats[score_stats$id == var, "sd"][1]
    score <- weight * (raws[[var]] - mean) / sd
    value <- value + score
  }
  value
}
