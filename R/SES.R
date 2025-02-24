#' SES
#'
#' This function defines a SES module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SES in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SES,
#' consider using \code{\link{SES_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Educational Degree"}, and \code{"ESeC"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{SES}()}.
#' @export
SES <- function(label = "SES",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "SES"

  elts <- main_test_ses(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    offset = 1,
    arrange_vertically = TRUE,
    dict = dict
  )
}

main_test_ses <- function(questionnaire_id,
                          label,
                          items,
                          subscales = c(),
                          offset = 1,
                          arrange_vertically = TRUE,
                          dict) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- c()

  if ("TSES_0001" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q1",
        "",
        c("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"),
        subprompt = psychTestR::i18n("TSES_0001_PROMPT"),
        labels = c(psychTestR::i18n("TSES_0001_CHOICE1"),
                   psychTestR::i18n("TSES_0001_CHOICE2"),
                   psychTestR::i18n("TSES_0001_CHOICE3"),
                   psychTestR::i18n("TSES_0001_CHOICE4"),
                   psychTestR::i18n("TSES_0001_CHOICE5"),
                   psychTestR::i18n("TSES_0001_CHOICE6"),
                   psychTestR::i18n("TSES_0001_CHOICE7")),
        trigger_button_text = psychTestR::i18n("CONTINUE"),
        failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = dict
    ))
  }

  if ("TSES_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q2",
        "",
        c("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"),
        subprompt = psychTestR::i18n("TSES_0002_PROMPT"),
        labels = c(psychTestR::i18n("TSES_0002_CHOICE1"),
                   psychTestR::i18n("TSES_0002_CHOICE2"),
                   psychTestR::i18n("TSES_0002_CHOICE3"),
                   psychTestR::i18n("TSES_0002_CHOICE4"),
                   psychTestR::i18n("TSES_0002_CHOICE5"),
                   psychTestR::i18n("TSES_0002_CHOICE6"),
                   psychTestR::i18n("TSES_0002_CHOICE7")),
        trigger_button_text = psychTestR::i18n("CONTINUE"),
        failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = dict
    ))
  }

  if ("TSES_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q3",
        shiny::p(psychTestR::i18n("TSES_0003_PROMPT"),style = "width:50%;text-align:justify"),
        c("choice1", "choice2", "choice3"),
        subprompt = psychTestR::i18n("TSES_0004_PROMPT"),
        labels = c(psychTestR::i18n("TSES_0004_CHOICE1"),
                   psychTestR::i18n("TSES_0004_CHOICE2"),
                   psychTestR::i18n("TSES_0004_CHOICE3")),
        trigger_button_text = psychTestR::i18n("CONTINUE"),
        failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"),
        on_complete = function(answer, state, ...) {
                   set_local("branch", answer, state)
        }
      )),
      dict = dict
    ))
  }

  if ("TSES_0005" %in% prompt_ids || "TSES_0006" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      conditional(function(state, ...) get_local("branch", state) == "choice1",
          radiobutton_NAFC_page("q5",
            "",
            c("choice1", "choice2"),
            subprompt = psychTestR::i18n("TSES_0006_PROMPT"),
            labels = c(psychTestR::i18n("TSES_0006_CHOICE1"),
                       psychTestR::i18n("TSES_0006_CHOICE2")),
            trigger_button_text = psychTestR::i18n("CONTINUE"),
            failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      conditional(function(state, ...) get_local("branch", state) == "choice2",
          radiobutton_NAFC_page("q4",
            "",
            c("choice1", "choice2"),
            subprompt = psychTestR::i18n("TSES_0005_PROMPT"),
            labels = c(psychTestR::i18n("TSES_0005_CHOICE1"),
                       psychTestR::i18n("TSES_0005_CHOICE2")),
            trigger_button_text = psychTestR::i18n("CONTINUE"),
            failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      )),
      dict = dict
    ))
  }

  if ("TSES_0007" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q6",
        "",
        c("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7", "choice8"),
        subprompt = psychTestR::i18n("TSES_0007_PROMPT"),
        labels = c(psychTestR::i18n("TSES_0007_CHOICE1"),
                   psychTestR::i18n("TSES_0007_CHOICE2"),
                   psychTestR::i18n("TSES_0007_CHOICE3"),
                   psychTestR::i18n("TSES_0007_CHOICE4"),
                   psychTestR::i18n("TSES_0007_CHOICE5"),
                   psychTestR::i18n("TSES_0007_CHOICE6"),
                   psychTestR::i18n("TSES_0007_CHOICE7"),
                   psychTestR::i18n("TSES_0007_CHOICE8")),
        trigger_button_text = psychTestR::i18n("CONTINUE"),
        failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = dict
    ))
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales),
                   psychTestR::end_module())
}

postprocess_ses <- function(subscale, results, scores) {
  sum_score <- 0
  if (subscale == "educational_degree") {
    scores <- scores - 1
    if (all(scores == 0)) { return(NA) }
    mean(scores[scores != 0], na.rm = TRUE)
  } else if (subscale == "class") {
    raw_scores <- c()
    if (scores[1] == 1) {
      raw_scores <- c(scores[1], NA, scores[2], scores[3])
    } else if (scores[1] == 2) {
      raw_scores <- c(scores[1], scores[2], NA, scores[3])
    } else if (scores[1] == 3) {
      raw_scores <- c(scores[1], 2, NA, scores[2])
    }

    get_ses_class(raw_scores[1], raw_scores[2], raw_scores[3], raw_scores[4])
  }
}

get_ses_class <- function(q1, q2, q3, q4) {
  if (all(is.na(c(q1, q2, q3, q4)))) {
    return(NA)
  }
  code <- get_ses_code(q1, q2, q3, q4)
  if (is.na(code)) {
    print("Code not defined!")
    return(NA)
  }
  if (code %in% c(1, 4, 5)) {
    return(1)
  }
  if (code %in% c(2, 3)) {
    if (q4 %in% c(1, 8)) {
      return(1)
    }
    return(3)
  }
  if (code == 6) {
    if (q4 %in% c(4:6)) {
      return(4)
    }
    return(1)
  }
  # code 7
  if (q4 %in% c(1, 3, 7, 8))
    return(1)
  if (q4 == 2)
    return(2)
  if (q4 == 4)
    return(4)
  if (q4 %in% 5:6)
    return(5)

  return(NA)
}

get_ses_code <- function(q1, q2, q3, q4) {
  if (any(is.na(c(q1, q4)))) {
    return(NA)
  }
  if (q1 == 3) {
    return(3)
  }
  if (q1 == 2) {
    return(ifelse(q2 == 1, 2, 1))
  }
  if (q1 == 1) {
    if (q4 == 3) {
      return(5)
    }
    else{
      return(ifelse(q3 == 1, 6, 7))
    }
  }
  return(NA)
}
