#' CCM
#'
#' This function defines a CCM module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CCM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the CCM,
#' consider using \code{\link{CCM_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation .
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}, and \code{"Extra"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{CCM}()}.
#' @export
CCM <- function(label = "CCM",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "CCM"

  main_test_ccm(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    offset = 1,
    dict = dict
  )
}

main_test_ccm <- function(questionnaire_id, label, items, subscales, offset = 1, arrange_vertically = TRUE, dict) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- c()

  if ("TCCM_0001" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      checkbox_page("q1",
                psychTestR::i18n("TCCM_0001_PROMPT"),
                c("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7", "choice8", "choice9"),
                labels = c(psychTestR::i18n("TCCM_0001_CHOICE1"),
                           psychTestR::i18n("TCCM_0001_CHOICE2"),
                           psychTestR::i18n("TCCM_0001_CHOICE3"),
                           psychTestR::i18n("TCCM_0001_CHOICE4"),
                           psychTestR::i18n("TCCM_0001_CHOICE5"),
                           psychTestR::i18n("TCCM_0001_CHOICE6"),
                           psychTestR::i18n("TCCM_0001_CHOICE7"),
                           psychTestR::i18n("TCCM_0001_CHOICE8"),
                           psychTestR::i18n("TCCM_0001_CHOICE9")),
                force_answer = TRUE,
                javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
                trigger_button_text = psychTestR::i18n("CONTINUE"),
                failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER"))
      ),
      dict = dict
    ))
  }
  if ("TCCM_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q2",
                psychTestR::i18n("TCCM_0002_PROMPT"),
                c("choice1", "choice2", "choice3", "choice4", "choice5"),
                labels = c(psychTestR::i18n("TCCM_0002_CHOICE1"),
                           psychTestR::i18n("TCCM_0002_CHOICE2"),
                           psychTestR::i18n("TCCM_0002_CHOICE3"),
                           psychTestR::i18n("TCCM_0002_CHOICE4"),
                           psychTestR::i18n("TCCM_0002_CHOICE5")),
                trigger_button_text = psychTestR::i18n("CONTINUE"),
                failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = dict
    ))
  }

  if ("TCCM_0003" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q3",
                psychTestR::i18n("TCCM_0003_PROMPT"),
                c("choice1", "choice2", "choice3", "choice4", "choice5"),
                labels = c(psychTestR::i18n("TCCM_0003_CHOICE1"),
                           psychTestR::i18n("TCCM_0003_CHOICE2"),
                           psychTestR::i18n("TCCM_0003_CHOICE3"),
                           psychTestR::i18n("TCCM_0003_CHOICE4"),
                           psychTestR::i18n("TCCM_0003_CHOICE5")),
                trigger_button_text = psychTestR::i18n("CONTINUE"),
                failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = dict
    ))
  }

  if ("TCCM_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q4",
                psychTestR::i18n("TCCM_0004_PROMPT"),
                c("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"),
                labels = c(psychTestR::i18n("TCCM_0004_CHOICE1"),
                           psychTestR::i18n("TCCM_0004_CHOICE2"),
                           psychTestR::i18n("TCCM_0004_CHOICE3"),
                           psychTestR::i18n("TCCM_0004_CHOICE4"),
                           psychTestR::i18n("TCCM_0004_CHOICE5"),
                           psychTestR::i18n("TCCM_0004_CHOICE6"),
                           psychTestR::i18n("TCCM_0004_CHOICE7")),
                trigger_button_text = psychTestR::i18n("CONTINUE"),
                failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TCCM_0005" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      radiobutton_NAFC_page("q5",
                psychTestR::i18n("TCCM_0005_PROMPT"),
                c("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"),
                labels = c(psychTestR::i18n("TCCM_0005_CHOICE1"),
                     psychTestR::i18n("TCCM_0005_CHOICE2"),
                     psychTestR::i18n("TCCM_0005_CHOICE3"),
                     psychTestR::i18n("TCCM_0005_CHOICE4"),
                     psychTestR::i18n("TCCM_0005_CHOICE5"),
                     psychTestR::i18n("TCCM_0005_CHOICE6"),
                     psychTestR::i18n("TCCM_0005_CHOICE7")),
                trigger_button_text = psychTestR::i18n("CONTINUE"),
                failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales),
                   psychTestR::end_module())
}

postprocess_ccm <- function(questionnaire_id, label, subscale, results, scores) {
  #browser()
  if (subscale == "General") {
    count_q1 <- if (results[[label]][["q1"]] == c("choice9")) {
      0
    } else {
      choices <- results[[label]][["q1"]]
      if("choice9" %in% choices) {
        shiny::stopApp("Error: 'choice9' in choices!")
      } else {
        length(choices)
      }
    }
    scoring_map <- psyquest::scoring_maps[[questionnaire_id]]
    mapped_value_q1 <- scoring_map[scoring_map$score == count_q1, ]$raw
    values <- c(mapped_value_q1,
                as.numeric(gsub("[^0-9]", "", results[[label]][["q4"]])),
                as.numeric(gsub("[^0-9]", "", results[[label]][["q5"]])))

    weights <- c(0.8, 0.88, 0.91)
    means <- c(-1.32900, 1.97, 2.254)
    sds <- c(1.801666, 1.25149, 1.43215)
    sum((values - means) * weights / sds)
  } else {
    mean(scores)
  }
}
