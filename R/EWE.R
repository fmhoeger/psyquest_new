#' EWE
#'
#' This function defines a EWE module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the EWE in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the EWE, consider using \code{\link{EWE_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) (i18n_dict) (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param language Language the questionnaire is rendered in.
#' @param ... Further arguments to be passed to \code{\link{EWE}()}.
#' @export
EWE <- function(label = "EWE",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "EWE"

  main_test_ewe(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    language = language,
    offset = 1,
    arrange_vertically = TRUE,
    dict = dict
  )
}

standard_style <- "text-align:justify;margin-left:20%;margin-right:20%;"

validate_text <- function(answer, ...){
  if (answer == "") {
    psychTestR::i18n("ANSWER_NEEDED")
  } else {
    TRUE
    }
}

main_test_ewe <- function(questionnaire_id,
                          label,
                          items,
                          subscales,
                          language,
                          offset = 1,
                          arrange_vertically = TRUE,
                          dict) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- c()
  if ("TEWE_0001" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      text_input_page("q1",
                      prompt = shiny::p(psychTestR::i18n("TEWE_0001_PROMPT"), style = standard_style),
                      button_text = psychTestR::i18n("CONTINUE"),
                      validate = NULL
                      )
,
    dict = dict
    ))
  }

  if ("TEWE_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q2",
                prompt = psychTestR::i18n("TEWE_0002_PROMPT"),
                choices = sprintf("%d", 1:5),
                labels = map(sprintf("TEWE_0002_CHOICE%d", 1:5), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 200px"
      ),
    dict = dict
    ))
  }

  if ("TEWE_0003" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q3",
                prompt = psychTestR::i18n("TEWE_0003_PROMPT"),
                choices = sprintf("%d", 1:2),
                labels = map(sprintf("TEWE_0003_CHOICE%d", 1:2), psychTestR::i18n),
                button_style = "min-width: 100px"
    ),
    dict = dict
    ))
  }

  if ("TEWE_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q4",
                prompt = psychTestR::i18n("TEWE_0004_PROMPT"),
                choices = sprintf("%d", 1:5),
                labels = map(sprintf("TEWE_0004_CHOICE%d", 1:5), psychTestR::i18n),
                button_style = "min-width: 250px;"
    ),
    dict = dict
    ))
  }

  if ("TEWE_0005" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q5",
                prompt = psychTestR::i18n("TEWE_0005_PROMPT"),
                choices = sprintf("%d", 1:5),
                labels = map(sprintf("TEWE_0005_CHOICE%d", 1:5), psychTestR::i18n),
                button_style = "min-width: 250px;"
      ),
      dict = dict
    ))
  }

  if ("TEWE_0006" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q6",
                    prompt = psychTestR::i18n("TEWE_0006_PROMPT"),
                    choices = sprintf("%d", 1:7),
                    labels = map(sprintf("TEWE_0006_CHOICE%d", 1:7), psychTestR::i18n),
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    force_answer = TRUE,
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
      dict = dict

    ))}
  if ("TEWE_0007" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      radiobutton_NAFC_page("q7",
                            prompt = psychTestR::i18n("TEWE_0007_PROMPT"),
                            choices = sprintf("%d", 1:7),
                            labels = map(sprintf("TEWE_0007_CHOICE%d", 1:7), psychTestR::i18n),
                            trigger_button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = dict
    ))
  }

  if ("TEWE_0008" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      radiobutton_NAFC_page("q8",
                            prompt = psychTestR::i18n("TEWE_0008_PROMPT"),
                            choices = sprintf("%d", 1:6),
                            labels = map(sprintf("TEWE_0008_CHOICE%d", 1:6), psychTestR::i18n),
                            trigger_button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = dict
    ))
  }
  if ("TEWE_0009" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      radiobutton_NAFC_page("q9",
                            prompt = psychTestR::i18n("TEWE_0009_PROMPT"),
                            choices = sprintf("%d", 1:3),
                            labels = map(sprintf("TEWE_0009_CHOICE%d", 1:3), psychTestR::i18n),
                            trigger_button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = dict
    ))
  }
  if ("TEWE_0010" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q10",
                prompt= psychTestR::i18n("TEWE_0010_PROMPT"),
                choices = sprintf("%d", 1:4),
                labels = map(sprintf("TEWE_0010_CHOICE%d", 1:4), psychTestR::i18n),
                button_style = "min-width: 250px"
      ),
      dict = dict
    ))
  }
  if ("TEWE_0011" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q11",
                    prompt = psychTestR::i18n("TEWE_0011_PROMPT"),
                    choices = sprintf("%d", 1:15),
                    labels = map(sprintf("TEWE_0011_CHOICE%d", 1:15), psychTestR::i18n),
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    force_answer = TRUE,
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
      dict = dict

    ))}

  if ("TEWE_0012" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      radiobutton_NAFC_page("q12",
                            prompt = psychTestR::i18n("TEWE_0012_PROMPT"),
                            choices = sprintf("%d", 1:3),
                            labels = map(sprintf("TEWE_0012_CHOICE%d", 1:3), psychTestR::i18n),
                            trigger_button_text = psychTestR::i18n("CONTINUE")),
    dict = dict
    ))
  }
  if ("TEWE_0013" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q13",
                prompt = psychTestR::i18n("TEWE_0013_PROMPT"),
                choices = sprintf("%d", 1:5),
                labels = map(sprintf("TEWE_0013_CHOICE%d", 1:5), psychTestR::i18n),
                button_style = "min-width: 250px;"),
    dict = dict
    ))
  }
  if ("TEWE_0014" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q14",
                    prompt = psychTestR::i18n("TEWE_0014_PROMPT"),
                    choices = sprintf("%d", 1:7),
                    labels = map(sprintf("TEWE_0014_CHOICE%d", 1:7), psychTestR::i18n),
                    force_answer = TRUE,
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
      dict = dict

    ))}
  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales),
                   psychTestR::end_module())
}

get_plain_text_ewe <- function(results, label, item_id){
  plain_text <- map_chr(results[[label]][[sprintf("q%s", item_id)]], function(x){
    sprintf("'%s'",
            psyquest::psyquest_dict$translate(sprintf("TEWE_00%02d_CHOICE%s", as.integer(item_id), x),
                                              language = "en"))
  })
  paste(plain_text, collapse = ",")

}
postprocess_ewe <- function(label, subscale, results, scores) {
  #browser()
  plain_text_items <- c("Lyrics" = 3,
                        "Trigger" = 6,
                        "Origin" = 7,
                        "Content" = 8,
                        "InnerForm" = 9,
#                        "Reproduction" = 10,
                        "Genre" = 11,
                        "Regularity" = 12,
                        "CounterStrategies" = 14)
  #browser()
  if (subscale == "Earworm") {
    results[[label]][["q1"]]
  }
  else if(subscale %in% names(plain_text_items)){
    get_plain_text_ewe(results, label, plain_text_items[subscale])
  }
  else{
    mean(scores)
  }
}
