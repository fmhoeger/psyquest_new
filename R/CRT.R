#' CRT
#'
#' This function defines a CRT module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CRT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the CRT, consider using \code{\link{CRT_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) (i18n_dict) (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param language Language the questionnaire is rendered in.
#' @param ... Further arguments to be passed to \code{\link{CRT}()}.
#' @export
CRT <- function(label = "CRT",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "CRT"

  main_test_crt(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    language = language,
    offset = 1,
    arrange_vertically = TRUE,
    dict = dict
  )
}

CRT_style <- "text-align:justify;margin-left:25%;margin-right:25%;max-width:400px"

validate_text <- function(answer, ...){
  if (answer == "") {
    psychTestR::i18n("ANSWER_NEEDED")
  } else {
    TRUE
    }
}

include_last_items <- function(state, ...){
  show_last_items <- psychTestR::get_local("show_last_items", state)
  if(is.null(show_last_items)){
    show_last_items <- FALSE
  }
  show_last_items
}

main_test_crt <- function(questionnaire_id, label, items, language, offset = 1, arrange_vertically = TRUE, dict) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- c()
  if ("TCRT_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      labelled_text_input_page("q1",
                      prompt = shiny::p(psychTestR::i18n("TCRT_0002_PROMPT"), style = CRT_style),
                      button_text = psychTestR::i18n("CONTINUE"),
                      width = "50px",
                      validate = NULL,
                      input_label = psychTestR::i18n("TCRT_0002_CHOICE1")
                      ),
    dict = dict
    ))
  }

  if ("TCRT_0003" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      labelled_text_input_page("q2",
                      prompt = shiny::p(psychTestR::i18n("TCRT_0003_PROMPT"), style = CRT_style),
                      button_text = psychTestR::i18n("CONTINUE"),
                      width = "50px",
                      validate = NULL,
                      input_label = psychTestR::i18n("TCRT_0003_CHOICE1")
      )
      ,
      dict = dict
    ))
  }

  if ("TCRT_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      labelled_text_input_page("q3",
                      prompt = shiny::p(psychTestR::i18n("TCRT_0004_PROMPT"), style = CRT_style),
                      button_text = psychTestR::i18n("CONTINUE"),
                      width = "50px",
                      validate = NULL,
                      input_label = psychTestR::i18n("TCRT_0004_CHOICE1")
      )
      ,
      dict = dict
    ))
  }
  if ("TCRT_0005" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q4",
                prompt = psychTestR::i18n("TCRT_0005_PROMPT"),
                choices = sprintf("%d", 1:3),
                labels = map(sprintf("TCRT_0005_CHOICE%d", 1:3), psychTestR::i18n),
                arrange_vertically = TRUE,
                button_style = "min-width: 200px"
      ),
      dict = dict
    ),
    psychTestR::code_block(function(state, ...){
      res <- psychTestR::get_results(state, complete = F) %>% as.list()
      show_last_items <- FALSE
      if("CRT" %in% names(res)){
        if(res$CRT$q4 == "1"){
          show_last_items <- TRUE
        }
      }
      psychTestR::set_local("show_last_items", show_last_items, state)
    }))
  }

  if ("TCRT_0006" %in% prompt_ids) {
    elts <- psychTestR::join(elts,
                             psychTestR::conditional(
                               test = include_last_items,
                               logic = psychTestR::new_timeline(
                                 checkbox_page("q5",
                                               prompt = psychTestR::i18n("TCRT_0006_PROMPT"),
                                               choices = sprintf("%d", 1:3),
                                               labels = map(sprintf("TCRT_0006_CHOICE%d", 1:3), psychTestR::i18n),
                                               force_answer = FALSE,
                                               trigger_button_text = psychTestR::i18n("CONTINUE"),
                                               failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
                                 dict = dict
                               )))
  }

  if ("TCRT_0007" %in% prompt_ids) {
    elts <- psychTestR::join(elts,
                             psychTestR::conditional(
                               test = include_last_items,
                               logic = psychTestR::new_timeline(
                                 NAFC_page("q6",
                                           prompt = psychTestR::i18n("TCRT_0007_PROMPT"),
                                           choices = sprintf("%d", 1:3),
                                           labels = map(sprintf("TCRT_0007_CHOICE%d", 1:3), psychTestR::i18n),
                                           arrange_vertically = TRUE,
                                           button_style = "min-width: 200px"
                                 ),
                                 dict = dict
                               )))
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales = c()),
                   psychTestR::end_module())
}

get_plain_text_crt <- function(results, label, item_id){
  plain_text <- map_chr(results[[label]][[sprintf("q%s", item_id)]], function(x){
    sprintf("'%s'",
            psyquest::psyquest_dict$translate(sprintf("TCRT_00%02d_CHOICE%s",
                                                      as.integer(item_id) + 1, x), language = "en"))
  })
  paste(plain_text, collapse = ",")
}

parse_number <- function(answer){
  if(!is.null(answer)){
    answer <- stringr::str_extract(answer, "[0-9.]+")
  }
  answer
}
postprocess_crt <- function(label, subscale, results, scores) {

  plain_text_items <- c("Correct Answer" = 6,
                        "Come Across" = 4)
  #browser()
  if (subscale == "Bat and Ball") {
    parse_number(results[[label]][["q1"]])
  }
  else  if (subscale == "Widgets") {
    parse_number(results[[label]][["q2"]])
  }
  else  if (subscale == "Lily Pads") {
    parse_number(results[[label]][["q3"]])
  }
  else  if (subscale == "Which Problems") {
    results[[label]][["q5"]]
  }
  else if(subscale %in% names(plain_text_items)){
    get_plain_text_crt(results, label, plain_text_items[subscale])
  }
  else{
    mean(scores)
  }
}
