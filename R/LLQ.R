#' LLQ
#'
#' This function defines a LLQ module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the LLQ in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the LLQ, consider using \code{\link{LLQ_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) (i18n_dict) (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param language Language the questionnaire is rendered in.
#' @param ... Further arguments to be passed to \code{\link{LLQ}()}.
#' @export
LLQ <- function(label = "LLQ",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "LLQ"

  main_test_llq(
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

main_test_llq <- function(questionnaire_id,
                          label,
                          items,
                          subscales,
                          language,
                          offset = 1,
                          arrange_vertically = TRUE,
                          dict) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  #browser()
  elts <- c()
  if ("TLLQ_0001" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q1",
                    prompt =  psychTestR::i18n("TLLQ_0001_PROMPT"),
                    choices = sprintf("%d", 1:9),
                    labels = map(sprintf("TLLQ_0001_CHOICE%d", 1:9), psychTestR::i18n),
                    force_answer = TRUE,
                    javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
    dict = dict
    ))
  }
  if ("TLLQ_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q2",
                    prompt =  psychTestR::i18n("TLLQ_0002_PROMPT"),
                    choices = sprintf("%d", 1:11),
                    labels = map(sprintf("TLLQ_0002_CHOICE%d", 1:11), psychTestR::i18n),
                    force_answer = TRUE,
                    javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
    dict = dict
    ))
  }

  if ("TLLQ_0003" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q3",
                    prompt =  psychTestR::i18n("TLLQ_0003_PROMPT"),
                    choices = sprintf("%d", 1:10),
                    labels = map(sprintf("TLLQ_0003_CHOICE%d", 1:10), psychTestR::i18n),
                    force_answer = TRUE,
                    javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
    dict = dict
    ))
  }

  if ("TLLQ_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q4",
                prompt = psychTestR::i18n("TLLQ_0004_PROMPT"),
                choices = sprintf("%d", 1:2),
                labels = map(sprintf("TLLQ_0004_CHOICE%d", 1:2), psychTestR::i18n),
                button_style = "min-width: 125px;"),
    dict = dict
    ))
  }

  if ("TLLQ_0005" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q5",
                prompt = psychTestR::i18n("TLLQ_0005_PROMPT"),
                choices = sprintf("%d", 1:2),
                labels = map(sprintf("TLLQ_0005_CHOICE%d", 1:2), psychTestR::i18n),
                button_style = "min-width: 125px;"
      ),
      dict = dict
    ))
  }

  if ("TLLQ_0006" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      checkbox_page("q6",
                    prompt =  psychTestR::i18n("TLLQ_0006_PROMPT"),
                    choices = sprintf("%d", 1:11),
                    labels = map(sprintf("TLLQ_0006_CHOICE%d", 1:11), psychTestR::i18n),
                    force_answer = TRUE,
                    #javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                    failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
    dict = dict
    ))
  }
  if ("TLLQ_0007" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q7",
                prompt = psychTestR::i18n("TLLQ_0007_PROMPT"),
                choices = sprintf("%d", 1:2),
                labels = map(sprintf("TLLQ_0007_CHOICE%d", 1:2), psychTestR::i18n),
                button_style = "min-width: 125px;"
      ),
      dict = dict
    ))
  }


  if ("TLLQ_0008" %in% prompt_ids) {
    elts <- psychTestR::join(elts,
                             psychTestR::new_timeline(
                               NAFC_page("q8",
                                         prompt = psychTestR::i18n("TLLQ_0008_PROMPT"),
                                         choices = sprintf("%d", 1:3),
                                         labels = map(sprintf("TLLQ_0008_CHOICE%d", 1:3), psychTestR::i18n),
                                         on_complete = function(answer, state, ...){
                                           set_local("lullabies_child", answer, state)
                                         },
                                         button_style = "min-width: 125px;"
                               ),
                               dict = dict
                             )
    )
  }

  if ("TLLQ_0009" %in% prompt_ids) {
    elts <- psychTestR::join(elts,
                             psychTestR::conditional(test = function(state, ...) {
                               psychTestR::get_local("lullabies_child", state) == "1"
                             },
                             logic =
                               psychTestR::new_timeline(
                                 checkbox_page("q9",
                                               prompt =  psychTestR::i18n("TLLQ_0009_PROMPT"),
                                               choices = sprintf("%d", 1:6),
                                               labels = map(sprintf("TLLQ_0009_CHOICE%d", 1:6), psychTestR::i18n),
                                               force_answer = TRUE,
                                               javascript = "checkboxes = $('input:checkbox'); checkboxes.slice(checkboxes.length - 1, checkboxes.length).click(function() { checkboxes.slice(0, checkboxes.length - 1).prop('checked', '') }); checkboxes.slice(0, checkboxes.length - 1).click(function() { checkboxes.slice(checkboxes.length - 1, checkboxes.length).prop('checked', '') });",
                                               trigger_button_text = psychTestR::i18n("CONTINUE"),
                                               failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER")),
                                 dict = dict
                               )))
  }

  if ("TLLQ_0010" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q10",
                prompt = psychTestR::i18n("TLLQ_0010_PROMPT"),
                choices = sprintf("%d", 1:2),
                labels = map(sprintf("TLLQ_0010_CHOICE%d", 1:2), psychTestR::i18n),
                button_style = "min-width: 125px;"
      ),
      dict = dict
    ))
  }


  if ("TLLQ_0011" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      NAFC_page("q11",
                prompt = psychTestR::i18n("TLLQ_0011_PROMPT"),
                choices = sprintf("%d", 1:2),
                labels = map(sprintf("TLLQ_0011_CHOICE%d", 1:2), psychTestR::i18n),
                button_style = "min-width: 125px;"
      ),
      dict = dict
    ))
  }

  if ("TLLQ_0012" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(
      labelled_text_input_page("q12",
                               prompt = psychTestR::i18n("TLLQ_0012_PROMPT"),
                               one_line = F,
                               save_answer = T,
                               validate = NULL,
                               button_text = psychTestR::i18n("CONTINUE"),
                               width = "300px",
                               height = "200px"),

      dict = dict
    ))
  }
  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales),
                   psychTestR::end_module())
}

get_plain_text_llq <- function(results, label, item_id){
  plain_text <- map_chr(results[[label]][[sprintf("q%s", item_id)]], function(x){
    sprintf("'%s'",
            psyquest::psyquest_dict$translate(sprintf("TLLQ_00%02d_CHOICE%s", as.integer(item_id), x),
                                              language = "en"))
  })
  paste(plain_text, collapse = ",")

}

postprocess_llq <- function(label, subscale, results, scores) {
  #browser()
  plain_text_items <- c("Feeling Listening" = 1,
                        "Feeling Singing" = 2,
                        "Sleeping Aids" = 3,
                        "Calmness" = 4,
                        "Rocking Rhythm" = 5,
                        "Lullaby Themes" = 6,
                        "Transmission" = 7,
                        "Lullabies Childhood" = 8,
                        "Lullaby Singer" = 9,
                        "Tradition Importance" = 10,
                        "Same Lullabies" = 11,
                        "Lullabies" = 12)
  #browser()
  if (subscale == "Lullabies") {
    results[[label]][["q12"]]
  }
  # else if(subscale == "Lullabies Childhood"){
  #   scores[scores == 3] <- NA
  #   2 - scores
  # }
  else if(subscale %in% names(plain_text_items)){
    get_plain_text_llq(results, label, plain_text_items[subscale])
  }
  else{
    mean(2 - scores)
  }
}


