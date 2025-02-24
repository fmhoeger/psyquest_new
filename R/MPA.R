#' MPA
#'
#' This function defines a Music Performance Anxiety Scale (MPA) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MPA in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MPA,
#' consider using \code{\link{MPA_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{MPA}()}.
#' @export
MPA <- function(label = "MPA",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test_mpa(
    label = label,
    button_style = "min-width: 290px",
    style_params = list(...)$style_params,
    dict = dict
  )
}

main_test_mpa <- function(label = label, button_style, style_params, dict){
  #browser()
  items <- get_items("MPA") %>% filter(subscales != "Playing an instrument")
  first_page <- psychTestR::new_timeline(
    psychTestR::NAFC_page(label = "plays_instrument",
      prompt =  shiny::p(psychTestR::i18n("TMPA_0002_PROMPT"),
                         style = "text-align:center;margin-left:20%;margin-right:20%;"),
      choices = c("yes", "no"),
      labels = map(sprintf("TMPA_0002_CHOICE%d",  1:2), psychTestR::i18n),
      arrange_vertically = F,
      button_style = "min-width:100px",
      save_answer = T
  ),
  dict = dict)

  main_test <- main_test("MPA",
                         label = label,
                         items = items,
                         offset = 2,
                         arrange_vertically = T,
                         button_style = button_style,
                         style_params = style_params,
                         dict = dict)
  mpa <- psychTestR::join(
    psychTestR::begin_module("MPA_intro"),
    first_page,
    psychTestR::end_module(),
    psychTestR::conditional(test = function(state, ...) {
      #print(psychTestR::get_results(state,complete = F) %>% as.list() %>% purrr::pluck("MPA_intro") %>% purrr::pluck("plays_instrument"))
      psychTestR::get_results(state,complete = F) %>% as.list() %>% purrr::pluck("MPA_intro") %>% purrr::pluck("plays_instrument") == "yes"
      },
    logic = main_test)
  )
  mpa
}
