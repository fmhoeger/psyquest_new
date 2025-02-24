#' PHT
#'
#' This function defines a PHT module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the PHT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the PHT,
#' consider using \code{\link{PHT_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{PHT}()}.
#' @export
PHT <- function(label = "PHT",
                subscales = c(),
                short_version = FALSE,
                randomize = FALSE,
                dict = psyquest::psyquest_dict,

                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "PHT"

  style_params = list(...)$style_params
  if(is.null(style_params)){
    style_params <- list(prompt_style = "color:#1f77b4;font-size:20px;margin-bottom:20px;font-weight:bold")
  }
  if(randomize){
    if(!is.null(style_params)){
      style_params$with_counter <- FALSE
    }
    else{
      style_params <- list(with_counter = FALSE)
    }
  }
  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id, subscales = subscales, short_version = short_version),
    offset = 1,
    arrange_vertically = TRUE,
    randomize = randomize,
    button_style = "min-width: 250px",
    style_params = style_params,
    dict = dict
  )
}
