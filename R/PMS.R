#' PMS
#'
#' This function defines a Profile of Mood States (PMS) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the PMS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the PMS,
#' consider using \code{\link{PMS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{PMS}()}.
#' @export
PMS <- function(label = "PMS",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "PMS"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px",
    dict = dict
  )
}
