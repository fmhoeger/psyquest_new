#' SWB
#'
#' This function defines a SWB module (Subjective Wellbeing Scale) for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SWB in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standaSWBe implementation of the SWB,
#' consider using \code{\link{SWB_standaSWBe}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{SWB}()}.
#' @export
SWB <- function(label = "SWB",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "SWB"
  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 0,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px"
  )
}
