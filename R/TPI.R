#' TPI
#'
#' This function defines a TPI module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the TPI in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the TPI,
#' consider using \code{\link{TPI_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Agreeableness"}, \code{"Conscientiousness"}, \code{"Emotional Stability"}, \code{"Extraversion"}, and \code{"Openness to Experiences"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{TPI}()}.
#' @export
TPI <- function(label = "TPI",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "TPI"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 284px",
    dict = dict
  )
}
