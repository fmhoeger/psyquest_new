#' SMP(STOMP-R)
#'
#' This function defines a SMP module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the STOMP-R in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SMP,
#' consider using \code{\link{SMP_standalone}()}.
#'
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#'
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' There are two possible subscale sets: 1. From the Do-re-mi paper: \code{"Reflective & Complex"},
#' \code{"Intense & Rebellious"}, \code{"Upbeat & Conventional"}, and \code{"Energetic & Rhythmic"};
#' 2. from the STOMP-R paper (MUSIC): \code{"Mellow"}, \code{"Unpretentious"}, \code{"Sophisticated"},
#' \code{"Intense"}, and \code{"Contemporary"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#'@param short_version (Scalar boolean) For the short version (STOMP)
#'of the questionnaire set this to TRUE. Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{SMP}()}.
#'
#' @export

SMP <- function(label = "SMP",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                short_version = FALSE,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "SMP"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales,
                      short_version = short_version),
    subscales = subscales,
    short_version = short_version,
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 284px",
    dict = dict
  )
}
