#' FSS
#'
#' This function defines a Flow State Scale (Marsh & Jackson, 1996) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the FSS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the FSS,
#' consider using \code{\link{FSS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Challenge-skill balance"}, \code{"Action-awareness merging"},
#' \code{"Clear goals"}, \code{"Unambiguous feedback"}, \code{"Concentration on task at hand"},
#' \code{"Paradox of control"}, \code{"Loss of self-consciousness"}, \code{"Transformation of time"},
#' and \code{"Autotelic experience"}.
#' If no subscales are provided all subscales are selected.
#' @param short_version (boolean) Use short version of test
#' @param ... Further arguments to be passed to \code{\link{main_test}()}.
#' @export
#'

FSS <- function(label = "FSS",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                short_version = F,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "FSS"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      short_version = short_version,
                      subscales = subscales),
    subscales = subscales,
    offset = 0,
    arrange_vertically = TRUE,
    button_style = c("vertically" = "min-width: 275px", "horizontally" = "max-width: 210px"),
    dict = dict
  )
}
