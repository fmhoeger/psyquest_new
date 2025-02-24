#' CMT
#'
#' This function defines a CMT module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CMT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the CMT,
#' consider using \code{\link{CMT_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"(Re)productive competence"}, \code{"Reflection competence"}, \code{"Psychomotoric competence"}, and \code{"Social competence"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{CMT}()}.
#' @export
CMT <- function(label = "CMT",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "CMT"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    with_prompt_head = TRUE,
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px",
    dict = dict
  )
}
