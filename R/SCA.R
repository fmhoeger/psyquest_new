#' SCA
#'
#' This function defines a SCA module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SCA in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SCA,
#' consider using \code{\link{SCA_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#' @param ... Further arguments to be passed to \code{\link{SCA}()}.
#' @export
SCA <- function(label = "SCA",
                dict = psyquest::psyquest_dict,
                short_version = FALSE,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  subscales <- if (short_version) { c("Extra") } else { c("General") }

  questionnaire_id <- "SCA"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    short_version = short_version,
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 218px",
    dict = dict
  )
}

postprocess_sca <- function(scores) {
  scoring_map <- psyquest::scoring_maps[["SCA"]]
  scoring_map[scoring_map$raw == sum(scores), ]$score
}

postprocess_sca_short <- function(scores) {
  coefficients <- data.frame(difficulty     = c(2.209, 3.392, 3.102),
                             discrimination = c(-1.986, -1.709, -1.988),
                             guessing       = c(-.545, -.464, -.411),
                             inattention    = c(1.607, 1.26, 1.417))
  scoring_map <- psyquest::scoring_maps[["SCA_short"]]
  theta <- catR::thetaEst(it = coefficients, x = scores[1:3] - 1, model = "GRM", method = "WL")
  scoring_map[which.min(abs(scoring_map$raw - theta)),]$score
}
