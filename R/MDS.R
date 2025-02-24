#' MDS
#'
#' This function defines a Music Dislikes Scale (MDS) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MDS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MDS,
#' consider using \code{\link{MDS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param target (Character scalar or named character vector) The main target (style, artist...) for the questionnaire, if different values for different lanugages are needed then it should be a named vector with the names according to language columns in the dict.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{MDS}()}.
#' @export
MDS <- function(label = "MDS",
                target = c("en" = "a music", "de" = "einer Musik", "de_f" = "einer Musik"),
                dict = psyquest::psyquest_dict,
                ...) {
  #browser()
  stopifnot(purrr::is_scalar_character(label))
  questionnaire_id <- "MDS"
  dict_raw_list <- dict$.__enclos_env__$private$dict %>% as.list()
  orig_target <- target
  if(is.null(target)){
    patch_dict <- dict
    #message("[MDS] No patch ")
  }
  else{
    if(length(target) > 1){
      if(is.null(names(target))){
        stop("Target vector must be named if it has more than one element")
      }
      common_names <- intersect(names(dict_raw_list[[1]]), names(target))
      if(length(common_names) != length(target)){
        warning("Target does not contain all dictionary languages.")
      }
      target <- target[common_names]
      if(length(target) == 0){
        stop("All target names invalid.")
      }
      #message("[MDS] Multi patch ")

    }
    else{
      #message("[MDS] Single patch ")
      target <- rep(target, length(dict_raw_list[[1]]) )
      names(target) <- names(dict_raw_list[[1]])
    }
    #browser()
    prompt <- dict_raw_list[["TMDS_0001_PROMPT"]] %>% unlist()
    #prompt <- dict_raw[dict_raw$key  == "TMDS_0001_PROMPT", names(target)] %>% as.character()
    fixed_prompt <- purrr::map_chr(seq_along(names(target)),
                            function(i){
                              replacement <-
                                stringr::str_replace(prompt[[i]], stringr::fixed("{{target}}"), target[i])

                              })
    #dict_raw[dict_raw$key  == "TMDS_0001_PROMPT", names(target)] <- as.list(fixed_prompt)
    patch_dict <-  dict
    for(i in seq_along(fixed_prompt)){
      patch_dict$edit("TMDS_0001_PROMPT", names(prompt)[i], fixed_prompt[i])
    }
  }
  items <- get_items(questionnaire_id) %>% filter(subscales != "Target")
  args <- list(...)
  num_items <- args$num_items
  if(!is.null(num_items)){
    num_items <- max(1, min(num_items, nrow(items)))
    items <- items %>% dplyr::slice(1:num_items)
  }
  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = items,
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px;width: 290px",
    dict = patch_dict,
    style_params = list(prompt_style = "color:red;font-size:20px", with_counter = FALSE)
  )
}
