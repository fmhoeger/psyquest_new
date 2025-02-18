is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

get_year <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}

#'get_tests
#'
#'Retrieves all available tests
#'
#' @export
get_tests <- function(){
  psyquest::psyquest_item_bank %>%
    pull(q_id) %>% unique() %>% sort()

}

#'get_subscales
#'
#'Retrieves available subscales for a questionnaired identified by questionaired_id
#' @param questionaired_id (three letter string) Questionnaire ID.
#'
#' @export
get_subscales <- function(questionnaire_id){
  psyquest::psyquest_item_bank %>%
    filter(stringr::str_detect(q_id, !!questionnaire_id)) %>%
    pull(subscales) %>% stringr::str_split(";") %>% unlist() %>% unique()

}

#'get_item_info
#'
#'Retrieves for info for items from questionnaire identified by questionnaire_id and subscale
#' @param questionnaire_id (three letter string) Questionnaire ID.
#' @param subscales (character vector)  of subscale names
#' @param language (character)  language of item texts
#'
#' @export
get_item_info <- function(questionnaire_id,
                          subscales = c(),
                          language = "en"){
  items <- get_items(questionnaire_id, subscales) %>%
    mutate(polarity = c("positive", "negative")[1 + stringr::str_detect(score_func, "-x|-\\(x")],
           prompt_id = stringr::str_extract(prompt_id, "[0-9]+$"),
           num_options = stringr::str_extract(option_type, "^[0-9]+")) %>%
    select(q_id, item_id, prompt_id, polarity, subscales, num_options)
  #browser()
  prompts <- psyquest::psyquest_dict_df %>%
    as.data.frame() %>%
    filter(stringr::str_detect(key, questionnaire_id)) %>%
    filter((key %in% sprintf("T%s_%s_PROMPT", questionnaire_id, items$prompt_id))) %>%
    select(!!language)
  items %>% dplyr::bind_cols(prompts)
}

#'get_item_choices
#'
#'Retrieves choice labels for an items from questionnaire identified by questionnaire_id and subscale
#' @param questionnaire_id (three letter string) Questionnaire ID.
#' @param item_id (integer or character) number of item in a questionnaire
#' @param language (character)  language of item texts
#'
#' @export
get_item_choices <- function(questionnaire_id, item_id, language = "en"){
  #browser()
  sstr <- sprintf("%s_%04d_CHOICE", questionnaire_id, as.integer(stringr::str_remove(as.character(item_id), "q")))
  choices <- psyquest::psyquest_dict_df %>%
    filter(stringr::str_detect(key, sstr))
  if(length(language) == 1){
    if(nrow(choices) > 0){
      choices %>% pull(!!dplyr::sym(language))
    }
    else{
      character(0)
    }
  }
  else{
    if(nrow(choices) > 0){
      choices %>% select(key, dplyr::all_of(language))
    }
    else{
      choices
    }
  }
}


#'get_items
#'
#'Retrieves items for a questionnaire identified by label and subscales
#' @param label (three letter string) Questionnaire ID.
#' @param subscales (character vector)  of subscale names
#' @param short_version (logical)  Short verion of GMS
#' @param configuationr_filepath (string)  Config file forGMS
#'
#' @export
get_items2 <- function(label, subscales = c(), short_version = FALSE, configuration_filepath = NULL) {
  prompt_id <- NULL
  items <- psyquest::psyquest_item_bank %>%
    filter(stringr::str_detect(prompt_id, stringr::str_interp("T${label}")))

  subscale_items <- NULL

  if (!is.null(subscales)) {
    filtered_items <- as.data.frame(items[map(subscales, function(x) grep(gsub("(", "\\(", gsub(")", "\\)", x, fixed = TRUE), fixed = TRUE), items$subscales)) %>% unlist() %>% unique(), ])
    if(nrow(filtered_items) == 0){
      stop(sprintf("Invalid subscales result in empty item set (subscales: %s)", paste(subscales, collapse = ", ")))
    }
    if(!short_version && label == "PHT"){
      return(filtered_items[order(filtered_items$prompt_id), ])
    }
  }

  question_ids <- c()
  if (label == "SCA") {
    if (short_version) {
      question_ids <- c(3, 13, 26, 30)
    } else {
      question_ids <- c(2, 4:12, 14:25, 27:29)
    }
    filtered_items <- as.data.frame(items[map(question_ids, function(x) grep(sprintf("TSCA_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

    return(filtered_items[order(filtered_items$prompt_id), ])
  } else if (label == "SCS") {
    if (short_version) {
      question_ids <- c(3, 5, 6, 10, 11, 17, 18, 22)
    } else {
      question_ids <- c(2:26)
    }
    filtered_items <- as.data.frame(items[map(question_ids, function(x) grep(sprintf("TSCS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

    return(filtered_items[order(filtered_items$prompt_id), ])
  } else if (label == "PHT") {
    if (short_version) {
      #old question_ids <- c(1, 4, 7, 11, 15, 20, 23, 27, 29, 32, 37, 39, 41, 46, 48, 53, 55)
      question_ids <- c(2, 7, 8, 13, 16, 18, 20, 23, 27, 31, 32, 37, 38, 43, 44, 48, 50)
    } else {
      question_ids <- c(1:51)
    }
    pht_subscales <- get_subscales("PHT")
    if(length(subscales) > 0){
     pht_subscales <- intersect(pht_subscales, subscales)
    }

    subscale_ids <- psyquest::get_item_info("PHT", pht_subscales)
    question_ids <- intersect(question_ids, subscale_ids$item_id)
    stopifnot(length(question_ids) > 0)
    filtered_items <- as.data.frame(items[map(question_ids, function(x) grep(sprintf("TPHT_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])
    browser()
    return(filtered_items[order(filtered_items$prompt_id), ])
  } else if (label == "SMP") {
    if (short_version) {
      question_ids <- c(2, 4, 5, 6, 7, 8, 9, 11, 13, 17, 19, 21, 22, 23)
    } else {
      question_ids <- c(2:24)
    }
    filtered_items <- as.data.frame(items[map(question_ids, function(x) grep(sprintf("TSMP_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

    return(filtered_items[order(filtered_items$prompt_id), ])
  } else if (label == "FSS") {
    filtered_items <- psyquest::psyquest_item_bank %>% filter(q_id == "FSS")
    if(short_version){
      filtered_items <- filtered_items %>% filter(short_version)
    }
    return(filtered_items %>% arrange(prompt_id))
  } else if (label == "GMS") {
    if (!is.null(configuration_filepath)) {
      subscale_ids <-
        (
          read.csv(
            file = configuration_filepath,
            header = TRUE,
            stringsAsFactors = FALSE,
          ) %>% tibble::as_tibble()
        )$MID

      subscale_id <- NULL
      question_ids <- unlist(map((
        read.csv(
          file = system.file("extdata", "GMS_items_to_subscales.csv", package = "psyquest"),
          header = TRUE,
          stringsAsFactors = FALSE
        ) %>% tibble::as_tibble() %>% filter(subscale_id %in% subscale_ids)
      )$item_id, function(item_id)
        as.integer(unlist(strsplit(item_id, "_"))[2])))

      filtered_items <- as.data.frame(items[map(question_ids, function(x) grep(sprintf("TGMS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

      return(filtered_items[order(filtered_items$prompt_id), ])
    } else if (short_version) {
      # list("Active Engagement" =  c(1, 2, 5, 6, 7, 9),  # -> 1, 4, 19, 27, 30, 36
      #      "Emotions" = c(1, 2, 3, 5, 6),               # -> 3, 10, 20, 26, 39
      #      "Musical Training" = c(1, 2, 3, 6, 7),       # -> 2, 12, 16, 22, 35
      #      "Perceptual Abilities"= c(1, 3, 4, 6, 7, 8), # -> 6, 13, 14, 24, 28, 29
      #      "Instrument" = 1,                            # -> 32
      #      "Singing Abilities" = c(2, 3, 4, 5),         # -> 8, 11, 23, 33
      #      "Start Age" = 1,                             # -> 40
      #      "Absolute Pitch" = 1)                        # -> 41
      question_ids <- c(1, 4, 19, 27, 30, 36, 3, 10, 20, 26, 39, 2, 12, 16, 22, 35, 6, 13, 14, 24, 28, 29, 32, 8, 11, 23, 33, 40, 41)

      filtered_items <- as.data.frame(items[map(question_ids, function(x) grep(sprintf("TGMS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

      return(filtered_items[order(filtered_items$prompt_id), ])
    }
  }

  items[order(items$prompt_id), ]
}
get_GMS_item_ids_from_config <- function(configuration_filepath){
  subscale_ids <-
    (
      read.csv(
        file = configuration_filepath,
        header = TRUE,
        stringsAsFactors = FALSE,
      ) %>% tibble::as_tibble()
    )$MID

  subscale_id <- NULL
  question_ids <- unlist(map((
    read.csv(
      file = system.file("extdata", "GMS_items_to_subscales.csv", package = "psyquest"),
      header = TRUE,
      stringsAsFactors = FALSE
    ) %>% tibble::as_tibble() %>% filter(subscale_id %in% subscale_ids)
  )$item_id, function(item_id)
    as.integer(unlist(strsplit(item_id, "_"))[2])))
}

#'get_items
#'
#'Retrieves items for a questionnaire identified by label and subscales
#' @param label (three letter string) Questionnaire ID.
#' @param subscales (character vector)  of subscale names
#' @param short_version (logical)  Short version (for some tests)
#' @param configuationr_filepath (string)  Config file for GMS
#'
#' @export
get_items <- function(q_id, subscales = c(), short_version = FALSE, configuration_filepath = NULL) {
  items <- psyquest::psyquest_item_bank %>%
    filter(q_id == !!q_id)
  if(nrow(items) == 0){
    stop(sprintf("Invalid questionnaire: %s", q_id))
  }
  #configuration file takes highest precedence
  if (q_id == "GMS" && !is.null(configuration_filepath)) {
    question_ids <- get_GMS_item_ids_from_config(configuration_filepath)
    return(items %>% filter(item_id %in% question_ids))
  }

  if(length(subscales) > 0 ){
    has_subscale <- sapply(items$subscales %>% stringr::str_split(";"), function(x){
      any(sapply(subscales, function(y){
        ifelse(nzchar(y), y %in% x, TRUE)
      } ))
    }, USE.NAMES = F)

    items <- items[has_subscale,]

    if(nrow(items) == 0){
      stop(sprintf("Invalid subscales (subscales: %s)", paste(subscales, collapse = ", ")))
    }
    if(q_id != "PHT" && short_version){
      warning(sprintf("Subscale filter and short version should not be combined for %s.", q_id))
    }

  }
  question_ids <- items$item_id

  if (short_version) {
    if (q_id == "SCA") {
      question_ids <- c(3, 13, 26, 30) -1
    }
    else if (q_id == "SCS") {
      question_ids <- c(3, 5, 6, 10, 11, 17, 18, 22) - 1
    }
    else if (q_id == "PAC") {
      browser()
      question_ids <- c(11:16)
    }
    else if (q_id == "PHT") {
      #question_ids <- c(1, 4, 7, 11, 15, 20, 23, 27, 29, 32, 37, 39, 41, 46, 48, 53, 55)
      #question_ids <- c(1, 6, 7, 12, 15, 17, 19, 22, 26, 30, 31, 36, 37, 42, 43, 47, 49)
      question_ids <- psyquest_item_bank %>% filter(q_id == "PHT") %>% filter(short_version) %>% pull(item_id)

    }
    else if (q_id == "SMP") {
      question_ids <- c(2, 4, 5, 6, 7, 8, 9, 11, 13, 17, 19, 21, 22, 23) - 1
    }
    else if (q_id == "FSS") {
      question_ids <- psyquest::psyquest_item_bank %>% filter(q_id == "FSS", short_version) %>% pull(item_id)
    }
    else if (q_id == "GMS") {
      question_ids <- c(1, 4, 19, 27, 30, 36, 3, 10, 20, 26, 39, 2, 12, 16, 22, 35, 6, 13, 14, 24, 28, 29, 32, 8, 11, 23, 33, 40, 41)

    }
  }

  items <- items[items$item_id %in% question_ids,]
  items[order(items$prompt_id),]
}

problems_info <- function(researcher_email) {
  problems_info_html <- c()
  for (i in 1:length(languages())) {
    span <- shiny::tags$span(
      psyquest::psyquest_dict$translate("PROBLEMS_INFO_1", languages()[[i]]),
      shiny::tags$br(),
      psyquest::psyquest_dict$translate("PROBLEMS_INFO_2", languages()[[i]]),
      shiny::tags$a(href = paste0("mailto:", researcher_email), researcher_email),
      psyquest::psyquest_dict$translate("PROBLEMS_INFO_3", languages()[[i]]))
    problems_info_html[[i]] <- span
  }

  names(problems_info_html) <- languages()
  problems_info_html
}
