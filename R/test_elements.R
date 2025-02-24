#' New month and year select page
#'
#' Creates month and year select page.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param save_answer Whether or not to save the answer.
#'
#' @param validate Validation function to execute.
#'
#' @param failed_validation_message Message to be displayed when validation fails.
#'
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id HTML ID for the response user interface.
#'
#' @param on_complete Optional function to execute on leaving the page
#' (after successful validation).
#' The argument list should include \code{...},
#' and any of:
#' \code{state}, the participant's state object;
#' \code{answer}, the participant's most recent answer;
#' \code{input}, the current page's Shiny input object;
#' \code{session}, the current Shiny session object;
#' \code{opt}, the test's option list as created by \code{test_options()}.
#'
#' @param admin_ui Optional UI component for the admin panel.
#'
#' @export
month_and_year_select_page <-
  function(label,
           prompt,
           save_answer = TRUE,
           min_year = 1930,
           max_year = 2013,
           show_month = TRUE,
           validate = month_and_year_select_page.validate(show_month),
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL,
           failed_validation_message = psychTestR::i18n(ifelse(show_month, "SELECT_MONTH_AND_YEAR", "SELECT_YEAR"))) {
    stopifnot(
      is.scalar.character(label)
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_month_and_year_select(
        label,
        hide = hide_response_ui,
        id = response_ui_id,
        min_year = min_year,
        max_year = max_year,
        show_month = show_month
      )
    )
    get_answer <- function(input, ...) {
      c(input$month, input$year)
    }
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Validate month and year from the month and year select page
#'
month_and_year_select_page.validate <- function(show_month = T) {
  function(state, input, ...) {
    if ((is.null(input$month) || input$month != "NA") && input$year != "NA") {
     TRUE
    } else {
      psychTestR::i18n(ifelse(show_month, "SELECT_MONTH_AND_YEAR", "SELECT_YEAR"))
    }
  }
}

#' Make month and year selectboxes
#'
#' Creates HTML code for month and year selectboxes.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param hide Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param id HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_month_and_year_select <-
  function(label,
           hide = FALSE,
           id = "response_ui",
           min_year = 1930,
           max_year = 2013,
           show_month = TRUE) {
    stopifnot(
      is.scalar.logical(hide),
      max_year >= min_year
    )
    months <- c("SELECT_MONTH", "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
    month_numbers <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    months <- setNames(month_numbers, map(months, psychTestR::i18n))
    years <- c(psychTestR::i18n("SELECT_YEAR"), rev(c(min_year:max_year)))
    years_numbers <- c(NA, rev(c(min_year:max_year)))
    years <- setNames(years_numbers, years)

    outer_div <-
      shiny::tags$div(id = id)

    selectboxes <-
      shiny::tags$div(outer_div,
                      if(show_month) shiny::selectizeInput("month",  label = psychTestR::i18n("MONTH"), choices = months, multiple = FALSE),
                      shiny::selectizeInput("year",
                                            label = psychTestR::i18n("YEAR"),
                                            choices = years, multiple = FALSE))

    shiny::tags$div(id = "rb", style = "width: 300px",
                    selectboxes,
                    psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
  }

#' Labelled text input page
#'
#' Creates a page where the participant puts their
#' answer in a text box. Derived from the psychTestR version  to include labels
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to display (character scalar or Shiny tag object).
#'
#' @param one_line Whether the answer box only has one line of text.

#' @param placeholder Placeholder text for the text box (character scalar).
#'
#' @param button_text Text for the submit button (character scalar).
#'
#' @param width Width of the text box (character scalar, should be valid HTML).
#'
#' @param height Height of the text box (character scalar, should be valid HTML).
#'
#' @export
labelled_text_input_page <- function(label, prompt,
                                     one_line = TRUE,
                                     save_answer = TRUE,
                                     placeholder = NULL,
                                     button_text = "Next",
                                     width = "300px",
                                     height = "100px", # only relevant if one_line == FALSE
                                     validate = NULL,
                                     input_label = NULL,
                                     on_complete = NULL,
                                     admin_ui = NULL) {
  stopifnot(is.scalar.character(label),
            is.scalar.logical(one_line))
  text_input <- if (one_line) {
    shiny::textInput("text_input", label = NULL,
                     placeholder = placeholder,
                     width = width)
  } else {
    shiny::textAreaInput("text_input", label = NULL,
                         placeholder = placeholder,
                         width = width,
                         height = height)
  }
  get_answer <- function(input, ...) input$text_input
  body = shiny::div(
    onload = "document.getElementById('text_input').value = '';",
    psychTestR:::tagify(prompt),
    pos_table(text_input, input_label, style = "display: inline-block; margin:auto")
  )
  # label_css <-  shiny::tags$head(
  #   shiny::tags$style(type="text/css",
  #                     "#inlin label{ display: table-cell; text-align: right; vertical-align: middle; }
  #                      #inlin .form-group { display: table-row;}")
  # )
  ui <- shiny::div(body, psychTestR::trigger_button("next", button_text))
  psychTestR::page(ui = ui, label = label, get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}

pos_table <- function(elts, label = "cents", ...){
  shiny::tags$table(
    shiny::tags$th(
      shiny::tags$td(style = "min-width:25%;vertical-align:middle;padding:0px"),
      shiny::tags$td(style = "min-width:50%;text-align:center;margin-top:10%;vertical-align:middle;padding:0px", elts),
      shiny::tags$td(style = "min-width:25%;text-align:center;vertical-align:middle;padding:1em;", label)
    ),
    ...
  )
}
