#' Standalone
#'
#' This function launches a standalone testing session for a questionnaire with the specified label.
#' Valid labels are 'BMR', 'BFT'. 'CBQ', CCM', 'DAC', 'DEG', 'GDS', 'GMS', 'GRT', 'HOP', 'HUM', 'HSP', 'IBQ', MES', 'MES', 'MHE', 'MHI', 'MHP', 'PAC', 'PMS', 'LON'
#' 'SDQ', 'SEM', 'SES','SMP', 'SOS', 'SWL', TOI', 'TOM', and 'TPI'.
#' This can be used for data collection, either in the laboratory or online.
#'
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' If no subscales are provided all subscales are selected.
#'
#' @param short_version (Boolean scalar) For the short version of the questionnaire
#' set this to TRUE. Defaults to FALSE.
#'
#' @param configuration_filepath (Character scalar) Optional path to a configuration file
#' exported from the GMSI-Configurator at https://shiny.gold-msi.org/gmsiconfigurator (GMS only).
#'
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#'
#' @param admin_password (Character scalar) Password for accessing the admin panel.
#'
#' @param researcher_email (Character scalar)
#' If not \code{NULL}, this researcher's email address is displayed at the
#' bottom of the screen so that online participants can ask for help.
#'
#' @param with_id (Boolean) Flag if ID should be entered
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto"
#' for default validation which means ID should consist only of alphanumeric characters.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
standalone <- function(label,
                       languages = psyquest::languages(),
                       subscales = NULL,
                       short_version = FALSE,
                       configuration_filepath = NULL,
                       dict = psyquest::psyquest_dict,
                       admin_password = "conifer",
                       researcher_email = NULL,
                       with_id = FALSE,
                       validate_id = "auto",
                       randomize = FALSE,
                       ...) {
  subscales <- sort(subscales)
  items <-
    get_items(label, subscales, short_version, configuration_filepath)

  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(
          prompt = psychTestR::i18n("ENTER_ID"),
          placeholder = paste(psychTestR::i18n("E.G."), "10492817"),
          button_text = psychTestR::i18n("CONTINUE"),
          validate = validate_id
        ),
      dict = dict
    ),
    # Call the questionnaire
    get(label)(
      language = languages,
      items = items,
      subscales = subscales,
      short_version = short_version,
      configuration_filepath = configuration_filepath,
      randomize = randomize,
      ...
    ),
    # psychTestR::code_block(function(state,...){
    #   res <- psychTestR::get_results(state, complete = T)
    #   browser()
    #   print(res %>% as.list())
    # }),
    psychTestR::new_timeline(psychTestR::final_page(
      shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER")
      )
    ), dict = dict)
  )
  title <-
    unlist(setNames(
      map(psyquest::languages(), function(x)
        psyquest::psyquest_dict$translate(stringr::str_interp("T${label}_0000_PROMPT"), x)),
      psyquest::languages()
    ))
  shiny::addResourcePath("www_psyquest", system.file("www", package = "psyquest"))
  #browser()
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      problems_info = problems_info(researcher_email),
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      logo = "www_psyquest/images/longgold_logo_transparent.png",
      logo_width = "100px",
      logo_height = "auto"
    )
  )
}
#' CBQ Standalone
#'
#' This function launches a standalone testing session for the CBQ questionnaire.
#' CBQ stands for 'Children's Behavior Questionnaire'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), \code{"de"} (German), \code{"de_f"} (German (formal), and \code{"ar"} (arabic).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
CBQ_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "CBQ", languages = languages, ...)

#' BFT Standalone
#'
#' This function launches a standalone testing session for the BFT questionnaire.
#' BFT stands for 'Big Five Inventory 10-items'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), \code{"de"} (German) and \code{"de_d"} (formal German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
BFT_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "BFT", languages = languages, ...)

#' BMR Standalone
#'
#' This function launches a standalone testing session for the BMR questionnaire.
#' BMR stands for 'Barcelona Music Rewards Questionnaire'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"it"} (Italian).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
BMR_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "BMR", languages = languages, ...)

#' BTQ Standalone
#'
#' This function launches a standalone testing session for the BFT questionnaire.
#' BFT stands for 'Bedtime Questionnaire'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), \code{"de"} (German) and \code{"de_d"} (formal German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
BTQ_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "BTQ", languages = languages, ...)

#' CCM Standalone
#'
#' This function launches a standalone testing session for the CCM questionnaire.
#' CCM stands for 'Concurrent Musical Activities'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}, and \code{"Extra"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
CCM_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "CCM",
               languages = languages,
               subscales = subscales,
               ...)
#' CHD Standalone
#'
#' This function launches a standalone testing session for the CCM questionnaire.
#' CHD stands for 'Children Demographics'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de", "de_f"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}, and \code{"Extra"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
CHD_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "CHD",
               languages = languages,
               subscales = subscales,
               ...)

#' CMS Standalone
#'
#' This function launches a standalone testing session for the CMS questionnaire.
#' CMS stands for 'Children  Musicality Questionnaire'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English),  \code{"de_f"} (German), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
CMS_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "CMS", languages = languages, ...)


#' CMT Standalone
#'
#' This function launches a standalone testing session for the CMT questionnaire.
#' CMT stands for 'Competence focus in Music Teaching'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
CMT_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "CMT", languages = languages, ...)

#' CRT Standalone
#'
#' This function launches a standalone testing session for the CRT questionnaire.
#' CRT stands for 'Cognitive Reflection Test'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"}/\code{"de_f"} (informal/formal German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
CRT_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "CRT", languages = languages, ...)

#' DAC Standalone
#'
#' This function launches a standalone testing session for the DAC questionnaire.
#' DAC stands for 'Drama Activity'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
DAC_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "DAC", languages = languages, ...)

#' DEG Standalone
#'
#' This function launches a standalone testing session for the DEG questionnaire.
#' DEG stands for 'Demographics'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are "Best Shot", "Hearing Impairment", "Type of Hearing Impairment", "Gender", "Age", "Nationality", "Country Formative Years", "First Language", "Second Language", and "Handedness".S4methods
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
DEG_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "DEG",
               languages = languages,
               subscales = subscales,
               ...)

#' EWE Standalone
#'
#' This function launches a standalone testing session for the EWE questionnaire.
#' EWE stands for 'Demographics'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are "Best Shot", "Hearing Impairment", "Type of Hearing Impairment", "Gender", "Age", "Nationality", "Country Formative Years", "First Language", "Second Language", and "Handedness".S4methods
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
EWE_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "EWE",
               languages = languages,
               ...)

#' FSS Standalone
#'
#' This function launches a standalone testing session for the FSS questionnaire.
#' FSS stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Challenge-skill balance"}, \code{"Action-awareness merging"},
#' \code{"Clear goals"}, \code{"Unambiguous feedback"}, \code{"Concentration on task at hand"},
#' \code{"Paradox of control"}, \code{"Loss of self-consciousness"}, \code{"Transformation of time"},
#' and \code{"Autotelic experience"}.
#' @param ... Further arguments to be passed to \code{\link{FSS}()}.
#'
#' @export
FSS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "FSS",
               languages = languages,
               subscales = subscales,
               ...)

#' FSR Standalone
#'
#' This function launches a standalone testing session for the FSS questionnaire.
#' FSR stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Absorption"}, \code{"Fluency of performance"}, \code{"Demands"},
#' \code{"Skills"}, \code{"Demand Fit"}
#' and \code{"Importance"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{FSR}()}.
#'
#' @export
FSR_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "FSR",
               languages = languages,
               subscales = subscales,
               ...)

#' GDS Standalone
#'
#' This function launches a standalone testing session for the GDS questionnaire.
#' GDS stands for 'Goldsmiths Dance Sophistication Index'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages only includes \code{"en"} (English).
#'
#' @param subscales (Character vector) There are 4 subscales, in general subscale and 6 items of 'dance experience observations' to be included in the questionnaire.
#' Possible subscales are \code{"Body Awareness"}, \code{"Social Dancing"}, \code{"Urge to Dance"}, \code{"Dance Training"}, \code{"General"}, and \code{"Observational Dance Experience"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
GDS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(
      label = "GDS",
      languages = languages,
      subscales = subscales,
      ...
    )

#' GMS Standalone
#'
#' This function launches a standalone testing session for the GMS questionnaire.
#' GMS stands for 'Goldsmiths Musical Sophistication Index'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Abilities"}, \code{"Absolute Pitch"}, \code{"Active Engagement"}, \code{"Emotions"}, \code{"General"}, \code{"Instrument"}, \code{"Musical Training"}, \code{"Perceptual Abilities"}, \code{"Singing Abilities"}, and \code{"Start Age"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' Overrides the \code{"short_version"} argument.
#' Overridden by the \code{configuration_filepath} argument.
#'
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#' Overridden by the \code{configuration_filepath} and \code{"subscales"} arguments.
#'
#' @param configuration_filepath (Character scalar) Optional path to a configuration file exported from the \href{https://shiny.gold-msi.org/gmsiconfigurator}{GMSI-Configurator}.
#' Overrides the \code{short_version} and \code{subscales} arguments.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
GMS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           short_version = FALSE,
           configuration_filepath = NULL,
           ...)
    standalone(
      label = "GMS",
      languages = languages,
      subscales = subscales,
      short_version = short_version,
      configuration_filepath = configuration_filepath,
      ...
    )

#' GRT Standalone
#'
#' This function launches a standalone testing session for the GRT questionnaire.
#' GRT stands for 'Short Grit Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
GRT_standalone <- function(languages = psyquest::languages(), ...)
  standalone(label = "GRT", languages = languages, ...)

#' HOP Standalone
#'
#' This function launches a standalone testing session for the HOP questionnaire.
#' HOP stands for 'Children’s Hope Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
HOP_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "HOP", languages = languages, ...)

#' HSP Standalone
#'
#' This function launches a standalone testing session for the HSP questionnaire.
#' HSP stands for 'Highly Sensitive Personality Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"it"} (Italian).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
HSP_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "HSP", languages = languages, ...)

#' HUM Standalone
#'
#' This function launches a standalone testing session for the HUM questionnaire.
#' HUM stands for 'Healthy-Unhealthy Music Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"it"} (Italian).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
HUM_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "HUM", languages = languages, ...)

#' IBQ Standalone
#'
#' This function launches a standalone testing session for the CBQ questionnaire.
#' IBQ stands for 'Children's Behavior Questionnaire'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), \code{"de"} (German), \code{"de_f"} (German (formal), and \code{"ar"} (arabic).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
IBQ_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "IBQ", languages = languages, ...)

#' IMI Standalone
#'
#' This function launches a standalone testing session for the IMI questionnaire.
#' IMI stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Goals Choice"}, and \code{"Theory of Inteligence"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
IMI_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "IMI",
               languages = languages,
               subscales = subscales,
               ...)
#' JIC Standalone
#'
#' This function launches a standalone testing session for the JIC questionnaire.
#' JIC stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
JIC_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "JIC",
               languages = languages,
               subscales = subscales,
               ...)

#' JIW Standalone
#'
#' This function launches a standalone testing session for the JIW questionnaire.
#' JIW stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}, \code{"Talent"}, \code{"Enjoyment"}, \code{"Competition"}, \code{"Confidence"}
#'  and \code{"Growth"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
JIW_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "JIW",
               languages = languages,
               subscales = subscales,
               ...)


#' LLQ Standalone
#'
#' This function launches a standalone testing session for the LLQ questionnaire.
#' LLQ stands for 'Lullaby Questionnaire'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are "Feeling Listening, Feeling Singing, Sleeping Aids, Calmess, Rocking Rhythm, Lullaby Themes, Transmission, Lullabies Childhood, Lullaby Singer, Tradition Importance, Same Lullabies, and Lullabies
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
LLQ_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "LLQ",
               languages = languages,
               ...)
#' LON Standalone
#'
#' This function launches a standalone testing session for the LON questionnaire.
#' LON stands for 'Drama Activity'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
LON_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "LON", languages = languages, ...)

#' MDS Standalone
#'
#' This function launches a standalone testing session for the HUM questionnaire.
#' MDS stands for 'Music Dislikes Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MDS_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MDS", languages = languages, ...)

#' MES Standalone
#'
#' This function launches a standalone testing session for the MES questionnaire.
#' MES stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MES_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "MES",
               languages = languages,
               subscales = subscales,
               ...)


#' MET Standalone
#'
#' This function launches a standalone testing session for the HUM questionnaire.
#' MET stands for 'Music Engagement Test'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MET_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MET", languages = languages, ...)


#' MHE Standalone
#'
#' This function launches a standalone testing session for the MHE questionnaire.
#' MHE stands for 'Musical Home Environment'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MHE_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MHE", languages = languages, ...)

#' MHI Standalone
#'
#' This function launches a standalone testing session for the MHI questionnaire.
#' MHI stands for 'Musical Home Environment for Infants'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English),  \code{"de"} (German),  and \code{"de_f"} (formal German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MHI_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MHI", languages = languages, ...)

#' MHP Standalone
#'
#' This function launches a standalone testing session for the MHI questionnaire.
#' MHI stands for 'Musical Home Environment for Preschoolers'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English),  \code{"de"} (German),  and \code{"de_f"} (formal German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MHP_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MHP", languages = languages, ...)

#' MPA Standalone
#'
#' This function launches a standalone testing session for the MPA questionnaire.
#' MPA stands for 'Music Performance Anxiety Test'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), \code{"lv"} (Latvian), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MPA_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MPA", languages = languages, ...)

#' MUS Standalone
#'
#' This function launches a standalone testing session for the MUSIC questionnaire.
#' MUS stands for 'Musical Preferences: A five-factor model (MUSIC)'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' There is a set of posible subscales (MUSIC): \code{"Mellow"}, \code{"Unpretentious"}, \code{"Sophisticated"},
#' \code{"Intense"}, and \code{"Contemporary"}.
#'
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
MUS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "MUS",
               languages = languages,
               subscales = subscales,
               ...)

#' PAC Standalone
#'
#' This function launches a standalone testing session for the PAC questionnaire.
#' PAC stands for 'Physical Activity'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
PAC_standalone <-
  function(languages = psyquest::languages(), exclude_dance = FALSE, ...)
    standalone(label = "PAC", languages = languages, exclude_dance = exclude_dance,  ...)

#' PHT Standalone
#'
#' This function launches a standalone testing session for the CBQ questionnaire.
#' PHT stands for 'Musical Phenotype'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
PHT_standalone <-
  function(languages = psyquest::languages(),
           subscales = c(),
           short_version = FALSE,
           randomize = FALSE,
           ...)
    standalone(label = "PHT",
               languages = languages,
               subscales = subscales,
               short_version = short_version,
               randomize = randomize,
               ...)

#' PMS Standalone
#'
#' This function launches a standalone testing session for the PMS questionnaire.
#' PMS stands for 'Profile of Mood States'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"it"} (Italian).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
PMS_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "PMS", languages = languages, ...)

#' QHC Standalone
#'
#' This function launches a standalone testing session for the QHC questionnaire.
#' QHC stands for 'Quick Hearing Check'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
QHC_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "QHC", languages = languages, ...)


#' SCA Standalone
#'
#' This function launches a standalone testing session for the SCA questionnaire.
#' SCA stands for 'Academic Self-Concept'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SCA_standalone <-
  function(languages = psyquest::languages(),
           short_version = FALSE,
           ...)
    standalone(label = "SCA",
               languages = languages,
               short_version = short_version,
               ...)

#' SCS Standalone
#'
#' This function launches a standalone testing session for the SCS questionnaire.
#' SCS stands for 'Social Self-Concept'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SCS_standalone <-
  function(languages = psyquest::languages(),
           short_version = FALSE,
           ...)
    standalone(label = "SCS",
               languages = languages,
               short_version = short_version,
               ...)

#' SDQ Standalone
#'
#' This function launches a standalone testing session for the SDQ questionnaire.
#' SDQ stands for 'Strengths and Difficulties Questionnaire (mental health)'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Conduct problems"}, \code{"Difficulties"}, \code{"Emotional problems"}, \code{"Externalising"}, \code{"Hyperactivity"}, \code{"Internalising"}, \code{"Peer problems"}, and \code{"Prosocial"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SDQ_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SDQ",
               languages = languages,
               subscales = subscales,
               ...)

#' SEM Standalone
#'
#' This function launches a standalone testing session for the SEM questionnaire.
#' SEM stands for 'School Engagement Measurement'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Attentiveness"}, \code{"Behavioral Engagement"}, \code{"Cognitive Strategy Use"}, \code{"Education"}, \code{"Emotional Engagement"}, \code{"School belonging"}, \code{"School Compliance"}, \code{"Self-regulated Learning"}, and \code{"Valuing of School Education"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SEM_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SEM",
               languages = languages,
               subscales = subscales,
               ...)

#' SES Standalone
#'
#' This function launches a standalone testing session for the SES questionnaire.
#' SES stands for 'Socio-economic Status'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Educational Degree"}, and \code{"ESeC"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SES_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SES",
               languages = languages,
               subscales = subscales,
               ...)

#' SMP Standalone
#'
#' This function launches a standalone testing session for the STOMP questionnaire.
#' SMP stands for 'Short Test of Musical Preferences (STOMP)'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' There are two possible subscale sets: 1. From the Do-re-mi paper: \code{"Reflective & Complex"},
#' \code{"Intense & Rebellious"}, \code{"Upbeat & Conventional"}, and \code{"Energetic & Rhythmic"};
#' 2. from the STOMP-R paper (MUSIC): \code{"Mellow"}, \code{"Unpretentious"}, \code{"Sophisticated"},
#' \code{"Intense"}, and \code{"Contemporary"}.
#'
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#'@param short_version (Scalar boolean) For the short version (original STOMP)
#'of the questionnaire set this to TRUE. Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SMP_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           short_version = FALSE,
           ...)
    standalone(label = "SMP",
               languages = languages,
               subscales = subscales,
               short_version = short_version,
               ...)

#' SOS Standalone
#'
#' This function launches a standalone testing session for the SOS questionnaire.
#' SOS stands for 'Student Opinion Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Effort"}, and \code{"Importance"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SOS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SOS",
               languages = languages,
               subscales = subscales,
               ...)
#' SWL Standalone
#'
#' This function launches a standalone testing session for the HOP questionnaire.
#' SWL stands for 'Satisfaction with Life Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), \code{"de"} (German), \code{"de_f"} (Formal German) and \code{"lv"} (Latvian).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SWL_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "SWL", languages = languages, ...)


#' SWB Standalone
#'
#' This function launches a standalone testing session for the SWB questionnaire.
#' SWB stands for 'Subjective Wellbeing Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
SWB_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "SWB", languages = languages, ...)

#' TOI Standalone
#'
#' This function launches a standalone testing session for the TOI questionnaire.
#' TOI stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Goals Choice"}, and \code{"Theory of Inteligence"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
TOI_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "TOI",
               languages = languages,
               subscales = subscales,
               ...)

#' TOM Standalone
#'
#' This function launches a standalone testing session for the TOM questionnaire.
#' TOM stands for 'Theory of Musicality'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Entity"}, \code{"Gift"}, \code{"Improvement"}, \code{"Incremental"}, \code{"Learning"}, and \code{"Stable"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
TOM_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "TOM",
               languages = languages,
               subscales = subscales,
               ...)

#' TPI Standalone
#'
#' This function launches a standalone testing session for the TPI questionnaire.
#' TPI stands for 'Ten Item Personality Inventory'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Agreeableness"}, \code{"Conscientiousness"}, \code{"Emotional Stability"}, \code{"Extraversion"}, and \code{"Openness to Experiences"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
TPI_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "TPI",
               languages = languages,
               subscales = subscales,
               ...)
