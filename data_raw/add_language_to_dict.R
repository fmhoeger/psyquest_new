library(tidyverse)
messagef <- function(...) message(sprintf(...))

add_languages_to_dict <- function(source_dict_dir,
                                  original_dict_dir,
                                  output_dir,
                                  languages = "it",
                                  over_write = T,
                                  key_name = "key",
                                  dry_run = T,
                                  source_file_type = ".csv"){
  dict_files <- list.files(source_dict_dir,
                           pattern = source_file_type,
                           full.names = TRUE)
  languages <- tolower(languages)
  bad_files <- c()
  browser()
  dicts <-
    map(dict_files, function(filepath) {
      #dict file must be UTF8 encoded!
      messagef("Reading %s", basename(filepath))
      if(source_file_type == ".csv"){
        browser()
        source <- read.csv(filepath,
                           sep = ";",
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           fileEncoding = "utf8") %>%
          as_tibble()
      }
      else{
        source <- readxl::read_xlsx(filepath)
      }
      source <- source %>% set_names(tolower(names(.)))

      if(!(tolower(key_name) %in% names(source))){
        messagef("Dictionary '%s' does not have 'key' column, Skipping.", basename(filepath))
        bad_files <<- c(bad_files, filepath)
        return(NULL)
      }

      browser()
      effective_languages <- intersect(languages, names(source))
      if(length(effective_languages) == 0){
        messagef("Dictionary '%s' does not contain any of the specified languages '%s'.  Skipping.",
                 basename(filepath),
                 paste(languages, collapse = ","))
        bad_files <<- c(bad_files, filepath)
        return(NULL)
      }

      browser()
      fname <- file.path(original_dict_dir,
                         sprintf("%s.csv",
                                 tools::file_path_sans_ext(basename(filepath))))

      message(sprintf("Try reading orignal dictionary '%s'...", basename(fname)))
      if(!file.exists(fname)){
        messagef("...failed (file missing). Skipping.")
        #tmp <- tmp %>% mutate(!!language := en)
        bad_files <<- c(bad_files, fname)
        return(NULL)
      }
      original <- read.csv(fname,
                           sep = ";",
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           fileEncoding = "utf8") %>%
        as_tibble()
      if(nrow(original) == 0){
        messagef("...failed (empty). Skipping.")
        # tmp <- tmp %>% mutate(!!language := en)
        bad_files <<- c(bad_files, fname)
        return(NULL)
      }
      if(any(effective_languages %in% names(original))){
        if(over_write){
          messagef("Warning: overwriting languages %s in original.",
                   paste(intersect(effective_languages, names(original)), collapse = ", "))
          original <- original %>% select(-all_of(effective_languages))
        }
        else{
          messagef("Found overlapping Languages '%s' in original. Skipping.",
                   paste(intersect(effective_languages, names(original)), collapse = ", "))
          bad_files <<- c(bad_files, fname)
          return(NULL)
        }
      }
      tmp <- original %>% left_join(source %>% select(key, all_of(effective_languages)), by = "key")

      if(nrow(original) != nrow(tmp)){
          messagef("Keys do not match in %s. Skipping.", basename(filepath))
          bad_files <<- c(bad_files, fname)
          return(NULL)
        }
      message(sprintf("-> Successfully added '%s' to  %s", paste(languages, collapse = ", "), basename(filepath)))
      return(tmp)

      tmp
  })
  browser()

  names(dicts) <- basename(dict_files)
  map(names(dicts), function(fname){
    browser()
    tmp_name <- sprintf("%s.csv", basename(fname) %>% tools::file_path_sans_ext())
    message(sprintf("Writing '%s' to  %s (dry run = %s)", tmp_name, output_dir, dry_run))
    if(!dry_run){
      if(!is.null(dicts[[fname]])){
        write_csv2(dicts[[fname]],
                   file.path(output_dir, tmp_name))
        # write.table(dicts[[fname]],
        #             file.path(output_dir, tmp_name), sep = ";",
        #             row.names = F,
        #             quote = T,
        #             col.names = T,
        #             fileEncoding = "utf-8")
      }
      else{
        browser()
      }
    }

  })
  ret <- ifelse(length(bad_files), basename(bad_files), "All OK.")
  invisible(ret)
}

extract_language_col <- function(excel_file, language){
  source <- readxl::read_excel(excel_file) %>%
    set_names(str_remove_all(names(.), '"')) %>%
    select(key, all_of(language))
  source
}

shift_prompts <- function(dict_file, offset = 1, exclude_numbers = c(0), rewrite = F){
  tmp <- readr::read_csv2(dict_file, col_types = cols())
  keys <- tmp$key
  numbers <- str_extract(keys, "[0-9]+") %>% as.integer()
  excludes <- sort(which(numbers %in% sort(exclude_numbers)))
  keep_keys <- keys[excludes]
  browser()
  key_format_str <- str_replace(keys, "[0-9]+", "%04d")
  ret <- sprintf(key_format_str, numbers + offset)
  ret[excludes] <- keep_keys
  if(rewrite){
    new_dict <- tmp
    new_dict$key <- ret
    tmp_name <- sprintf("%s_shifted.%s", tools::file_path_sans_ext(dict_file), tools::file_ext(dict_file))
    write_csv2(new_dict, tmp_name)
  }
  ret
}

excel_dict_to_csv <- function(excel_file, csv_file, cols = NULL, sep = ",", quote = T){
  browser()
  source <- readxl::read_xlsx(excel_file) %>%
    set_names(str_remove_all(names(.), '"')) %>%
    na.omit()

  if(is.null(cols)){
    cols <- names(source)
  }

  eff_cols <- intersect(names(source), cols)

  if(length(eff_cols) == 0){
    stop("All cols not found.")
  }

  source <- source %>% select(all_of(cols))

  if(basename(csv_file) == csv_file){
    csv_file <- file.path(dirname(excel_file), csv_file)
  }

  write.table(source,
              csv_file,
              sep = sep,
              row.names = F,
              quote = quote,
              col.names = T,
              fileEncoding = "utf8")

}
