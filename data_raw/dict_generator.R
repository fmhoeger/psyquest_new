library(tidyverse)

psyquest_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/",  full.names = TRUE), function(filepath) {
    #dict file must be UTF8 encoded!
    print(filepath)
    # if(str_detect(filepath, "CBQ")){
    #   browser()
    # }
    #tmp <- read.table(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "utf8")
    tmp <- readr::read_csv2(filepath, col_types = cols())
    #browser()
    if(nrow(problems(tmp)) > 0){
      browser()
      print(problems(tmp))
    }
    if(!("de" %in% names(tmp))){
      tmp <- tmp  %>% mutate(de = en)
    }
    if(!("de_f" %in% names(tmp))){
      tmp <- tmp  %>% mutate(de_f = de)
    }
    if(!("it" %in% names(tmp))){
      tmp <- tmp  %>% mutate(it = en)
    }
    if(!("es" %in% names(tmp))){
      tmp <- tmp  %>% mutate(es = en)
    }
    if(!("lv" %in% names(tmp))){
      #print(tmp$key[1])
      tmp <- tmp  %>% mutate(lv = en)
    }

    tmp %>% filter(nchar(de) != 0, nchar(en) != 0)
  })

psyquest_dict <- psychTestR::i18n_dict$new(psyquest_dict_raw)
psyquest_dict_df <- psyquest_dict %>% as.data.frame()
usethis::use_data(psyquest_dict, overwrite = TRUE)
usethis::use_data(psyquest_dict_df, overwrite = TRUE)
