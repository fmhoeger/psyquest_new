library(tidyverse)

psyquest_item_bank_raw <-
  map_dfr(list.files("./data_raw/item_banks", full.names = TRUE), function(filepath) {
     if(grepl("BTQ", filepath)){
      #browser()
     }

    read.csv(filepath, sep = ";", stringsAsFactors = FALSE, header = TRUE)
  })

psyquest_item_bank <-
  psyquest_item_bank_raw %>%
  as_tibble() %>%
  filter(str_detect(language, "en"), nzchar(score_func)) %>%
  mutate(q_id = substr(main_id, 2, 4),
         subscales = str_replace(subscales, "; ", ";")) %>%
  group_by(q_id) %>%
  mutate(item_id = 1:n()) %>%
  ungroup() %>%
  select(q_id, item_id, prompt_id = main_id, option_type = template, score_func, subscales, layout, audio_file, short_version)

#hack for DEG
psyquest_item_bank[psyquest_item_bank$q_id == "DEG" & psyquest_item_bank$item_id >= 6 & psyquest_item_bank$item_id <= 12,]$item_id <- psyquest_item_bank[psyquest_item_bank$q_id == "DEG" & psyquest_item_bank$item_id >= 6 & psyquest_item_bank$item_id <= 12,]$item_id - 1

usethis::use_data(psyquest_item_bank, overwrite = TRUE)
