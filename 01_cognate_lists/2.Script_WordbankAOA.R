######### 1. Obtaining age of acquisition from Wordbank for each items across English and Quebce-French CDI
######### 2. Keeping only nouns in the Cognate list
library(tidyverse)
library(tidylog)
library(here)
library(wordbankr) #version 0.3.1 is used in this project 

### Read in CognateList_full_611TE.csv
CognateList_full <- read.csv(here("cognate_lists/CognateList_full_611TE.csv")) %>%
  mutate(english_word_full = str_replace(english_word_full, " ", ""))

### Read in data from Wordbank
## English
english_ws_items <- get_item_data("English (American)", "WS") %>%
  select(uni_lemma, definition, num_item_id, type, category, lexical_category, lexical_class) %>%
  rename_at(vars(-num_item_id), funs(paste0("english_", .))) %>%
  rename(english_item_id = num_item_id) %>%
  # Duplicate the item_id for checking consistency with the CognateList item_id
  mutate(english_item_id_CDI = english_item_id)
  
#english_ws_data <- get_instrument_data("English (American)", "WS")
#english_ws_admins <- get_administration_data("English (American)", "WS")

## French
french_ws_items <- get_item_data("French (Quebecois)", "WS") %>%
  select(uni_lemma, definition, num_item_id, type, category, lexical_category, lexical_class) %>%
  rename_at(vars(-num_item_id), funs(paste0("french_", .))) %>%
  rename(french_item_id = num_item_id) %>%
  # Duplicate the item_id for checking consistency with the CognateList item_id
  mutate(french_item_id_CDI = french_item_id)

#french_ws_data <- get_instrument_data("French (Quebecois)", "WS")
#french_ws_admins <- get_administration_data("French (Quebecois)", "WS")


## Check if item_id of the cognate list match the ones on the wordbankr lists
Check_EngItemID <- CognateList_full %>%
  mutate(english_item_id_CognateList = english_item_id) %>%
  left_join(english_ws_items, by = "english_item_id") %>%
  mutate(english_definition = str_replace(english_definition, " ", "_"),
         english_definition = str_replace(english_definition, "/", "_")) %>%
  select(english_item_id_CognateList, english_word_full, english_item_id_CDI, english_definition) %>%
  mutate(check = case_when(english_definition != english_word_full ~ 0, TRUE ~ 1)) %>%
  filter(check == 0)

Check_FrItemID <- CognateList_full %>%
  mutate(french_item_id_CognateList = french_item_id) %>%
  left_join(french_ws_items, by = "french_item_id") %>%
  mutate(french_definition = str_replace(french_definition, " ", "_"),
         french_definition = str_replace(french_definition, "/", "_")) %>%
  select(french_item_id_CognateList, french_word_full, french_item_id_CDI, french_definition) %>%
  mutate(check = case_when(french_definition != french_word_full ~ 0, TRUE ~ 1)) %>%
  filter(check == 0)

### Fix typo in english_item_id (*no typo found in french_item_id)
CognateList_full <- CognateList_full %>%
  mutate(english_item_id = case_when(english_word_full=="peas" ~ "131",
                                     english_word_full=="peanut_butter" ~ "132",
                                     english_word_full=="potato_chip" ~ "137",
                                     english_word_full=="potato" ~ "138",
                                     english_word_full=="finger" ~ "193",
                                     english_word_full=="feet" ~ "194",
                                     english_word_full=="toe" ~ "206",
                                     english_word_full=="tongue" ~ "207",
                                     english_word_full=="tooth" ~ "208",
                                     english_word_full=="bowl" ~ "214",
                                     english_word_full=="box" ~ "215",
                                     english_word_full=="rocking_chair" ~ "282",
                                     english_word_full=="refrigerator" ~ "283",
                                     english_word_full=="window" ~ "292",
                                     english_word_full=="washing_machine" ~ "293",
                                     english_word_full=="hello" ~ "384",
                                     english_word_full=="hi" ~ "385",
                                     english_word_full=="not" ~ "648",
                                     TRUE ~ as.character(english_item_id))) %>%
  mutate(english_item_id = as.numeric(english_item_id))

### Remove problematic items (i.e., items with one-to-many mappings)
CognateList_full_clean <- CognateList_full %>%
  # Remove one-to-many mappings
  filter(is.na(One_to_Many)) %>%
  select(-One_to_Many) %>%
  # Remove items that has more than one word in one language
  filter(!(french_word_full %in% c("alligator_crocodile", "ourson_nounours", 
                                   "nouilles_pates", "beurre_de_pinotte_d_arachide", 
                                   "perles_boules", "espadrille_soulier_de_course", 
                                   "petite culotte_culotte", "zip_fermeture_eclair", 
                                   "bedaine_ventre", "couverte_couverture", 
                                   "sacoche_sac_a_main", "mouchoir_kleenex", 
                                   "salle_de_bain_toilette", "bassinette_berceau", 
                                   "veranda_balcon", "frigidaire_frigo", 
                                   "evier_lavabo", "tuyau_boyau"))) %>%
  filter(!(english_word_full %in% c("buttocks_bottom", "owie_boo_boo",
                                    "ball", "balloon")))

### Save the cleaned full cognate list 
write.csv(CognateList_full_clean, "CognateList_full_clean.csv", row.names=F)

### Merge the CognateList with Wordbank to get info on lexical category
#CognateList_nouns <- CognateList_full_clean %>%
  ## Merging the CognateList with the Wordbank data
  #left_join(english_ws_items, by = "english_item_id") %>%
  #left_join(french_ws_items, by = "french_item_id") %>%
  #select(-c(english_item_id_CDI, french_item_id_CDI)) %>%
  ## Check if the lexical category does not match between English and French
  #mutate(CatMatch = case_when(english_category != french_category ~ 0, 
  #                            TRUE ~ 1),
  #       TypeMatch = case_when(english_type != french_type ~ 0, 
  #                             TRUE ~ 1),
  #       LexCatMatch = case_when(english_lexical_category != french_lexical_category ~ 0, 
  #                               TRUE ~ 1),
  #       LexClassMatch = case_when(english_lexical_class != french_lexical_class ~ 0, 
  #                                 TRUE ~ 1)) %>%
  #filter(CatMatch == 0)
  #mutate(check = case_when(english_uni_lemma != french_uni_lemma ~ 0, 
  #                         TRUE ~ 1)) %>%
  #filter(check == 0) %>%
  #select(english_uni_lemma, french_uni_lemma, english_word_full, french_word_full)

CognateList_nouns <- CognateList_full_clean %>%
  # Merging the CognateList with the Wordbank data
  left_join(english_ws_items, by = "english_item_id") %>%
  left_join(french_ws_items, by = "french_item_id") %>%
  # Removing unnecessary variables
  select(-c(english_item_id_CDI, french_item_id_CDI, english_uni_lemma, english_definition,
            french_uni_lemma, french_definition)) %>%
  # Keeping only one "type", "lexical_category" & "lexical_class" variable 
  # since they are identical between English & French
  select(-c(french_type, french_lexical_category, french_lexical_class)) %>%
  rename_at(vars(english_type, english_lexical_category, english_lexical_class), 
            funs(sub("english_", "", .))) %>%
  # Keeping only nouns
  filter(lexical_category == "nouns") %>%
  # Removing unnecessary variables
  select(-c(type, lexical_class)) 

CognateList_nouns %>%
  filter(cognate_status == "cognate") %>%
  count(english_category)

CognateList_nouns %>%
  filter(cognate_status == "non-cognate") %>%
  count(english_category)

### Save list as .csv file

write.csv(CognateList_nouns, "CognateList_nouns.csv", row.names = FALSE)


### Obtain Age of Acquisition info from Wordbank
##English nouns
EngNouns <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(lexical_category == "nouns")

eng_ws_data_nouns <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   items = EngNouns$item_id,
                                   administrations = TRUE,
                                   iteminfo = TRUE)

EngAoA <- fit_aoa(eng_ws_data_nouns, measure = "produces", method = "glm", proportion = 0.5) %>%
  rename(english_item_id = num_item_id,
         Eng_AoA = aoa) %>%
  select(english_item_id, Eng_AoA)


##French nouns
FrNouns <- get_item_data(language = "French (Quebecois)", form = "WS") %>%
  filter(lexical_category == "nouns")

fr_ws_data_nouns <- get_instrument_data(language = "French (Quebecois)",
                                         form = "WS",
                                         items = FrNouns$item_id,
                                         administrations = TRUE,
                                         iteminfo = TRUE)

FrAoA <- fit_aoa(fr_ws_data_nouns, measure = "produces", method = "glm", proportion = 0.5) %>%
  rename(french_item_id = num_item_id,
         Fr_AoA = aoa) %>%
  select(french_item_id, Fr_AoA)

##Merge with the CognateList_nouns data
CognateList_nouns_AoA <- CognateList_nouns %>%
  left_join(EngAoA, by = "english_item_id") %>%
  left_join(FrAoA, by = "french_item_id") %>%
  # filter out any item that doesn't have AoA info (NA)
  filter_at(vars(Eng_AoA:Fr_AoA), all_vars(!is.na(.)))


### Save list as .csv file
write.csv(CognateList_nouns_AoA, "CognateList_nouns_AoA.csv", row.names = FALSE)

