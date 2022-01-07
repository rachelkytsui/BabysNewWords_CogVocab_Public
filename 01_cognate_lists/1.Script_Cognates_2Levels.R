######### Creating 2 levels of cognates: Cognates & Non-cognates
library(tidyverse)
library(here)

### Read in the TE list
TE_List <- read.csv(here("cognate_lists/original_Translation_Equivalents_List.csv")) %>%
  # Filter out non-TEs
  filter_at(vars(Item_Number,Item_Number.1), all_vars(!is.na(.))) %>%
  # Rename variables
  rename(english_word_full = English,
         french_word_full = French,
         english_item_id = Item_Number,
         french_item_id = Item_Number.1) %>%
  # Rename typos
  mutate(french_word_full = case_when(french_word_full=="bebe" ~ "bébé",
                                      french_word_full=="baton" ~ "bâton",
                                      french_word_full=="boite" ~ "boîte",
                                      french_word_full=="camera" ~ "caméra",
                                      french_word_full=="cereale" ~ "céréales",
                                      french_word_full=="elephant" ~ "éléphant",
                                      french_word_full=="helicoptere" ~ "hélicoptère",
                                      french_word_full=="creme_glacee" ~ "crème_glacée",
                                      french_word_full=="cles" ~ "clés",
                                      french_word_full=="medicament" ~ "médicament",
                                      french_word_full=="chaise_bercante" ~ "chaise_berçante",
                                      french_word_full=="telephone" ~ "téléphone",
                                      french_word_full=="brosse_a_dent" ~ "brosse_à dent",
                                      french_word_full=="television" ~ "télévision",
                                      TRUE ~ as.character(french_word_full)))

### Read in the original CDI_Cognate list
CognateList <- read.csv(here("cognate_lists/original_CogSim_ranked_words.csv")) %>%
  # Replace space between two words with an underscore in the english_word_full and french_word_full variables
  mutate(english_word_full = str_replace(english_word_full, " ", "_"),
         french_word_full = str_replace(french_word_full, " ", "_"),
         french_word_full = str_replace(french_word_full, "-", "_")) %>%
  # Rename some rows to match the TE_List
  mutate(english_word_full = case_when(english_word_full=="booboo" ~ "owie_boo_boo", 
                                       english_word_full=="short" ~ "shorts",
                                       english_word_full=="carrot" ~ "carrots",
                                       english_word_full=="pajamas" ~ "pyjamas",
                                       TRUE ~ as.character(english_word_full)),
         french_word_full = case_when(french_word_full=="piquenique" ~ "pique_nique",
                                      french_word_full=="au" ~ "a_au",
                                      french_word_full=="brosser" & word_pairs=="brush - brosse" ~ "brosse",
                                      french_word_full=="crayon_de couleur" ~ "crayon_de_couleur",
                                      french_word_full=="un_autre" & english_word_full=="other" ~ "autre",
                                      french_word_full=="pantalon" ~ "pantalons",
                                      TRUE ~ as.character(french_word_full)))
    
### Merge the TE and Cognate list
TE_Cognate_List <- TE_List %>%
  full_join(CognateList, by = c("english_word_full", "french_word_full")) %>%
  # Filter out items from the CognateList that doesn't match any items from the TE_List
  filter_at(vars(english_item_id, french_item_id), all_vars(!is.na(.))) %>%
  # Create a new column to indicate the cognate status (cognates vs. non-cognate)
  mutate(cognate_status = case_when(rank > 1 ~ "cognate",
                                    TRUE ~ "non-cognate")) %>%
  # Some other words should also be cognate but were missing 
  # because of an error in creating the perceptual similarity ranking experimental program,
  # so we are changing their cognate status to "cognate" here
  mutate(cognate_status = case_when(english_word_full=="choo_choo"&french_word_full=="tchou_tchou" ~ "cognate",
                                    english_word_full=="grr"&french_word_full=="grr" ~ "cognate",
                                    english_word_full=="meow"&french_word_full=="miaou" ~ "cognate",
                                    english_word_full=="moo"&french_word_full=="meuh" ~ "cognate",
                                    english_word_full=="vroom"&french_word_full=="vroum" ~ "cognate",
                                    english_word_full=="woof_woof"&french_word_full=="wouf_wouf" ~ "cognate",
                                    english_word_full=="yum_yum"&french_word_full=="miam_miam" ~ "cognate",
                                    english_word_full=="uh_oh"&french_word_full=="oh_oh" ~ "cognate",
                                    english_word_full=="quack_quack"&french_word_full=="coin_coin" ~ "cognate",
                                    english_word_full=="baa_baa"&french_word_full=="beee_beee" ~ "cognate",
                                    english_word_full=="pizza"&french_word_full=="pizza" ~ "cognate",
                                    english_word_full=="ball"&french_word_full=="balle" ~ "cognate",
                                    english_word_full=="balloon"&french_word_full=="ballon" ~ "cognate",
                                    # english_word_full=="coffee"&french_word_full=="cafe" ~ "cognate",
                                    english_word_full=="toast"&french_word_full=="toast" ~ "cognate",
                                    english_word_full=="penis"&french_word_full=="penis" ~ "cognate",
                                   ## english_word_full=="potty"&french_word_full=="pot" ~ "cognate",
                                    TRUE ~ as.character(cognate_status))) %>%
  # Unite "english_word_full" and "french_word_full" to create "word_pairs" variable
  unite(word_pairs, c(english_word_full, french_word_full), sep = " - ", remove = FALSE)


### Count how many cognates & non-cognates we have
TE_Cognate_List %>%
  count(cognate_status)

### Save the TE_Cognate_List
write.csv(TE_Cognate_List, "CognateList_full_611TE.csv", row.names=FALSE)




### Divide the original list into 2 levels based on the rank similarity rating 
#CognateList_2levels <- CognateList %>%
  ## Break the rank variables into 3 equal-sized groups
  #mutate(Levels = cut(rank, breaks = 2, labels = c("Far-cognate", "Close-cognate"))) %>%
  ## Shift rank 88 from the "Close-cognate" group to the "Far-cognate" group)
  #mutate(Levels = case_when(rank == 88 ~ "Far-cognate",
  #                          TRUE ~ as.character(Levels))) %>%
  #mutate(Levels = as.factor(Levels))

### Save as new .csv file
#write.csv(CognateList_2levels, "Cognates_2Levels.csv")