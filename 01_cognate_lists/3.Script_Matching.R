######### Matching the cognate and non-cognate lists by age of acquistion
library(tidyverse) 
library(here)
library(optmatch)

### Read in CognateList_nouns_AoA.csv & prepare for matching analysis (81 cognates)
CognateList_nouns_AoA <- read.csv(here("cognate_lists/CognateList_nouns_AoA.csv")) %>%
  #convert "cognate_status" to 0 or 1
  mutate(cognate_status = case_when(cognate_status == "cognate" ~ 1,
                                    cognate_status == "non-cognate" ~ 0))


### Matching1: Optmatch on exact age of acquisition in both English and French
### along with the closest match on word category,
### using pairmatch to find pairings that minimizes the sum of discrepancies
### (this creates 52 exact-matches)
matching <- pairmatch(glm(cognate_status ~ english_category, 
                          data=CognateList_nouns_AoA, family=binomial),
                      data=CognateList_nouns_AoA,
                      within=exactMatch(cognate_status ~ Eng_AoA + Fr_AoA,
                                        data=CognateList_nouns_AoA))

summary(matching)

# Merge the matching dataframe with the CognateList_nouns_AoA dataframe
cognate.exact.matched <- data.frame(CognateList_nouns_AoA, matches = matching, check.rows=TRUE) %>%
  filter(!is.na(matches)) %>%
  arrange(matches, desc(cognate_status)) %>%
  mutate(cognate_status = case_when(cognate_status == 1 ~ "cognate",
                                    cognate_status == 0 ~ "non-cognate")) %>%
  relocate(word_pairs, .before = english_item_id) %>%
  relocate(lexical_category, .before = english_category)


### Matching2: (29 cognates remaining from the previous exact match) 
### Optmatch on age of acquisition in both English and French,
### allowing a 1-month possible deviation in either English/French or both

# Create new dataset with cognates NOT matched in exactmatch that still need matching 
cognate.exact.NOTmatched <- data.frame(CognateList_nouns_AoA, matches = matching, check.rows=TRUE) %>%
  filter(is.na(matches)) %>%
  select(-matches)

# Matching
matching2 <- pairmatch(cognate_status ~ Eng_AoA + Fr_AoA, 
                       caliper=1, # this allows matches with a 1-month difference
                       data = cognate.exact.NOTmatched)

summary(matching2)

# Merge the matching2 dataframe with the cognate.exact.NOTmatched dataframe
cognate.close.matched <- data.frame(cognate.exact.NOTmatched, matches = matching2, check.rows=TRUE) %>%
  filter(!is.na(matches)) %>%
  arrange(matches, desc(cognate_status)) %>%
  mutate(cognate_status = case_when(cognate_status == 1 ~ "cognate",
                                    cognate_status == 0 ~ "non-cognate")) %>%
  relocate(word_pairs, .before = english_item_id) %>%
  relocate(lexical_category, .before = english_category)


### Merge the exact-matched and close-matched dataframes
cognates.final.matched <- cognate.exact.matched %>%
  bind_rows(cognate.close.matched)


### Save the matching dataframe
write.csv(cognates.final.matched, "CognateList_matching.csv", row.names = FALSE)


### T-tests comparing the age of acquisition between the matched samples

# Calculate the average AoA for both cognates and non-cognates
cognates.final.matched %>%
  group_by(cognate_status) %>%
  summarize(EngAoA_Mean = mean(Eng_AoA, na.rm=TRUE),
            EngAoA_SD = sd(Eng_AoA, na.rm=TRUE),
            FrAoA_Mean = mean(Fr_AoA, na.rm=TRUE),
            FrAoA_SD = sd(Fr_AoA, na.rm=TRUE))

# Boxplot visualization
library(ggpubr)
cognates.final.matched %>% ggboxplot(x = "cognate_status", 
                                     y = c("Eng_AoA", "Fr_AoA"),
                                     data = .,
                                     merge = TRUE,
                                     xlab = "Cognate status",
                                     ylab = "Months",
                                     title = "N = 83 cognates & 83 non-cognates")

# T-tests for Eng_AoA
cognates.final.matched %>% t.test(Eng_AoA ~ cognate_status,
                                  conf.level=.95, 
                                  paired = TRUE, 
                                  alternative = "two.sided", 
                                  data = .)

# T-tests for Fr_AoA
cognates.final.matched %>% t.test(Fr_AoA ~ cognate_status,
                                  conf.level=.95, 
                                  paired = TRUE, 
                                  alternative = "two.sided", 
                                  data = .)

