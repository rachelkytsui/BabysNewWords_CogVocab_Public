# BabysNewWords_CogVocab_Public

Repository overview: This repository holds the data and analysis script for the paper "Cognates are advantaged in early bilingual expressive vocabulary development".

## data cleaning scripts
* `01_cognate_lists`: These sets of scripts generated the cognate lists used in this study.
  + In our study, we have two cognate lists: (1) a complete list of 537 translation equivalents (131 cognates and 406 non-cognates) and (2) a matched list of 162 translation equivalents (81 cognates and 81 non-cognates) that were matched for part of speech, typical age of acquisition, and word category when possible. 
* `02_exclusion`: This documents the exclusion criteria used in this study.
  + This generates the "exclusions.csv" file in the "data_keepers" folder.

## data_raw
* `public_clean_cdi_eng.csv`: The raw English CDI data with all the administrations during August 2020 and May 2021.
* `public_clean_cdi_fr.csv`: The raw French CDI data with all the administrations during August 2020 and May 2021.

## data_keepers
* `exclusions.csv`: List of participants excluded from the final analysis due to failing to meet the study's age/language criteria.
* `public_clean_demog.csv`: The demographic information of the participants.
* `public_keepers_cognate_full.csv`: The final data merged with the complete cognate list (with 537 cognates and non-cognates).
* `public_keepers_cognate_matched.csv`: The final data merged with the matched cognate list (with 162 cognates and non-cognates).

## data_dictionary
* `data_dictionary.html`: Data dictionary with information about the data set used in the paper (i.e., public_keepers_cognate_full.csv & public_keepers_cognate_matched.csv).

## paper
This folder contains the .Rmd file which runs the analysis and knits the result into a .pdf file.
* `CogVocab_paper.Rmd`: The analysis script and content of the paper.
* `CogVocab_paper.pdf`: The final paper.
