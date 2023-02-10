###
### Name: parameters_from_raventable
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: 
###


###
### Library
###
library(tidyverse)
library(here)
library(warbleR)
library(tuneR)
library(soundgen)

mutate <- dplyr::mutate
group_by <- dplyr::group_by
here <- here::here

###
### Main function here
###
parameters_from_raventable <- function(selection_path){
  
  # make a list of selection tables that need to work on
  selection_list <- list.files(selection_path, pattern = ".txt$", recursive = TRUE)
  
  
  data_sp_song_all <- NULL
  for (i in selection_list) {
    try({
      # read in each individual table
      data <- read_delim(paste0(selection_path, "/", i))
      
      # make the columns fit the standard selection table
      data_clean <- data %>%
        dplyr::rename(selec = Selection,
               channel = Channel,
               start = "Begin Time (s)",
               end = "End Time (s)",
               bottom.freq = "Low Freq (Hz)",
               top.freq = "High Freq (Hz)") %>%
        mutate(sound.files = paste0(str_extract(i, pattern = ".+(?=_se)"), ".WAV"), 
               selec.file = i,
               bottom.freq = bottom.freq/1000,
               top.freq = top.freq/1000) 
      
      # transfer the data frame to standard selection table
      selection_table <- selection_table(data_clean, 
                                         path = selection_path) 
      
      # extract spectrogram parameters from syllables
      data_sp_syllable <- selection_table %>%
        spectro_analysis(path = selection_path,
                         fast = FALSE) %>%
        left_join(data_clean, by = c("sound.files", "selec")) %>%
        mutate(song = str_extract(Annotation, pattern = "[:digit:]+"),
               syllable = str_extract(Annotation, pattern = "(?<=-)[:digit:]+")) %>%
        as_tibble()
      
      # extract spectrogram parameters from songs (from syllables dataframe)
      data_sp_song <- data_sp_syllable %>%
        group_by(sound.files, song) %>%
        mutate(sl = max(end) - min(start),
               i1 = start[which(syllable == 3)] - start[which(syllable == 2)],
               i2 = start[which(syllable == 5)] - start[which(syllable == 4)],
               i3 = start[which(syllable == 7)] - start[which(syllable == 6)],
               min_d = min(mindom),
               max_d = max(maxdom)) %>%
        ungroup() %>%
        select(sound.files, song, syllable,
               sl, i1, i2, i3, min_d, max_d,
               duration, dfrange, meandom) %>%
        pivot_wider(names_from = syllable, values_from = c(duration, dfrange, meandom)) 
      
      # collect the song parameters
      data_sp_song_all <- rbind(data_sp_song_all, data_sp_song)
    })
  }

return(data_sp_song_all)
}


###
### Example code
###

#final <- parameters_from_raventable(selection_path = here("Ch2_owl_individual", "data", "BAOW_recordings"))
#write_csv(final, here("Ch2_owl_individual", "data", "barred_owl_recordings_list_parameters_2.csv"))







