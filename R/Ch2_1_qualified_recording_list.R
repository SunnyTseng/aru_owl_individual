###
### Name: qualified_recording_list
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: sections_min, the minimum detections a recording must include to be qualified
### Output: a tidy dataframe with recordings fit the requirement
###

qualified_recording_list <- function(sections_min){
  
  # import data
  data_2021 <- read_csv(here("data", "processed", "2021_owl_BirdNET.csv")) %>%
    filter(common_name == "Barred Owl")
  
  data_2022 <- read_csv(here("data", "processed", "2022_owl_BirdNET.csv")) %>%
    filter(common_name == "Barred Owl")
  
  # select qualified recordings and save as a list
  recordings_2021 <- data_2021 %>%
    group_by(site, recording) %>%
    mutate(sections = n(),
           start = min(start_s), 
           end = max(end_s)) %>%
    filter(sections >= sections_min) %>%
    group_by(year, month, day, season, site, recording, sections, start, end) %>%
    summarize()
  
  recordings_2022 <- data_2022 %>%
    group_by(site, recording) %>%
    mutate(sections = n(),
           start = min(start_s), 
           end = max(end_s)) %>%
    filter(sections >= sections_min) %>%
    group_by(year, month, day, season, site, recording, sections, start, end) %>%
    summarize()
  
  # combine the datasets into one
  final <- rbind(recordings_2021, recordings_2022)

return(final)
}


###
### Example code
###

#final <- qualified_recording_list(sections_min = 5) # each recording has at least 5 detections
#write_csv(final, here("Ch2_owl_individual", 
#                      "data",
#                      "barred_owl_recordings_list.csv"))



