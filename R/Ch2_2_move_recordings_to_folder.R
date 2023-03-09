###
### Name: move_recordings_to_folder
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: 
###


###
### Main function here
###
move_recordings_to_folder <- function(from, to){
  
  # import data
  data <- read_csv(here("Ch2_owl_individual", "data", "barred_owl_recordings_list.csv"))
  
  
  # copy recordings from one folder to another folder
  for(i in 1:nrow(data)){
    target_file <- paste0(from, "/",
                          paste0(data$year[i], "_owl/"),
                          data$site[i], "/",
                          data$recording[i], ".WAV")
    
    file.copy(target_file, to)
    
    # add site information in front
    file.rename(paste0(to, "/", paste0(data$recording[i], ".WAV")),
                paste0(to, "/", paste0(data$site[i], "_", data$recording[i], ".WAV")))
  }
  
return(NULL)  
}


###
### Example code
###

#move_recordings_to_folder(from = "F:\\Audio",
#                          to = here("Ch2_owl_individual",
#                                    "data",
#                                    "BAOW_recordings"))




