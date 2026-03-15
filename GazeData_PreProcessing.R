#####################################################################################
#  (c) Lucy Jackson 2026 - Conducted as part of Thesis Research   
#
#             GAZE DATA PRE-PROCESSING
#
#    The processing of this data has been completed in line with the VWPre Package by Vincent Poretta: https://vincentporretta.r-universe.dev/VWPre 
#    This was carried out in line with the fantastic tutorial created by Vincent: https://cran.r-project.org/web/packages/VWPre/vignettes/VWPre_Basic_Preprocessing.html
#   
#    This pre-processing has been completed on ALL data, prior to category subsetting. 
#   
################################################################################



library(VWPre)
library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(data.table)
#rm(list=ls())


##########################################################################################################
##########################################################################################################

# Loading in text.file data from the Data Viewer Sample Report. 
# Iteratively loaded as participant recruitment progressed.

# Pps 1- 21
#data1 <- read.table("Raw_Data/040625_click1K.txt",
                #    header = T, sep = "\t", na.strings = c(".", "NA"))
data1 <- read.table("Raw_Data/040625_click1K.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data
# Pps 22 - 25
#data2 <- read.table("Raw_Data/5jun_22to25.txt",
                #  header = T, sep = "\t", na.strings = c(".", "NA"))
data2 <- read.table("Raw_Data/5jun_22to25.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data
# Pps 26-27
#data3 <- read.table("Raw_Data/10jun_26and27.txt",
                 # header = T, sep = "\t", na.strings = c(".", "NA"))
data3 <- read.table("Raw_Data/10jun_26and27.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data

# Pps 28 - 32
#data4 <- read.table("Raw_Data/19June_28to32.txt",
                  #header = T, sep = "\t", na.strings = c(".", "NA"))
data4 <- read.table("Raw_Data/19June_28to32.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data



###############################################################
# Pps 33-40 - 16th Sept

data5 <- read.table("Raw_Data/33to40_SR.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data

###############################################################

# Pps 40-48 - 16th Sept NEED TO DROP 40 HERE

data6 <- read.table("Raw_Data/40to48_SR.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data


data6 <- data6 %>% filter(RECORDING_SESSION_LABEL != "40_f_s ")

###############################################################

# Pps 40-48 - 16th Sept NEED TO DROP 40 HERE

data7 <- read.table("Raw_Data/49_to_60.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data


# Check sample rate of data input
#unique(data7$SAMPLING_RATE)

data8 <- read.table("Raw_Data/61to70_SR.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data


data9 <- read.table("Raw_Data/71to74.txt",
                    header = TRUE,
                    sep = "\t",
                    na.strings = c(".", "NA"),
                    quote = "", # This is the most important fix
                    fill = FALSE,
                    comment.char = "") # Add comment.char="" to prevent issues with # in the data


###############################################################


# Deleting Sample Rate column  so that the bind later will work for correct col numbers
data6 <- subset(data6, select = -SAMPLING_RATE) 
data7 <- subset(data7, select = -SAMPLING_RATE) 
data8 <- subset(data8, select = -SAMPLING_RATE) 
data9 <- subset(data9, select = -SAMPLING_RATE)


# Filtering out two participants: one had incorrect sample rate, other did not complete. 
data9 <- data9 %>% 
  filter(!RECORDING_SESSION_LABEL %in% c("38_e_cs", "1_a_s"))

"##############################################################################

        BINDING DATA TOGETHER

########################################################"


"keeping only critical data"
VWdat <- data9 %>%
  filter(sentence_type=="critical")


"#############################################################################################################
#--------------- Preparing data with prep_data function
  # Conversion col names and re-assignment
#############################################################################################################"


dat0 <- prep_data(data = VWdat, 
                  Subject = "RECORDING_SESSION_LABEL", Item = "item")

dat0 <- dat0 %>%
  rename(variant = utterance)

"#######################################################
#--------------- Relabel NA samples as outside any interest area
  # Examining interest area cols for NA cells, then assigns 0 to ID columns
  # and 'outside' to the label cols. 
#######################################################"

"**** Number of interest areas defined to be suppled to NoIA parameter"

dat1 <- relabel_na(data = dat0, NoIA = 4)

# No. Levels not match Left interest area because we only have data from right eye.

"#######################################################
#--------------- Check encoding of interest areas
  # later processing requires interest area IDs to be numerically coded with
  # vals randing from 0 (outside interest area) up to max of 8. 
  # checking IDs present in data conforms
#######################################################"

check_ia(data = dat1)

"For some reason, my interest areas are not conforming properly, I created a code below to 
  re-iterate the coded labelling for my interest areas"

dat1_cleaned <- dat1 %>%
  mutate(
    # Use case_when to conditionally assign the RIGHT_INTEREST_AREA_LABEL
    # based on the RIGHT_INTEREST_AREA_ID
    RIGHT_INTEREST_AREA_LABEL = case_when(
      RIGHT_INTEREST_AREA_ID == 0 ~ "Outside",
      RIGHT_INTEREST_AREA_ID == 1 ~ "Target_IA",
      RIGHT_INTEREST_AREA_ID == 2 ~ "Competitor_IA",
      RIGHT_INTEREST_AREA_ID == 3 ~ "Dist1_IA",
      RIGHT_INTEREST_AREA_ID == 4 ~ "Dist2_IA",
      # Add more conditions if you have more RIGHT_INTEREST_AREA_IDs
      TRUE ~ RIGHT_INTEREST_AREA_LABEL # This keeps original labels for IDs not explicitly listed
    )
  )

VWPre::check_ia(data = dat1_cleaned) # all good  now.


"##############################################################################################################
##############################################################################################################


#--------------- Aligning to a specific message -
https://cran.r-project.org/web/packages/VWPre/vignettes/VWPre_Message_Alignment.html

Align data to critical stimulus, via sample message alignment.

- In this section I need to clear out the sample message col that had tracker time interference.
- This is done before I align to the sample message being 'target_onset'. 

##############################################################################################################
##############################################################################################################"

aligndat <- dat1_cleaned

### Before creating the time series column, I noticed I had a lot of TRACKER Time interference. 
# This can be messages the machine sends, but it messes up the columns. 

" To amend this, I need to strip it and keep the sample messages of critical periods that I want only." 


# Getting rid of TRACKER TIME message in my sample message, cleaning out. 
  # Note this can take some time with large data, don't worry. 

aligndat_cleaned <- aligndat %>%
  mutate(
    # First, split the message by ';' and take the last part
    # This handles cases like "TRACKER_TIME X Y;target_offset" -> "target_offset"
    # and "MOUSE_CLICK;Show_correct_feedback" -> "Show_correct_feedback"
    # and leaves "PLAY_SOUND" as "PLAY_SOUND"
    Clean_SAMPLE_MESSAGE = sapply(str_split(SAMPLE_MESSAGE, ";"), tail, 1)
  ) %>%
  # Then, filter out messages that are purely "TRACKER_TIME" or NA after splitting
  mutate(
    Clean_SAMPLE_MESSAGE = case_when(
      str_detect(Clean_SAMPLE_MESSAGE, "^TRACKER_TIME") ~ NA_character_, # Set pure TRACKER_TIME to NA
      TRUE ~ Clean_SAMPLE_MESSAGE # Keep all other messages
    )
  )


#Checking
unique(aligndat_cleaned$Clean_SAMPLE_MESSAGE) # all good, trackertime is now gone.

"Because VWPre uses the sample message col internally, will need to relabel it back"

aligndat <- aligndat_cleaned

aligndat <- aligndat %>%
  rename(old_samplemessage = SAMPLE_MESSAGE)


aligndat <- aligndat %>%
  rename(SAMPLE_MESSAGE = Clean_SAMPLE_MESSAGE)

######################################################################################
#   Checking that TIMESTAMP values associated with each message are not the same for each event. 
         #check_all_msgs(data = aligndat) # This prints all the messages if you need to look

# Checkikng the timestamp values associated with the message.
   # They are not the same, showing that alignment is required. 
check_msg_time(data = aligndat, Msg = "target_onset")

"# Converting SAMPLE_MESSAGE back to factor, to allow align_msg to detect the string"

aligndat <- aligndat %>%
  mutate(SAMPLE_MESSAGE = as.factor(SAMPLE_MESSAGE))

"###################################################################################
###---------- Alignment commencing in 3, 2, 1
###################################################################################" 

# creating new column called ;align' which represnts time sequence relative to message

aligned1 <- align_msg(data = aligndat, Msg = "target_onset")

check_msg_time(data = aligned1, Msg = "target_onset") # checking... success if align col = 0 
"Aligned at 0"

# MSGTime <- check_msg_time(data = aligned1, Msg = "target_onset", ReturnData = TRUE)


#######################################################
#--------------- Creating Timeseries col
"This now goes back to the main page: https://cran.r-project.org/web/packages/VWPre/vignettes/VWPre_Basic_Preprocessing.html"
#######################################################

"Once aligned time sequence relative to the message, need to create TIME col
Here, our message relates specifically to time at which target onset was played

NOTE THIS IS NOT SACCADE LAG ADJUSTMENT."

aligned2 <- create_time_series(data = aligned1, Adjust = 0) # adjust col at 0

check_time_series(data = aligned2)
check_msg_time(data = aligned2, Msg = "target_onset") # We see our message is still at 0 point in Timeseries col = GOOD

"##############################################################################################################
##############################################################################################################
##############################################################################################################

                                    NOW ABLE TO PROCEED WITH PRE-PROCESSING

##############################################################################################################
##############################################################################################################
##############################################################################################################"

#######################################################
#--------------- Selecting which eye to use
#######################################################

check_eye_recording(data = aligned2) # showing both

dat3 <- select_recorded_eye(data = aligned2, Recording = "R", WhenLandR = "Right")

#######################################################
#--------------- Trackloss
#######################################################

"Prior to binning, some researchers prefer to remove trials with excessive trackloss"
"I DO NOT HAVE GAZE_Y and GAZE_X ---- "
"
# Tried to amend but still error could not find object 'In_Blink'

dat3 <- dat3 %>%
  rename(
    RIGHT_GAZE_X = Gaze_X,
    RIGHT_GAZE_Y = Gaze_Y
  )"

#dat3 <- mark_trackloss(dat3, Type = "Both", ScreenSize = c(1920, 1080)) #setting to lab size

"####################  move on for now..."


#######################################################
#--------------- Binning the data
#######################################################
check_samplingrate(dat3) 


###################################################################################################

#ds_options(SamplingRate = 1000) # checking suggested downsampled rate, 50ms best

"binning the data"
dat4 <- bin_prop(dat3, NoIA = 4, BinSize = 20, SamplingRate = 1000)# there is a warning 
  # I think because of my sentence variability and across accents
  # did have it at 20ms bin, now 50ms and is a lot better

"This should be tackled when doing time windows. because some sentences wont have anything at certain timepoints. 
"
#check_samplingrate(dat4)

"Samplin rate present in data are 50Hz"
#######################################################
#--------------- Empirical logits
#######################################################
check_samples_per_bin(dat4) 

# Check samples per bin for below


dat5 <- transform_to_elogit(dat4, NoIA = 4, ObsPerBin = 20) #obsperbin - 

# There are other aspects like binomial data (not for me cant do GAMMs,) or fastrack - skipping


#######################################################
#--------------- Saving the data
#######################################################

# Calculate empirical logit for the competitor (IA_2_C)
dat5$elogComp <- log((dat5$IA_2_C + 0.5) / (dat5$NSamples - dat5$IA_2_C + 0.5))

# Calculate empirical logit for the target (IA_1_C)
dat5$elogTarg <- log((dat5$IA_1_C + 0.5) / (dat5$NSamples - dat5$IA_1_C + 0.5))

# Calculate empirical logit Target Advantage

dat5$elogTarAdv <- dat5$elogTarg - dat5$elogComp



"SAVING DATA..."

save(dat5, file="Processed_Data/_Processed.FULL.rda", compress = "xz")



##############################################################################################
##############################################################################################
##############################################################################################



