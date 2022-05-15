# Cleaned Respo Code for plate community P/R #####
# Created by Nyssa Silbiger
# Edited on 12/14/2021


#Install Packages 

if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install_github('colin-olito/LoLinR') 
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron') 
if ("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 
if ("here" %in% rownames(installed.packages()) == 'FALSE') install.packages('here') 
if ("patchwork" %in% rownames(installed.packages()) == 'FALSE') install.packages('patchwork') 


#Read in required libraries
##### Include Versions of libraries
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('tidyverse')
library('here')
library("patchwork")

# get the file path


#set the path to all of the raw oxygen datasheets
path.p<-here("data","RespoFiles","RawO2") #the location of all your respirometry files

# bring in all of the individual files
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders

#basename above removes the subdirectory name from the file, re-nale as file.names.full
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) 


#Load your respiration data file, with all the times, water volumes(mL), algal biomass weight (dry weight) (g)
RespoMeta <- read_csv(file = here("data","RespoFiles","RespoMetadata.csv")) %>%
  mutate(Notes_respo = Notes)%>%
  select(!Notes)
WhelkMeta <- read_csv(file = here("data","RespoFiles","WhelkMetaData.csv")) %>%
  left_join(read_csv(file = here("data","RespoFiles","WhelksAFDW_Final.csv"))) # join with the AFDW data


# join the data together
Sample.Info <-left_join(RespoMeta, WhelkMeta) %>%
  mutate(start.time = mdy_hms(paste(as.character(Date), as.character(start.time))),
         stop.time = mdy_hms(paste(as.character(Date), as.character(stop.time))))

# convert datetime in SampleInfo

#View(Sample.Info)

##### Make sure times are consistent ####

# make start and stop times real times, so that we can join the data frames
# Sample.Info$start.time <- as.POSIXct(Sample.Info$start.time,format="%H:%M", tz = "") #convert time from character to time
# Sample.Info$stop.time <- as.POSIXct(Sample.Info$stop.time,format="%H:%M", tz = "") #convert time from character to time

#view(Sample.Info)
## There are some extra files from repeats so I added this line to only select the ones in the actual metadata sheet
# filenames_final<-strsplit(file.names, '.csv') %>% # extract the filename
#   unlist() %>% # make it a vector
#   tibble() %>% # now a tibble so I can filter easily in the pipe
#   filter(. %in% Sample.Info$FileName) %>% # only keep the file names that are on the metadatasheet
#   pull(.) # make it a vector again

filenames_final<-file.names
#generate a 3 column dataframe with specific column names
# data is in umol.L.sec
Respo.R<- data.frame(matrix(NA, nrow=length(filenames_final), ncol=4))
colnames(Respo.R) <- c("FileID","Intercept", "umol.L.sec","Temp.C")
#View(Respo.R)



###forloop#####
for (i in 1: length(file.names.full)) {
  FRow<-which(Sample.Info$FileID==filenames_final[i]) #stringsplit this renames our file
  Respo.Data1 <-read_csv(file.path(path.p, paste0(file.names.full[i])), skip = 1) %>%
    mutate(Time = mdy_hms(paste(Date,Time))) %>% # covert time
    dplyr::select(Time,Value,Temp) %>% # keep only what we need
    
    drop_na() # drop NAs
  
  Respo.Data1 <- Respo.Data1 %>%
    #filter(between(Time, max(Time)-60*21,max(Time)-60*1)) #taking last 20 minutes, from 21st to the last minute
  filter(between(Time, Sample.Info$start.time[FRow], Sample.Info$stop.time[FRow])) # select only data between start and stop time
    
  
  Respo.Data1 <-  Respo.Data1[-c(1:120),] %>% #we want to start at minute 2 to avoid any noise from the start of the trial
       mutate(sec = 1:n())  #create a new column for every second for the regression

    #Get the filename without the .csv
  rename<- sub(".csv","", filenames_final[i]) 
  #rename<-  filenames_final[i]
  
  
  ### plot and export the raw data ####
  p1<- ggplot(Respo.Data1, aes(x = sec, y = Value))+
    geom_point(color = "dodgerblue")+
    labs(
      x = 'Time (seconds)',
      y = expression(paste(' O'[2],' (',mu,'mol/L)')),
      title = "original"
    )
  
  # thin the data by every 20 seconds to speed it up
  Respo.Data.orig<-Respo.Data1#save original unthinned data #there is no thin() anymore, trying to find alternative 
  Respo.Data1 <-  thinData(Respo.Data1 ,by=20)$newData1 #thin data by every 20 points for all the O2 values
  Respo.Data1$sec <- as.numeric(rownames(Respo.Data1 )) #maintain numeric values for time
  Respo.Data1$Temp<-NA # add a new column to fill with the thinned data
  Respo.Data1$Temp <-  thinData(Respo.Data.orig,xy = c(1,3),by=20)$newData1[,2] #thin data by every 20 points for the temp values
 
  p2<- ggplot(Respo.Data1, aes(x = sec, y = Value))+
    geom_point(color = "dodgerblue")+
    labs(
      x = 'Time (seconds)',
      y = expression(paste(' O'[2],' (',mu,'mol/L)')),
      title = "thinned"
    )
  
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "eq", "pc")
  Regs  <-  rankLocReg(xall=Respo.Data1$sec, yall=Respo.Data1$Value, alpha=0.5, method="pc", verbose=TRUE)  
 
  # Print across two pages so use baseplot to create the pdf
  pdf(paste0(here("output","RespoOutput"),"/", rename,"thinning.pdf"))
  
  plot(Regs) # plot the results of Regs
  plot(p1+p2) # use patchwork to bring the raw and thinned data together
  dev.off()
  
  # fill in all the O2 consumption and rate data
  # need clarity on what this is
  Respo.R[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Respo.R[i,1] <- paste0(rename,".csv") #stores the file name in the Date column
  Respo.R[i,4] <- mean(Respo.Data1$Temp, na.rm=T)  #stores the Temperature from the incubation in the Temp.C column
}  



#export raw data and read back in as a failsafe 
#this allows me to not have to run the for loop again 
write_csv(Respo.R, here("data","RespoFiles","Respo.R.csv"))  

Respo.R <- read_csv(here("data","RespoFiles","Respo.R.csv"))

# Calculate Respiration rate

Respo.R<-Respo.R %>%
  drop_na(FileID) %>% # drop NAs
  left_join(Sample.Info) %>% # Join the raw respo calcuations with the metadata
  mutate(volume = ifelse(is.na(volume),650,volume))%>% # add 650 ml for volume of all blanks
  mutate(umol.sec = umol.L.sec*volume) %>% #Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
  mutate_if(sapply(., is.character), as.factor) %>% #convert character columns to factors
  mutate(BLANK = as.factor(BLANK)) #make the blank column a factor



#View(Respo.R)


#Account for blank rate by light/Dark and Block (if we do one blank per block)

#View(Respo.R)

## Remove the "bad" data
bad<-read_csv(here("data","RespoFiles", "respo_files_to_check.csv")) %>%
  select(Bad) %>% # select only the bad file names
  drop_na() %>%
  mutate(FileID = paste0(Bad,"_O2.csv")) # make it the same as the filenames

Respo.R_Normalized <- Respo.R %>%
  filter(!FileID %in% bad$FileID)%>% # remove the bad files
#  group_by(BLANK, Temp.Block)%>% # also add block here if one blank per block 
  group_by(BLANK, Temp.Block)%>% # also add 
 # group_by(BLANK)%>% # also add block here if one blank per block
  summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>%
  filter(BLANK ==1)%>% # only keep the actual blanks
  select(blank.rate = umol.sec, Temp.Block) %>% # only keep what we need and rename the blank rate column
  ungroup()%>%
  select(!BLANK) %>%
  right_join(Respo.R) %>% # join with the respo data %>%
  mutate(umol.sec.corr = umol.sec - blank.rate, # subtract the blank rates from the raw rates
         mmol.gram.hr = 0.001*(umol.sec.corr*3600)/AFDW_g,
         mmol.gram.hr_uncorr = 0.001*(umol.sec*3600)/AFDW_g)  %>% # convert to mmol g hr-1
  filter(is.na(BLANK)) %>% # remove all the blank data
  select(Date, SampleID, Species, Weight,TotalBiomass = AFDW_g,volume, mmol.gram.hr, chamber.channel,Temp.C, Temp.Block, mmol.gram.hr_uncorr) %>%  #keep only what we need
  ungroup()
  
#View(Respo.R_Normalized)
## convert the rates to a positive number so it is not just O2 uptake but a respo rate

Respo.R_Normalized <-Respo.R_Normalized %>%
  mutate(mmol.gram.hr = -mmol.gram.hr,
         mmol.gram.hr_uncorr = -mmol.gram.hr_uncorr)

write_csv(Respo.R_Normalized , here("data","RespoFiles","Respo.RNormalized.csv"))  

# Remove data after death and remove some of the bad ones.. everything after 28 degrees stopped respiring and microbe respiration took over. Two AS snails this happened after 25C and 6 snails have bad data
Respo.R_Normalized_clean<-Respo.R_Normalized %>%
  filter(!SampleID %in% c("AS_5", "AS_17", "AS_6", "AS_7", "ML_4", "ML_15")) %>% # the bad snails
  filter(Temp.Block<30) %>% # kepp all the temps below 28 C %>%
  filter(Temp.Block != 28 & SampleID != "AS_15",
         Temp.Block != 28 & SampleID != "AS_16",
         Temp.Block != 28 & SampleID != "ML_17",
         Temp.Block != 28 & SampleID != "ML_18",
         Temp.Block != 10 & SampleID != "AS_10",
         Temp.Block != 16 & SampleID != "ML_12"
         
         
  )

write_csv(Respo.R_Normalized_clean , here("data","RespoFiles","Respo.RNormalized_clean.csv"))  


# quick plot
Respo.R_Normalized_clean %>%
 # filter(Temp.Block != 28)%>%
  mutate(mmol.gram.hr2 = ifelse(mmol.gram.hr>0,0,mmol.gram.hr))%>%
  ggplot(aes(x = Temp.C, y = log(mmol.gram.hr+0.1), color = Species))+
  geom_point()+
  geom_line()+
  facet_wrap(~SampleID, scales = "free_y")+
  theme_bw()
  #xlim(13,28)
  
Respo.R_Normalized %>%
  # filter(Temp.Block != 28)%>%
  mutate(mmol.gram.hr2 = ifelse(mmol.gram.hr>0,0,mmol.gram.hr))%>%
  ggplot(aes(x = Temp.C, y = log(mmol.gram.hr+0.1), color = Species))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~SampleID, scales = "free")+
  theme_bw()

blank.rate<-Respo.R %>%
  group_by(BLANK) %>% # also add block here if one blank per block
  # group_by(BLANK)%>% # also add block here if one blank per block
  summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>%
  filter(BLANK ==1)%>% # only keep the actual blanks
  select(blank.rate = umol.sec) # only keep what we need and rename the blank rate column
Respo.R_Normalized2<-Respo.R %>% # join with the respo data %>%
  mutate(blank.rate = blank.rate$blank.rate,
         umol.sec.corr = umol.sec - blank.rate, # subtract the blank rates from the raw rates
         mmol.gram.hr = 0.001*(umol.sec.corr*3600)/AFDW_g,
         mmol.gram.hr_uncorr = 0.001*(umol.sec*3600)/AFDW_g)  %>% # convert to mmol g hr-1
  filter(is.na(BLANK)) %>% # remove all the blank data
  select(Date, SampleID, Species, Weight,TotalBiomass = AFDW_g,volume, mmol.gram.hr, chamber.channel,Temp.C, Temp.Block, mmol.gram.hr_uncorr) %>%  #keep only what we need
  ungroup()

#View(Respo.R_Normalized)
Respo.R_Normalized2 %>%
  mutate(mmol.gram.hr2 = ifelse(mmol.gram.hr>0,0,mmol.gram.hr))%>%
   # filter(Temp.Block != 24 & SampleID == "ML_9") %>%
 # filter(Temp.Block != 28)%>%
  ggplot(aes(x = Temp.C, y = -mmol.gram.hr, color = Species))+
  geom_point()+
  geom_line()+
  facet_wrap(~SampleID, scale = "free_y")+
  theme_bw()
