library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

## pull in all csv files

files_to_import <- data.frame(filename = list.files(path = "/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_csv/")) ## detect raw fit files in data directory
files_to_import <- files_to_import %>% filter(str_detect(filename,"csv")) ## filter out old csvs, only look at fit files

## identify files not already in foder
clean_files <- data.frame(filename = list.files(path = "/Users/hanson377/Desktop/local_projects/garmin_bike/data/clean_csv/"))

files_to_import <- files_to_import %>% anti_join(clean_files,by='filename')
files_to_import <- files_to_import$filename ## convert to list for loop below

## run loop to bind individual csv files into one large dataframe for manipulation
df <- NA
counter <- 1
n_files <- length(files_to_import)

for (i in files_to_import) {

string <- paste('/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_csv/',i,sep='')
temp_df <- read.csv(string)
temp_df$filename <- i

df <- rbind(df,temp_df)
print(paste('% done: ',counter/n_files,sep=''))
counter <- counter+1

}
###

## extract split data
splits <- subset(df,Type == 'Data' & Message == 'record')
splits <- splits %>% group_by(filename) %>% mutate(row_number = row_number())

## calculate distance related data
distances <- splits %>%
  select(filename,row_number,cumu_distance_meters = Value.4) %>%
    mutate(cumu_distance_miles = cumu_distance_meters*0.000621371)  %>%
      arrange(filename,cumu_distance_meters) %>%
        mutate(distance_segment_miles = cumu_distance_miles-lag(cumu_distance_miles))



## function for pulling value of interest
wrangleField <- function(string_value){

  df <- splits[ , grepl( "Field." , names( splits ) ) ]
  df[] <- lapply(df, as.character)

  ## identify all possible columns with heart rate
  fields_to_pull <- unique(colnames(df)[max.col(df==string_value)])
  values_to_pull <- str_split_fixed(fields_to_pull,'[.]',n=2)[,2]

  hr_df <- data.frame(filename=NA,row_number=NA,field=NA,value=NA)

  for (i in values_to_pull){

    field <- paste('Field.',i,sep='')
    value <- paste('Value.',i,sep='')

    temp <- splits %>% dplyr::select(filename,row_number,field = {{field}},value = {{value}})

    hr_df <- rbind(hr_df,temp)


  }
  hr_df <- hr_df %>% filter(field==string_value & is.na(field) == FALSE)
  return(hr_df)
}
heart_rate <- wrangleField('heart_rate')
enhanced_altitude <- wrangleField('enhanced_altitude')
timestamp <- wrangleField('timestamp')
enhanced_speed <- wrangleField('enhanced_speed')
functional_threshold_power <- wrangleField('functional_threshold_power')
position_lat <- wrangleField('position_lat')
position_long <- wrangleField('position_long')

## rename accordingly
heart_rate <- heart_rate %>% select(filename,row_number,heart_rate=value)
enhanced_altitude <- enhanced_altitude %>% select(filename,row_number,enhanced_altitude_m=value)
timestamp <- timestamp %>% select(filename,row_number,timestamp=value)

timestamp <- timestamp %>% select(filename,row_number,timestamp=value)
enhanced_speed <- enhanced_speed %>% select(filename,row_number,enhanced_speed=value)
functional_threshold_power <- functional_threshold_power %>% select(filename,row_number,functional_threshold_power=value)
position_lat <- functional_threshold_power %>% select(filename,row_number,position_lat=value)
position_long <- functional_threshold_power %>% select(filename,row_number,position_long=value)


## join onto base of distance
final_df <- distances %>% left_join(heart_rate,by=c('filename','row_number'))
final_df <- final_df %>% left_join(enhanced_altitude,by=c('filename','row_number'))
final_df <- final_df %>% left_join(timestamp,by=c('filename','row_number'))

## convert to numerics
final_df$timestamp <- as_datetime(as.numeric(final_df$timestamp),origin = '1990-01-01',tz='MST')
final_df$row_number <- as.numeric(final_df$row_number)
final_df$cumu_distance_meters <- as.numeric(final_df$cumu_distance_meters)
final_df$cumu_distance_miles <- as.numeric(final_df$cumu_distance_miles)
final_df$distance_segment_miles <- as.numeric(final_df$distance_segment_miles)

final_df$heart_rate <- as.numeric(final_df$heart_rate)
final_df$enhanced_altitude_m <- as.numeric(final_df$enhanced_altitude_m)

## calculatre time intervals
final_df <- final_df %>%
  arrange(filename,row_number) %>%
    group_by(filename) %>%
      mutate(time_s = difftime(timestamp, lag(timestamp), units = "secs"))

final_df$time_s <- as.numeric(final_df$time_s)

## calculate change in elevation
final_df <- final_df %>% arrange(filename,row_number) %>% group_by(filename) %>% mutate(elevation_change_m = enhanced_altitude_m-lag(enhanced_altitude_m))

##
final_df$pace_secs_per_mile <- final_df$time_s/final_df$distance_segment_miles
final_df$pace_mins_per_mile <- final_df$pace_secs_per_mile/60
final_df$log_pace <- log10(final_df$pace_mins_per_mile)

## log hr
final_df$log_hr <- log10(final_df$heart_rate)

#write.csv(final_df,paste('/Users/hanson377/Desktop/local_projects/garmin_bike/data/clean_csv/')
