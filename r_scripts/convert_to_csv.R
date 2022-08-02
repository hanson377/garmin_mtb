library(stringr)
library(dplyr)

files_to_convert <- data.frame(files = list.files(path = "/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_fit/")) ## detect raw fit files in data directory
files_to_convert <- files_to_convert %>% filter(str_detect(files,"fit")) ## filter out old csvs, only look at fit files
files_to_convert <- files_to_convert$files

files_to_convert <- data.frame(value = str_split_fixed(files_to_convert,'[.]',n=2)[,1])

files_to_convert <- files_to_convert$value

for (i in files_to_convert) { ## run loop to convert fit files to csv via the fit sdk offered by garmin

string <- paste('java -jar /Users/hanson377/Desktop/local_projects/chicago_half_training/fitSDK/java/FitCSVTool.jar /Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_fit/',i,'.fit',sep='')
system(string)

## now, clean up
file.copy(from = paste("/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_fit/", i, ".csv",sep=''),
          to = paste("/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_csv/", i, ".csv",sep=''))

file.remove(from = paste("/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_fit/", i, ".csv",sep=''))
file.remove(from = paste("/Users/hanson377/Desktop/local_projects/garmin_bike/data/raw_fit/", i, ".fit",sep=''))

}
