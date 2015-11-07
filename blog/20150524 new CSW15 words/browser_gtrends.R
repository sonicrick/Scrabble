URL_GT=function(keyword="", country=NA, region=NA, year=NA, month=1, length=3){
  
  start="http://www.google.com/trends/trendsReport?hl=en-US&q="
  end="&cmpt=q&content=1&export=1"
  geo=""
  date=""
  
  #Geographic restrictions
  if(!is.na(country)) {
    geo="&geo="
    geo=paste(geo, country, sep="")
    if(!is.na(region)) geo=paste(geo, "-", region, sep="")
  }
  
  queries=keyword[1]
  if(length(keyword)>1) {
    for(i in 2:length(keyword)){
      queries=paste(queries, "%2C ", keyword[i], sep="")
    }
  }
  
  #Dates
  if(!is.na(year)){
    date="&date="
    date=paste(date, month, "%2F", year, " ", length, "m", sep="")
  }
  
  URL=paste(start, queries, geo, date, end, sep="")
  return(URL)
}

downloadGT=function(URL, downloadDir){
  
  #Determine if download has been completed by comparing the number of files in the download directory to the starting number
  startingFiles=list.files(downloadDir)
  browseURL(URL)
  endingFiles=list.files(downloadDir)
  
  while(length(setdiff(endingFiles,startingFiles))==0) {
    Sys.sleep(3)
    endingFiles=list.files(downloadDir)
  }
  filePath=setdiff(endingFiles,startingFiles)
  return(filePath)
}

words <- cfull  # if not loaded, run curate_words.R
downloadDir="C:/Users/Ricky/Downloads" # (i.e. your default browsers download path)

# manually do batches of 100 words, then copy them into new folders
batch <- words[801:873]

for (w in batch) {
  URL=URL_GT(w)
  downloadGT(URL, downloadDir)
  Sys.sleep(.1)
}

#setwd(downloadDir)
#data=read.csv(downloadGT(URL), header=F)