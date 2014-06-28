# script file for fetching, unpacking, formatting all data in the correct formats

print("searching for Derived_Data.RData")
if (!file.exists("Data/RObj/Derived_Data.RData")){
  print("could not find derived-but-not-tidied data. Searching for raw files")
  if (!file.exists("Data/HMD")){
    print("Could not find the raw files in the directory HMD. Searching for zipped file instead")
    if (!file.exists("Data/zipped/hmd_statistics_12_05_2014.zip")){
      print("Could not find the zipped file. Fetching from dropbox")
      # ACTION: Find code that fetches the new file from dropbox  
      
      url_newerData <- 
      if (!file.exists("Data/Zipped/hmd_statistics_12_05_2014.zip")){
        print("Zipfile not found.")
        print("Extremely annoyingly. I don't yet know a reliable way of downloading the file")
        print("directly from dropbox. Instead ask me and place the zipped file in ")
        print("Data/Zipped/hmd_statistics_12_05_2014.zip")
        
          
        } else{
          print("Newer zipfile found. Not downloading")
        }
      
      
      print("Unpacking zipped file to Data/HMD/")
      dir.create("Data/HMD/", recursive=T, showWarnings=F)
      system(
        "unzip Data/Zipped/hmd_statistics_12_05_2014.zip -d Data/HMD/",
        wait=T
      )
      
      print("zipped file now unpacked. Ready for creating derived data")
        
      if(!file.exists("Data/Raw/country_codes__new.csv")){
        print("Country codes file missing. Fix this.")
      }
      
      country_codes <- read.csv("Data/Raw/country_codes__new.csv", header=T)
      
      print("Producing Derived_Data")  
          Make_Derived_Data(
            HMD_Location="Data/HMD/",
            Country.Codes=country_codes,
            Outfile_Location="Data/RObj/",
            Outfile_Name="Derived_Data.RData",
            old_HMD=FALSE
          )
      print("Derived_Data produced. Ready for tidying")
    }
  }
}



