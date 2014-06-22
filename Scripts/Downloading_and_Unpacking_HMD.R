# Code for Downloading and unpacking HMD zip file

url <- "https://www.dropbox.com/s/qcp8yk1f20lx6c0/hmd_statistics_12_05_2014.zip"



download_file_url <- function (
    
    url, 
    outfile,
    ..., sha1 = NULL) 
{
    require(RCurl)
    require(devtools)
    require(repmis)
    require(httr)
    require(digest)
    
    stopifnot(is.character(url), length(url) == 1)
    filetag <- file(outfile, "wb")
    request <- GET(url)
    stop_for_status(request)
    writeBin(content(request, type = "raw"), filetag)
    close(filetag)
}

download_file_url(
    url=url,
    outfile="hmd_statistics_12_05_2014.zip"
    )

dir.create("Data")
dir.create("Data/HMD/")
system("unzip hmd_statistics_12_05_2014.zip -d Data/HMD/", wait=F)
