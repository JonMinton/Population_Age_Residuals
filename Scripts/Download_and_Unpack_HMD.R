# Code for Downloading and unpacking HMD zip file

url <- "https://www.dropbox.com/s/qcp8yk1f20lx6c0/hmd_statistics_12_05_2014.zip"


download_file_url(
    url=url,
    outfile="hmd_statistics_12_05_2014.zip"
    )

dir.create("Data/HMD/", recursive=T)
system("unzip hmd_statistics_12_05_2014.zip -d Data/HMD/", wait=F)
