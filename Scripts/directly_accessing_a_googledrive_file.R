
#install.packages("repmis")

require(repmis)

# example from http://yaitguy.blogspot.ie/2012/08/direct-link-to-shared-google-drive-file.html



codebit <- "0B0M50ZN42mjdM29kRTN4Ni1RaDg"
url_to_use <- paste0(
    "https://drive.google.com/uc?export=&confirm=no_antivirus&id=",
    codebit    
)


tryfile <- source_data(url_to_use, sep=",")
