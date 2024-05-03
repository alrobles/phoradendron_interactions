
download.file(url = "https://ecointeraction.fra1.digitaloceanspaces.com/input.zip",
              destfile = "data/input.zip")

if(!dir.exists("data/input")){
  dir.create("data/input")
}
unzip(zipfile = "data/input.zip", exdir = "data/input")
