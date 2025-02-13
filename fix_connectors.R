# Connectors are busted. There are a bunch of duplicate entries (although 
# different systems), satellites are coded using a legacy system that is 
# incompatible, and they have no icons present. This script will fix that 
# and keep them in xml format so they can then be processed properly by 
# the convert_data.R script.


# Read libraries and data -------------------------------------------------

source("check_packages.R")
connectors <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_connectors.xml")

# getting the children directly gives me a list that is either to work with
connectors <- xml_children(connectors)

# Set up icon samples -----------------------------------------------------

gasg_sample <- paste("gasg", 1:38, sep="")
iceg_sample <- paste("iceg", 1:19, sep="")
rock_sample <- paste("rock", 1:17, sep="")
asteroid_sample <- paste("asteroid", 1:3, sep="")
hell_sample <- paste("hell", 1:37, sep="")
wet_sample <- paste("wet", 1:21, sep="")
green_sample <- paste("green", 1:57, sep="")
dry_sample <- paste("dry", 1:27, sep="")
barren_sample <- paste("barren", 1:24, sep="")
greenhouse_sample <- paste("greenhouse", 1:41, sep="")
frozen_sample <- paste("frozen", 1:62, sep="")
badwater_sample <- paste("badwater", 1:45, sep="")

# Remove duplicates -------------------------------------------------------

# get ids
id <- map_vec(connectors, function(x) {
  return(xml_text(xml_find_first(x, "id")))
})
sum(duplicated(id))

# only keep the non-dupes
connectors <- connectors[which(!duplicated(id))]

# check for dupes again
id <- map_vec(connectors, function(x) {
  return(xml_text(xml_find_first(x, "id")))
})
sum(duplicated(id))

# Add icons ---------------------------------------------------------------


