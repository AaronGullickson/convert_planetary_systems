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


# Create icon sampling function -------------------------------------------

choose_icon <- function(planet) {
  type <- xml_text(xml_find_first(planet, "type"))
  pressure <- xml_text(xml_find_first(planet, "pressure"))
  atmosphere <- xml_text(xml_find_first(planet, "atmosphere"))
  water <- xml_text(xml_find_first(planet, "water"))
  temperature <- xml_text(xml_find_first(planet, "temperature"))
  pos <- as.numeric(xml_text(xml_find_first(planet, "sysPos")))
  
  icon <- "default"
  if(type=="Gas Giant" | (type=="Giant Terrestrial" & pressure=="Very High")) {
    icon <- sample(gasg_sample, 1)
  } else if(type=="Ice Giant") {
    icon <- sample(iceg_sample, 1)
  } else if(type=="Dwarf Terrestrial") {
    icon <- sample(rock_sample, 1)
  } else if(type=="Asteroid Belt") {
    icon <- sample(asteroid_sample, 1)
  } else if(type=="Terrestrial" | type=="Giant Terrestrial") {
    #does it have atmosphere?
    if(pressure=="Vacuum" | pressure=="Trace") {
      #airless rock, but 1 in 100 are hell planets
      if(sample(1:100,1)==1) {
        icon <- sample(hell_sample, 1)
      } else {
        icon <- sample(rock_sample, 1)
      }
    } else if(temperature>100 & (pressure=="High" | pressure=="Very High")) {
      icon <- sample(greenhouse_sample, 1)
    } else {
      if(water>0 & temperature < 0) {
        icon <- sample(frozen_sample, 1)
      } else if(water>=20 & sample(1:100, 1) < 6) {
        icon <- sample(badwater_sample, 1)
      } else if(water>=75) {
        icon <- sample(wet_sample, 1)
      } else if(water>=20) {
        icon <- sample(green_sample, 1)
      } else if(water>0) {
        icon <- sample(dry_sample, 1)
      } else {
        icon <- sample(barren_sample, 1)
      }
    } 
  }
  
  return(icon)
}

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

planets <- xml_find_all(connectors, "planet")

temp <- map(planets, function(planet) {
  xml_add_child(planet, "icon", choose_icon(planet))
})


# Fix Satellites ----------------------------------------------------------

rock_sample <- paste("rock", 1:17, sep="")
oddmoon_sample <- paste("oddmoon", 1:3, sep="")

satellites <- xml_find_all(planets, "satellite")

temp <- map(satellites, function(satellite) {
  planet <-xml_parent(satellite)
  name <- xml_text(satellite)
  size <- xml_attr(satellite, "size")
  if(size=="medium" & sample(1:2,1)==1) {
    #medium moons may not be nice and spherical
    moon_icon <- sample(oddmoon_sample, 1)
  } else {
    moon_icon <- sample(rock_sample, 1)
  }
  xml_remove(satellite)
  new_satellite <- xml_add_child(planet, "satellite")
  xml_add_child(new_satellite, "name", name)
  xml_add_child(new_satellite, "size", size)
  xml_add_child(new_satellite, "icon", moon_icon)
})
