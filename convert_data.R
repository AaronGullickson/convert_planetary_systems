# This script will read in the XML files for planetary systems directly from
# the MekHQ GH repo and will then convert them to separate YAML files for each
# planetary system. These files will be placed in output/planetary_systems. 
# Most of the hard-work of this script is in the functions created in the 
# separate functions.R script which converts the xml data into a set of embedded
# lists which the yaml library knows how to easily convert into yaml.

# Load libraries and custom functions -------------------------------------

source("check_packages.R")
source("functions.R")


# Remove prior data -------------------------------------------------------

quietly(do.call(file.remove, 
                list(list.files(here("output", "planetary_systems"), 
                                full.names = TRUE))))

# Load XML data -----------------------------------------------------------

all_systems <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/systems.xml")
all_system_events <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_events.xml")
# TODO: there is something bad in this XML 
#system_name_change <- read_xml("https://github.com/MegaMek/mekhq/blob/master/MekHQ/data/universe/planetary_systems/system_namechanges.xml")
all_system_connectors <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_connectors.xml")


# Clean up input data -----------------------------------------------------

# get children for all
all_systems <- xml_children(all_systems)
all_system_events <- xml_children(all_system_events)

# name lists by id
all_systems <- set_names(all_systems, 
                         map_vec(all_systems, function(x) {
                           return(xml_text(xml_find_first(x, "id")))
                         }))
all_system_events <- set_names(all_system_events, 
                               map_vec(all_system_events, function(x) {
                                 return(xml_text(xml_find_first(x, "id")))
                               }))

# Test Case ---------------------------------------------------------------

#id <- "Graham IV"
#psystem <- read_planetary_system(id)
#cat(as.yaml(psystem, indent.mapping.sequence = TRUE, precision = 12))

# Process planetary systems -----------------------------------------------

names(all_systems) |>
  map(function(x) {
    result <- read_planetary_system(x) |>
      as.yaml(indent.mapping.sequence = TRUE, precision = 12) |>
      # this will fix weird error with quoted y keys
      str_replace("\\'y\\'", "y")
    
    file <- file(paste("output/planetary_systems/", x, ".yml", sep = ""), 
                 "w", 
                 encoding = "UTF-8")
    
    cat(result, file = file, sep = "")
  })
