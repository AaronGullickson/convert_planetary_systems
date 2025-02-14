# This script will read in the XML files for planetary systems directly from
# the MekHQ GH repo and will then convert them to separate YAML files for each
# planetary system. These files will be placed in output/planetary_systems. 
# Most of the hard-work of this script is in the functions created in the 
# separate functions.R script which converts the xml data into a set of embedded
# lists which the yaml library knows how to easily convert into yaml.

Sys.time()

# Load libraries and custom functions -------------------------------------

source("check_packages.R")
source("functions.R")

# Fix Connectors ----------------------------------------------------------

source("fix_connectors.R")

# Remove prior data -------------------------------------------------------

list.files(here("output", "planetary_systems", "canon_systems"), 
           full.names = TRUE) |>
  file.remove()
list.files(here("output", "planetary_systems", "connector_systems"), 
           full.names = TRUE) |>
  file.remove()

# Load XML data -----------------------------------------------------------

all_systems <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/systems.xml")
all_system_events <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_events.xml")
all_system_name_change <- read_xml("https://raw.githubusercontent.com/MegaMek/mekhq/refs/heads/master/MekHQ/data/universe/planetary_systems/system_namechanges.xml")

# Clean up input data -----------------------------------------------------

# get children for all
all_systems <- xml_children(all_systems)
all_system_events <- xml_children(all_system_events)
all_system_name_change <- xml_children(all_system_name_change)

# name lists by id
all_systems <- set_names(all_systems, 
                         map_vec(all_systems, function(x) {
                           return(xml_text(xml_find_first(x, "id")))
                         }))
all_system_events <- set_names(all_system_events, 
                               map_vec(all_system_events, function(x) {
                                 return(xml_text(xml_find_first(x, "id")))
                               }))
all_system_name_change <- set_names(all_system_name_change, 
                                    map_vec(all_system_name_change, function(x) {
                                      return(xml_text(xml_find_first(x, "id")))
                                    }))
connectors <- set_names(connectors, 
                        map_vec(connectors, function(x) {
                          return(xml_text(xml_find_first(x, "id")))
                        }))

# Test Case ---------------------------------------------------------------

#id <- "O'Fallon"
#psystem <- read_planetary_system(id)
#cat(as.yaml(psystem, indent.mapping.sequence = TRUE, precision = 12),
#    file = paste("output/planetary_systems/canon_systems/", 
#                 make_clean_names(id), 
#                 ".yml", sep=""))



# Process planetary systems -----------------------------------------------

## Canon Systems ##

#check to make sure cleaning won't produce dupes
y <- make_clean_names(names(all_systems),
                     allow_dupes = TRUE,
                     case = "big_camel")
sum(duplicated(y))

names(all_systems) |>
  map(function(x) {
    result <- read_planetary_system(x) |>
      as.yaml(indent.mapping.sequence = TRUE, precision = 12)

    file_name <- paste("output/planetary_systems/canon_systems/",
                       make_clean_names(x, case = "big_camel"),
                       ".yml",
                       sep = "")

    file <- file(file_name,
                 "w",
                 encoding = "UTF-8")

    cat(result, file = file, sep = "")
    close(file)
  })

## Connector Systems ##

#check to make sure cleaning won't produce dupes
y <- make_clean_names(names(connectors), 
                      allow_dupes = TRUE,
                      case = "upper_lower")
sum(duplicated(y))

names(connectors) |>
  map(function(x) {
    result <- read_planetary_system(x, connector = TRUE) |>
      as.yaml(indent.mapping.sequence = TRUE, precision = 12)
    
    file_name <- paste("output/planetary_systems/connector_systems/",
                       make_clean_names(x, case = "upper_lower"),
                       ".yml",
                       sep = "")
    
    file <- file(file_name,
                 "w",
                 encoding = "UTF-8")
    
    cat(result, file = file, sep = "")
    close(file)
  })

Sys.time()

