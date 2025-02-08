


# Load libraries and custom functions -------------------------------------

source("check_packages.R")
source("functions.R")


# Load XML data -----------------------------------------------------------

systems <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/systems.xml")
system_events <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_events.xml")
# TODO: there is something bad in this XML 
#system_name_change <- read_xml("https://github.com/MegaMek/mekhq/blob/master/MekHQ/data/universe/planetary_systems/system_namechanges.xml")
system_connectors <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_connectors.xml")


# Clean up input data -----------------------------------------------------

# get children for all
systems <- xml_children(systems)
system_events <- xml_children(system_events)

# name lists by id
systems <- set_names(systems, 
                     map_vec(systems, function(x) {
                       return(xml_text(xml_find_first(x, "id")))
                     }))
system_events <- set_names(system_events, 
                           map_vec(system_events, function(x) {
                             return(xml_text(xml_find_first(x, "id")))
                           }))

# Test Case ---------------------------------------------------------------

id <- "A Place"

system_xml <- xml_child(systems, 1)
psystem <- read_planetary_system(system_xml)

cat(as.yaml(psystem, indent.mapping.sequence = TRUE, precision = 6))
