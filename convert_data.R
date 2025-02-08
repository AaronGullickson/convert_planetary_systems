


# Load libraries and custom functions -------------------------------------

source("check_packages.R")
source("functions.R")


# Load XML data -----------------------------------------------------------

systems <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/systems.xml")
system_events <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_events.xml")
# TODO: there is something bad in this XML 
#system_name_change <- read_xml("https://github.com/MegaMek/mekhq/blob/master/MekHQ/data/universe/planetary_systems/system_namechanges.xml")
system_connectors <- read_xml("https://github.com/MegaMek/mekhq/raw/refs/heads/master/MekHQ/data/universe/planetary_systems/system_connectors.xml")

# Test Case ---------------------------------------------------------------

system_xml <- xml_child(systems, 1)
psystem <- read_planetary_system(system_xml)
