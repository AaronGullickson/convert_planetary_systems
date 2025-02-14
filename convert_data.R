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


# Read canon population data ----------------------------------------------

# We used some canon population data last time but never recorded the 
# canon sources. I will read this data in and just use the columns as 
# vectors to match exact population values, because the system names 
# do not reliably match (and in some cases we did not input the data).
pop <- read_csv(here("input", "canon_populations.csv"))

# turn these into vectors where we can test if a value is in it with the 
# %in% syntax
pop2750 <- as.numeric(na.omit(pop$`2750`))
pop3025 <- as.numeric(na.omit(pop$`3025`))
pop3067 <- as.numeric(na.omit(pop$`3067`))
pop3079 <- as.numeric(na.omit(pop$`3079`))
pop3145 <- as.numeric(na.omit(pop$`3145`))

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

# get children for all because they become lists that are easier to work with
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

# put in test ids here. Always include Terra if you want to run it in MekHQ.
# Galatea is also recommended because that is where your starting system
# will be as a merc at default dates
# test_ids <- c("Terra","Galatea","Addicks","Albiero","Caph","Celano",
#               "Liao (Cynthiana 2202-)","Keid")
# for(id in test_ids) {
#   psystem <- read_planetary_system(id)
#   cat(as.yaml(psystem, indent.mapping.sequence = TRUE, precision = 12),
#       file = paste("output/planetary_systems/canon_systems/",
#                    make_clean_names(id, case = "big_camel"),
#                    ".yml", sep=""))
# }

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

