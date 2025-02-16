# These functions will read in a given xml format and turn it into a set of 
# embedded lists that can be processed by the yaml library.


# key values in this vector should be split into a source and value pair
values_sourceable <- c("name", "dayLength", "yearLength", "sysPos", "diameter",
                        "density", "smallMoons", "gravity", "ring", "size",
                       "temperature", "water", "composition", "population",
                       "faction", "nadirCharge", "zenithCharge", "type",
                       "atmosphere", "lifeForm", "hpg", "hiringHall",
                       "socioIndustrial", "pressure", "spectralType")

# put key values here that need to be transformed for certain values that 
# show up in events
values_logical <- c("nadirCharge", "zenithCharge", "ring")
values_integer <- c("sucsId", "primarySlot", "sysPos", "diameter", "dayLength", 
                    "temperature", "water", "smallMoons")
values_double <- c("xcood", "ycood", "orbitalDist", "gravity", "population", 
                   "density", "yearLength")


# these are the planet characteristics which may or may not be present so
# we need to add them to the base planet characteristics
planet_optional <- c("pressure", "atmosphere", "composition", "gravity",
                     "diameter", "density", "dayLength", "yearLength",
                     "temperature", "water", "lifeForm", "desc", "ring",
                     "smallMoons")

# read an individual calue from xml node and determine how to display it in yaml
read_value <- function(xml_data, value_name, year = NULL) {
  
  value_node <- xml_data |>
    xml_find_first(value_name)
  
  value <- value_node |>
    xml_text()
  
  if(is.na(value)) {
    return(NA)
  }
  
  # does the value need to changed from character?
  if(value_name %in% values_logical) {
    value <- as.logical(value)
  } else if(value_name %in% values_integer) {
    value <- as.integer(value)
  } else if(value_name %in% values_double) {
    value <- as.double(value)  
  }
  
  # faction can have multiple factions like "IND, FS". We need to put those
  # in an unnamed list so they print out as an array. 
  if(value_name == "faction") {
    value <- value |> str_split_1(",") |>
      str_trim() |>
      as.list()
  }
  
  # capitalize lifeform values and remove ending s to conform to enum
  if(value_name == "lifeForm") {
    value <- value |> 
      str_remove("s$") |>
      str_to_upper() |>
      str_replace("^AMPH$", "AMPHIBIAN")
  }
  
  # covnert atmosphere to ENUM values
  if(value_name == "atmosphere") {
    value <- value |>
      str_remove_all("\\(") |>
      str_remove_all("\\)") |>
      str_remove_all("\\s+") |>
      str_to_upper() |>
      str_replace("POISONOUS$", "POISON") |>
      str_replace("FLAMMABLE$", "FLAME")
  }
  
  if(value_name == "type") {
    value <- value |>
      str_replace(" ", "_") |>
      str_to_upper()
  }
  
  if(value_name == "pressure") {
    value <- value |>
      str_replace(" ", "_") |>
      str_to_upper() |>
      str_replace("^LOW$", "THIN") |>
      str_replace("^NORMAL$", "STANDARD")
  }
  
  # try to clean up date values
  if(value_name == "date") {
    # the namechange data has starting values of "0000-01-01" which
    # lubridate handles find, but outputs as "0-01-01" which Jackson
    # does not like. We will change these to "2108-12-05" which is the
    # date of the Tau Ceti launch.
    value <- value |>
      str_replace("^0000-01-01", "2108-12-05")
    
    value <- as.character(as_date(value))
  }
  
  source <- xml_attr(value_node, "source")
  
  # hack alert - we need to fix up population values for some canon entries
  # that are not recorded as such. 
  if(value_name == "population" & !is.null(year)) {
    if(year == 2750 & value %in% pop2750) {
      source <- "Star League Handbook, original"
    }
    if((year == 3025 & value %in% pop3025) |
       (year == 3067 & value %in% pop3067) |
       (year == 3079 & value %in% pop3079) |
       (year == 3145 & value %in% pop3145)) {
      source <- "canon"
    }
  }
  
  #do we need to split this into source and value?
  if(value_name %in% values_sourceable) {
    if(is.na(source) || source == "noncanon") {
      # if there is no source or its noncanon, we treat that as default
      # and do not add a source key
      return(value)
    } else {
      return(list(source = source, value = value))
    }
  } else {
    # just a simple primitive type returned since its not sourceable
    return(value)
  }
}

# the primary function
read_planetary_system <- function(id, connector = FALSE) {
  
  # retrieve the system information
  if(connector) {
    system_xml <- connectors[[id]]
  } else {
    system_xml <- all_systems[[id]]
  }
  system_event_xml <- all_system_events[[id]]
  system_name_change_xml <- all_system_name_change[[id]]

  # start the planetary system list with basic information
  planetary_system <- list(
    id = id,
    sucsId = read_value(system_xml, "sucsId"),
    xcood = read_value(system_xml, "xcood"),
    ycood = read_value(system_xml, "ycood"),
    spectralType = read_value(system_xml, "spectralType")
  )
  
  # if not a connector add a primarySlot
  if(!connector) {
    planetary_system$primarySlot <- read_value(system_xml, "primarySlot")
  }
  
  # add system-level events
  if(!is.null(system_event_xml)) {
    system_events <- map(xml_find_all(system_event_xml, "event"), read_event)
    if(!is_empty(system_events)) {
      planetary_system$event <- system_events
    }
  }
  
  # insert planet events into planet xml
  planets <- xml_find_all(system_xml, "planet")
  if(!is.null(system_event_xml)) {
    planet_events <- xml_find_all(system_event_xml, "planet")
    for(i in 1:length(planet_events)) {
      sysPos <- as.numeric(xml_text(xml_find_first(planet_events[[i]], "sysPos")))
      current_events <- xml_find_all(planet_events[[i]], "event")
      for(event in current_events) {
        xml_add_child(planets[[sysPos]], event)
      }
    }
  }
  if(!is.null(system_name_change_xml)) {
    planet_name_change <- xml_find_all(system_name_change_xml, "planet")
    for(i in 1:length(planet_name_change)) {
      sysPos <- as.numeric(xml_text(xml_find_first(planet_name_change[[i]], "sysPos")))
      current_name_change <- xml_find_all(planet_name_change[[i]], "event")
      for(name_change in current_name_change) {
        xml_add_child(planets[[sysPos]], name_change)
      }
    }
  }
  
  # add planet information
  system_planets <- map(planets, read_planet)
  if(!is_empty(system_planets)) {
    planetary_system$planet <- system_planets
  }
  
  return(planetary_system)
}

# subfunction for reading in the data for a specific planet
read_planet <- function(planet_xml) {
  
  # start planet list with the basics
  planet <- list(
    name = read_value(planet_xml, "name"),
    type = read_value(planet_xml, "type"),
    orbitalDist = read_value(planet_xml, "orbitalDist"),
    sysPos = read_value(planet_xml, "sysPos"),
    icon = read_value(planet_xml, "icon")
  )
  
  # these ones may or may not be present
  for(planet_characteristic in planet_optional) {
    planet_value <- read_value(planet_xml, planet_characteristic)
    if(length(planet_value) > 1 || !is.na(planet_value)) {
      planet[[planet_characteristic]] <- planet_value
    }
  }
  
  # check for landmasses
  landmasses <- map(xml_find_all(planet_xml, "landMass"), read_landmass)
  if(!is_empty(landmasses)) {
    planet$landmass = landmasses
  }
  
  # check for satellites
  satellites <- map(xml_find_all(planet_xml, "satellite"), read_satellite)
  if(!is_empty(satellites)) {
    planet$satellite = satellites
  }
  
  # now look for planetary events and add them
  planet_events <- map(xml_find_all(planet_xml, "event"), read_event)
  if(!is_empty(planet_events)) {
    planet$event = planet_events
  }
  
  return(planet)
}

# subfunction for reading in a single event date
read_event <- function(events_xml) {
  
  # get name of each lement
  element_names <- xml_name(xml_children(events_xml))
  
  # get year to feed into read_value function
  year <- events_xml |>
    xml_find_first("date") |>
    xml_text() |>
    as_date() |>
    year()
  
  # read in values to a list
  events <- list()
  for(element_name in element_names) {
    events[[element_name]] <- read_value(events_xml, element_name, year)
  }
  
  return(events)
}

# subfunction for reading in landmass data
read_landmass <- function(landmass_xml) {
  
  landmass_str <- xml_text(landmass_xml)
  source <- xml_attr(landmass_xml, "source")
  
  # we may need to split landmass into name and capital city
  name <- str_extract(landmass_str, "^[^\\(]+") |> str_trim()
  capital <- str_extract(landmass_str, "(?<=\\().*?(?=\\))")
  
  if(is.na(source) || source == "noncanon") {
    if(is.na(capital)) {
      return(list(name = name))
    } else if(is.na(name)) {
      return(list(capital = capital))
    } else {
      return(list(name = name, capital = capital))
    }
  } else {
    if(is.na(capital)) {
      return(list(name = list(source = source, value = name)))
    } else if(is.na(name)) {
      return(list(capital = list(source = source, value = capital)))
    }  else {
      return(list(name = list(source = source, value = name),
                  capital = list(source = source, value = capital)))
    }
  }
}

# subfunction for reading in satellite data
read_satellite <- function(satellite_xml) {
  
  name <- read_value(satellite_xml, "name")
  size <- read_value(satellite_xml, "size")
  icon <- read_value(satellite_xml, "icon")
  
  return(list(name = name, size = size, icon = icon))
}