# These functions will read in a given xml format and turn it into a set of 
# embedded lists that can be processed by the yaml library.


# key values in this vector should be split into a source and value pair
values_sourceable <- c("ring", "sysPos", "diameter", "dayLength", "temperature",
                       "water", "smallMoons", "gravity", "density", 
                       "yearLength", "spectralType", "name", "type", 
                       "pressure", "atmosphere", "composition", "lifeForm",
                       "size", "faction", "population", "hpg", 
                       "socioIndustrial", "nadirCharge", "zenithCharge")

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

# TODO: figure out landmass which needs an additional hack-ey check

read_value <- function(xml_data, value_name) {
  
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
  
  # do we need to split this into source and value?
  if(value_name %in% values_sourceable) {
    source <- xml_attr(value_node, "source")
    if(is.na(source)) {
      source <- "noncanon"
    }
    return(list(source = source, value = value))
  } else {
    return(value)
  }
  
}

# the primary function
read_planetary_system <- function(id) {
  
  # retrieve the system information
  system_xml <- all_systems[[id]]
  system_event_xml <- all_system_events[[id]]
  system_name_change_xml <- all_system_name_change[[id]]

  # start the planetary system list with basic information
  planetary_system <- list(
    id = id,
    sucsId = read_value(system_xml, "sucsId"),
    xcood = read_value(system_xml, "xcood"),
    ycood = read_value(system_xml, "ycood"),
    spectralType = read_value(system_xml, "spectralType"),
    primarySlot = read_value(system_xml, "primarySlot")
  )
  
  # add system-level events
  system_events <- map(xml_find_all(system_event_xml, "event"), read_event)
  if(!is_empty(system_events)) {
    planetary_system$event <- system_events
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
  
  # read in values to a list
  events <- list()
  for(element_name in element_names) {
    events[[element_name]] <- read_value(events_xml, element_name)
  }
  
  return(events)
}

# subfunction for reading in satellite data
read_satellite <- function(satellite_xml) {
  
  name <- read_value(satellite_xml, "name")
  size <- read_value(satellite_xml, "size")
  icon <- read_value(satellite_xml, "icon")
  
  return(list(name = name, size = size, icon = icon))
}