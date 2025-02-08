# These functions will read in a given xml format and turn it into a set of 
# embedded lists that can be processed by the yaml library.


# put key values here that need to be transformed for certain values that 
# show up in events
values_logical <- c("nadirCharge", "zenithCharge")
values_integer <- c("")
values_double <- c("population")

# the primary function
read_planetary_system <- function(id) {
  
  system_xml <- all_systems[[id]]
  system_event_xml <- all_system_events[[id]]
  
  sucsId <- as.integer(xml_text(xml_find_first(system_xml, "sucsId")))
  x <- as.numeric(xml_text(xml2::xml_find_first(system_xml, "xcood")))
  y <- as.numeric(xml_text(xml_find_first(system_xml, "ycood")))
  spectralType <- xml_text(xml_find_first(system_xml, "spectralType"))
  primarySlot <- as.integer(xml_text(xml_find_first(system_xml, "primarySlot")))
  
  planetary_system <- list(id = id,
                           sucsId = sucsId,
                           x = x,
                           y = y,
                           spectralType = spectralType,
                           primarySlot = primarySlot)
  
  system_events <- map(xml_find_all(system_event_xml, "event"), read_event)
  if(!is_empty(system_events)) {
    planetary_system$event <- system_events
  }
  
  # insert planet events into planet xml
  planets <- xml_find_all(system_xml, "planet")
  planet_events <- xml_find_all(system_event_xml, "planet")
  for(i in 1:length(planet_events)) {
    sysPos <- as.numeric(xml_text(xml_find_first(planet_events[[i]], "sysPos")))
    current_events <- xml_find_all(planet_events[[i]], "event")
    for(event in current_events) {
      xml_add_child(planets[[sysPos]], event)
    }
  }
  
  system_planets <- map(planets, read_planet)
  if(!is_empty(system_planets)) {
    planetary_system$planet <- system_planets
  }
  
  return(planetary_system)
}

# subfunction for reading in the data for a specific planet
read_planet <- function(planet_xml) {
  
  name <- xml_text(xml_find_first(planet_xml, "name"))
  type <- xml_text(xml_find_first(planet_xml, "type"))
  orbitalDist <- as.numeric(xml_text(xml_find_first(planet_xml, "orbitalDist")))
  sysPos <- as.integer(xml_text(xml_find_first(planet_xml, "sysPos")))
  icon <- xml_text(xml_find_first(planet_xml, "icon"))
  
  planet <- list(name = name,
                 type = type,
                 orbitalDist = orbitalDist,
                 sysPos = sysPos,
                 icon = icon)
  
  # these ones may or may not be present
  pressure <- xml_text(xml_find_first(planet_xml, "pressure"))
  if(!is.na(pressure)) { planet$pressure <- pressure }
  
  atmosphere <- xml_text(xml_find_first(planet_xml, "atmosphere"))
  if(!is.na(atmosphere)) { planet$atmosphere <- atmosphere }
  
  composition <- xml_text(xml_find_first(planet_xml, "composition"))
  if(!is.na(composition)) { planet$composition <- composition }
  
  gravity <- as.numeric(xml_text(xml_find_first(planet_xml, "gravity")))
  if(!is.na(gravity)) { planet$gravity <- gravity }
  
  diameter <- as.integer(xml_text(xml_find_first(planet_xml, "diameter")))
  if(!is.na(diameter)) { planet$diameter <- diameter }
  
  density <- as.numeric(xml_text(xml_find_first(planet_xml, "density")))
  if(!is.na(density)) { planet$density <- density }
  
  dayLength <- as.integer(xml_text(xml_find_first(planet_xml, "dayLength")))
  if(!is.na(dayLength)) { planet$dayLength <- dayLength }
  
  yearLength <- as.integer(xml_text(xml_find_first(planet_xml, "yearLength")))
  if(!is.na(yearLength)) { planet$yearLength <- yearLength }
  
  temperature <- as.integer(xml_text(xml_find_first(planet_xml, "temperature")))
  if(!is.na(temperature)) { planet$temperature <- temperature }
  
  water <- as.integer(xml_text(xml_find_first(planet_xml, "water")))
  if(!is.na(water)) { planet$water <- water }
  
  lifeForm <- xml_text(xml_find_first(planet_xml, "lifeForm"))
  if(!is.na(lifeForm)) { planet$lifeForm <- lifeForm }
  
  description <- xml_text(xml_find_first(planet_xml, "desc"))
  if(!is.na(description)) { planet$description <- description }
  
  ring <- as.logical(xml_text(xml_find_first(planet_xml, "ring")))
  if(!is.na(ring)) { planet$ring <- ring }
  
  smallMoons <- as.integer(xml_text(xml_find_first(planet_xml, "smallMoons")))
  if(!is.na(smallMoons)) { planet$smallMoons <- smallMoons }
  
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
  
  # get values
  values <- xml_text(xml_children(events_xml))
  # get name of element
  element_names <- xml_name(xml_children(events_xml))
  # name the values
  names(values) <- element_names
  # coerce to a list
  values <- as.list(values)
  
  # convert values to appropriate types as needed - default is character string
  values <- map2(values, as.list(element_names), function(x, y) {
    x <- ifelse(y %in% values_logical, as.logical(x), x)
    x <- ifelse(y %in% values_double, as.double(x), x)
    x <- ifelse(y %in% values_integer, as.integer(x), x)
    return(x)
  })
  
  return(values)
}

# subfunction for reading in satellite data
read_satellite <- function(satellite_xml) {
  
  name <- xml_text(xml_find_first(satellite_xml, "name"))
  size <- xml_text(xml_find_first(satellite_xml, "size"))
  icon <- xml_text(xml_find_first(satellite_xml, "icon"))
  
  return(list(name = name, size = size, icon = icon))
}