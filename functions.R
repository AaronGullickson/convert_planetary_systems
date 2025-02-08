
# Functions to read in xml and save in list format ------------------------

read_planetary_system <- function(file) {
  
  system_xml <- xml2::read_xml(file)
  
  id <- xml2::xml_text(xml2::xml_find_first(system_xml, "id"))
  sucsId <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "sucsId")))
  x <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "xcood")))
  y <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "ycood")))
  spectralType <- xml2::xml_text(xml2::xml_find_first(system_xml, "spectralType"))
  primarySlot <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "primarySlot")))
  
  planetary_system <- list(id = id,
                           sucsId = sucsId,
                           x = x,
                           y = y,
                           spectralType = spectralType,
                           primarySlot = primarySlot)
  
  system_events <- purrr::map(xml2::xml_find_all(system_xml, "event"),
                              read_event)
  
  if(!purrr::is_empty(system_events)) {
    planetary_system$event <- system_events
  }
  
  system_planets <- purrr::map(xml2::xml_find_all(system_xml, "planet"),
                               read_planet)
  
  if(!purrr::is_empty(system_planets)) {
    planetary_system$planet <- system_planets
  }
  
  return(planetary_system)
  
}

read_planet <- function(planet_xml) {
  
  name <- xml2::xml_text(xml2::xml_find_first(planet_xml, "name"))
  type <- xml2::xml_text(xml2::xml_find_first(planet_xml, "type"))
  orbitalDist <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "orbitalDist")))
  sysPos <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "sysPos")))
  icon <- xml2::xml_text(xml2::xml_find_first(planet_xml, "icon"))
  
  planet <- list(name = name,
                 type = type,
                 orbital_dist = orbitalDist,
                 sysPos = sysPos,
                 icon = icon)
  
  # these ones may or may not be present
  pressure <- xml2::xml_text(xml2::xml_find_first(planet_xml, "pressure"))
  if(!is.na(pressure)) { planet$pressure <- pressure }
  
  atmosphere <- xml2::xml_text(xml2::xml_find_first(planet_xml, "atmosphere"))
  if(!is.na(atmosphere)) { planet$atmosphere <- atmosphere }
  
  composition <- xml2::xml_text(xml2::xml_find_first(planet_xml, "composition"))
  if(!is.na(composition)) { planet$composition <- composition }
  
  gravity <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "gravity")))
  if(!is.na(gravity)) { planet$gravity <- gravity }
  
  dayLength <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "dayLength")))
  if(!is.na(dayLength)) { planet$dayLength <- dayLength }
  
  diameter <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "diameter")))
  if(!is.na(diameter)) { planet$diameter <- diameter }
  
  density <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "density")))
  if(!is.na(density)) { planet$density <- density }
  
  ring <- xml2::xml_text(xml2::xml_find_first(planet_xml, "ring"))
  if(!is.na(ring)) { planet$ring <- ring }
  
  smallMoons <- as.numeric(xml2::xml_text(xml2::xml_find_first(planet_xml, "smallMoons")))
  if(!is.na(smallMoons)) { planet$smallMoons <- smallMoons }
  
  # check for satellites
  satellites <- purrr::map(xml2::xml_find_all(planet_xml, "satellite"),
                           read_satellite)
  
  if(!purrr::is_empty(satellites)) {
    planet$satellite = satellites
  }
  
  # now look for planetary events and add them
  planet_events <- purrr::map(xml2::xml_find_all(planet_xml, "event"),
                              read_event)
  
  if(!purrr::is_empty(planet_events)) {
    planet$event = planet_events
  }
  
  return(planet)
  
}

read_event <- function(events_xml) {
  
  # get values
  values <- xml2::xml_text(xml2::xml_children(events_xml))
  # name the values
  names(values) <- xml2::xml_name(xml2::xml_children(events_xml))
  # coerce to a list
  values <- as.list(values)
  
  # convert numbers back to numeric values
  values <- purrr::map(values, function(x) {
    ifelse(stringr::str_detect(x, "\\D"), x, as.numeric(x))
  })
  
  return(values)
  
}

read_satellite <- function(satellite_xml) {
  
  name <- xml2::xml_text(xml2::xml_find_first(satellite_xml, "name"))
  size <- xml2::xml_text(xml2::xml_find_first(satellite_xml, "size"))
  icon <- xml2::xml_text(xml2::xml_find_first(satellite_xml, "icon"))
  
  return(list(name = name, size = size, icon = icon))
  
}