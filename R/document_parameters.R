#Data directories
image.dir <- here("images")
gis.dir <- here("gis")

#General inline text input for report
council <- "New England Fishery Management Council"
council_abbr <- "NEFMC"

#Region identifiers
epu <- "New England"
epu_abbr <- c("GOM","GB")
region <- "New England"
region_abbr <- "NE" #Some commercial data organized by "MA" or "NE" regions, not by EPU 

#CRS
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Coastline shapefile
coast <- ne_countries(scale = 10,
                      continent = "North America",
                      returnclass = "sf") %>%
  sf::st_transform(crs = crs)

#State polygons
ne_states <- ne_states(country = "united states of america",
                       returnclass = "sf") %>%
  sf::st_transform(crs = crs)

#EPU shapefile
epu_sf <- ecodata::epu_sf %>% 
  filter(EPU %in% c("MAB","GB","GOM"))

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -77
xmax = -65
ymin = 36
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

#Time series constants
shade.alpha <- 0.3
shade.fill <- "lightgrey"
lwd <- 1
pcex <- 2
trend.alpha <- 0.5
trend.size <- 2
hline.size <- 1
hline.alpha <- 0.35
hline.lty <- "dashed"
label.size <- 5
hjust.label <- 1.5
letter_size <- 4
feeding.guilds <- c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos")
x.shade.min <- 2009
x.shade.max <- 2018

#commercial facet names
comm_facet_names <- list("Apex predators" = expression("Apex predators"),
                    "Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))