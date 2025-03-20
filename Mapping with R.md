---
title: "Mapping with R"
author:
- name: Shaoke Lei
  email: shaokelei@gmail.com
  output: github_document
---


#```{r setup, include = FALSE}
#knitr::opts_chunk$set(fig.width = 12, fig.height = 8, autodep = TRUE, message = FALSE, warning = FALSE)
#options(width = 150)
#```


```{r include = FALSE}
wd <- "C:/Users/SHAOKE/OneDrive - The University of Melbourne/x. MCRI/Mapping_share/Mapping with R"
```


# Digital boundaries files
Australian Statistical Geography Standard (ASGS) digital boundaries are available in either the OGC GeoPackage, or ESRI shapefile formats, which are available to download from [the ABS website](https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files). In the following, we use shapefile to create the map.

# Mapping with plot

```{r}
library(sf)
library(RColorBrewer)
library(dplyr)
```

```{r results = 'hide'}
aus <- st_read(dsn = file.path(wd, "LGA_2022_AUST_GDA2020_SHP"), layer = "LGA_2022_AUST_GDA2020")
```

Create a new column 'Number' with simulated values
```{r}
set.seed(1000)
aus <- aus %>% mutate(Number = abs(rnorm(nrow(aus), 100,  50)))
```

If we only VIC map, then remove data for other states
```{r}
vic <- filter(aus, STE_NAME21 == "Victoria")
pal <- brewer.pal(5, "OrRd")
plot(vic["Number"], breaks = "quantile", nbreaks = 5, pal = pal)
```

```{r include = FALSE}
rm(list=ls())
wd <- "C:/Users/SHAOKE/OneDrive - The University of Melbourne/x. MCRI/Mapping_share/Mapping with R"
```

# Mapping with ggplot2

```{r}
#library(rgdal)
library(dplyr)
library(ggplot2)
library(broom)
```

Load Australian LGA shapefile data: "LGA_2022_AUST_GDA2020_SHP" is the downloaded folder from ABS and "LGA_2022_AUST_GDA2020" is the shapefile file in that folder with a file extension <span style="color:red">.shp</span>

```{r eval = FALSE}
#wd <- "your_path_to_data_source"
```

```{r results = 'hide'}
aus <- read_sf(dsn = file.path(wd, "LGA_2022_AUST_GDA2020_SHP"), layer = "LGA_2022_AUST_GDA2020")
```

This is the map from the shapfile data
```{r message = FALSE}
ggplot() + 
    geom_sf(data = aus, fill = "#69b3a2", color = "white") +
    theme_void() 
```

Create a new column 'Number' with simulated values
```{r}
set.seed(1000)
aus <- aus %>% mutate(Number = abs(rnorm(nrow(aus), 100,  50)))
```

Create a data table that contain the long/lat of three hospitals if we want to add markers on the map 
```{r}
hospital <- tibble(hospital = c("RCH", "Sunshine Hospital", 
                                "Werribee Mercy Hospital", 
                                "Northern Hospital Epping"),
                   lat = c(-37.79, -37.76, -37.89, -37.65), 
                   long = c(144.95, 144.82, 144.70, 145.02))
```

Now join the two data sets: data with coordinates and data with Number
```{r}
#aus@data$id <- rownames(aus@data)
#aus_df <- broom::tidy(aus)
#df <- left_join(aus_df, aus@data, by = "id")
```

For VIC map, remove data for other states
```{r}
df_vic <- filter(aus, STE_NAME21 == "Victoria")


VIC_poa <- ggplot(data = df_vic) + 
		           geom_sf(aes(fill = Number)) + 
  			       geom_point(data = hospital, 
  			                  aes(x = long, y = lat, group = hospital, shape = hospital), 
  			                  size = 1.5) +
		           scale_fill_gradient(low = "#ffffcc", 
		                               high = "#ff0000",
                                   space = "Lab", 
		                               na.value = "grey50",
                                   guide = "colourbar") +
		           ggtitle("you_data_map")

VIC_poa		     
```

# mapping with leaflet

```{r include = FALSE}
rm(list=ls())
wd <- "C:/Users/SHAOKE/OneDrive - The University of Melbourne/x. MCRI/Mapping_share/Mapping with R"
```

```{r leaflet}
library(sf)
library(dplyr)
library(leaflet) 
```

```{r results = 'hide'}
aus <- st_read(dsn = file.path(wd, "LGA_2022_AUST_GDA2020_SHP"), layer = "LGA_2022_AUST_GDA2020")
```

Create simulated data column. <span style="color:red">For real data, it is mostly like to join your data with the column "LGA_CODE22" from data "aus". </span>
```{r}
set.seed(1000)
aus <- aus %>% mutate(Number = abs(rnorm(nrow(aus), 100,  50)))
```

```{r out.width = "100%"}
vic <- filter(aus, STE_NAME21 == "Victoria")

vic <- st_transform(vic, 4326)

pal <- colorNumeric(palette = c("#ffffcc", "#ff0000"), domain = vic$Total, na.color = "#90000000")

p_popup <- paste0("Total number of presentation: </strong>", vic$Total)

vic_map <- leaflet(vic) %>%
                addTiles() %>%
                addPolygons(
                    stroke = TRUE, # remove polygon borders
                    weight = 0.5,
                    fillColor = ~pal(Number), # set fill color with function from above and value
                    fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
                    popup = p_popup) %>% # add popup
                addMarkers(~144.95, ~-37.79, popup = ~"RCH", label = ~"RCH") %>%
                addMarkers(~144.82, ~-37.76, popup = ~"Sunshine Hospital", label = ~"Sunshine Hospital") %>%
                addMarkers(~144.70, ~-37.89, popup = ~"Werribee Mercy Hospital", label = ~"Werribee Mercy Hospital") %>%
                addMarkers(~145.02, ~-37.65, popup = ~"Northern Hospital Epping", label = ~"Northern Hospital Epping") %>%
                addLegend("topright", 
                             pal = pal, 
                             values = ~Number,
                             title = "Total number of presentation by post code",
                             opacity = 1) %>%
                setView(lng = 144.95, lat = -37.79, zoom = 8) %>%
                setMaxBounds(lng1 = 144
                             ,lat1 = -37
                             ,lng2 = 145
                             ,lat2 = -36)
vic_map
```

# Mapping with Shiny

```{r include = FALSE}
rm(list=ls())
wd <- "C:/Users/SHAOKE/OneDrive - The University of Melbourne/x. MCRI/Mapping_share/Mapping with R"
```

```{r shiny}
library(sf)
library(dplyr)
library(leaflet) 
library(shiny)
```

```{r result = 'hide'}
aus <- st_read(dsn = file.path(wd, "LGA_2022_AUST_GDA2020_SHP"), layer = "LGA_2022_AUST_GDA2020")
```

```{r}
vic <- filter(aus, STE_NAME21 == "Victoria")

set.seed(1000)
num = round(abs(rnorm(nrow(vic), 100,  50)), 0)


simulated_data <- bind_rows(
                      tibble(LGA_CODE22 = vic$LGA_CODE22,
                             number = num,
                             year = 2021,
                             triage = "1, 2, 3"
                      ),
                      tibble(LGA_CODE22 = vic$LGA_CODE22,
                             number = num*2,
                             year = 2021,
                             triage = "4, 5"         
                      ),             
                      tibble(LGA_CODE22 = vic$LGA_CODE22,
                             number = num*3,
                             year = 2022,
                             triage = "1, 2, 3"         
                      ),          
                      tibble(LGA_CODE22 = vic$LGA_CODE22,
                             number = num*4,
                             year = 2022,
                             triage = "4, 5"         
                      )
                  )

vic <- left_join(vic, simulated_data, by = "LGA_CODE22")

vic <- st_transform(vic, 4326)
```

Specify shiny user interface and server.

```{r}
ui <- bootstrapPage(tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
                    leafletOutput("map", width = "100%", height = "100%"),
                    # position and properties of the time slider
                    absolutePanel(bottom = 100, 
                                  right = 100, 
                                  draggable = TRUE,
                                  # slider title, step increments, and ticks
                                  sliderInput("year", 
                                              "Year",
                                              ticks = FALSE, 
                                              min = min(vic$year, na.rm = TRUE), 
                                              max = max(vic$year, na.rm = TRUE), 
                                              value = max(vic$Year, na.rm = TRUE), 
                                              step = 1,
                                              animate = animationOptions(interval = 5000, loop = TRUE)),
                                  selectInput("triage", 
                                              "Triage Category:", 
                                              choices = c("1, 2 and 3" = "1, 2, 3", "4 and 5" = "4, 5")),
                                  )
                    )    
```

```{r}
server <- function(input, output, session) {
    filteredData = reactive({
                        vic %>% filter(year == input$year, 
                                       triage == input$triage)
    })
    
    pal = colorNumeric(palette = c("#ffffcc", "#ff0000"), domain = vic$number, na.color = "#90000000")
    
    output$map = renderLeaflet({
      
                      leaflet(vic) %>%
                          addTiles() %>%
                          addLegend("topright", 
                              pal = pal, 
                              values = ~ number,
                              title = "Your title",
                              opacity = 1) %>%
                          addMarkers(~144.95, ~-37.79, 
                                     popup = ~"RCH",
                                     label = ~"RCH") %>%
                          addMarkers(~144.82, ~-37.76, 
                                     popup = ~"Sunshine Hospital", 
                                     label = ~"Sunshine Hospital") %>%
                          addMarkers(~144.70, ~-37.89, 
                                     popup = ~"Werribee Mercy Hospital", 
                                     label = ~"Werribee Mercy Hospital") %>%
                          addMarkers(~145.02, ~-37.65, 
                                     popup = ~"Northern Hospital Epping", 
                                     label = ~"Northern Hospital Epping") %>%
        
                          setView(lng = 144.95, lat = -37.79, zoom = 11)
            
    })

    observe({
        p_popup = paste0("LGA: </strong>", 
                         filteredData()$LGA_NAME22, 
                         "; ", 
                         "Number: </strong>", 
                         filteredData()$number
                  )
        
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addPolygons(
                stroke = FALSE, 
                weight = 0.9,
                fillColor = ~pal(number), 
                fillOpacity = 0.8, smoothFactor = 0.5,
                popup = p_popup
            )
    
    })

}

```

```{r shiny2, eval = FALSE}
shinyApp(ui, server)
```
