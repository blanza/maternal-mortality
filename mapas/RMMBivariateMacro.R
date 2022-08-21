### ------------------------------------------------- -###
###  Mapas Bivariados da Razao de Mortalidade Materna  ###  
###  por Macro Regiao de Saude com ggplot2 and sf      ###
###  usando rotina do Timo Grossenbacher             ###
### ------------------------------------------------- -###

# some constants
default_font_color <- "#4e4d47"
default_background_color <- "#f5f5f2"
default_font_family <- "serif"

# define options for saving plots
knitr::opts_chunk$set(
  out.width = "100%",
  dpi = 600,
  fig.width = 6,
  fig.height = 8,
  fig.path = "https://timogrossenbacher.ch/wp-content/uploads/2019/04/bm-",
  strip.white = T,
  dev = "png",
  dev.args = list(png = list(bg = default_background_color))
)


detach_all_packages <- function() {
  basic_packages_blank <-  c("stats",
                             "graphics",
                             "grDevices",
                             "utils",
                             "datasets",
                             "methods",
                             "base")
  basic_packages <- paste("package:", basic_packages_blank, sep = "")
  
  package_list <- search()[
    ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  
  package_list <- setdiff(package_list, basic_packages)
  
  if (length(package_list) > 0)  for (package in package_list) {
    detach(package, character.only = TRUE, unload = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detach_all_packages()

# suppress scientific notation
options(scipen = 999)

# unload global rstudioapi and knitr again to avoid conflicts with checkpoint
# this is only necessary if executed within RStudio
# outside of RStudio, namely in the knit.sh script, this causes RMarkdown
# rendering to fail, thus should not be executed there
if (Sys.getenv("RSTUDIO") == "1"){
  detach_all_packages()
}

library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)
library(extrafont)
library(dplyr)

# Definir diretorio onde programa e bases de dados estao localizados
setwd("C:/Users/pinhe/OneDrive/Desktop/Mortalidade Materna/MapasBivariados")

# Ler base de dados
data <- read.csv("RMMQ_Macro.csv")
BaseVarExp <- read.csv("BaseVarExp.csv")

# Definir/escolher quais variáveis serão correlacionadas
data <- data %>% add_column(RMM=data$RMM20142018, VarExp=BaseVarExp$MIDH2010, )
data <- data %>% rename(cod_macro = macrocode)
data <- as.data.frame(data)
sapply(data,class)

# All shapefiles need to have the same projection
# read estate borders
states <- read_sf("C:/Users/pinhe/OneDrive/Desktop/Mortalidade Materna/MapasBivariados/input/UFEBRASIL.shp")
Macro  <- read_sf("C:/Users/pinhe/OneDrive/Desktop/Mortalidade Materna/MapasBivariados/input/macro.shp")

### Join Geodata with Thematic Data
Macro <- left_join(Macro, data, by="cod_macro")

## Define a Map Theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 10, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 8, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

## Create a Bivariate Choropleth
### Create the Bivariate Color Scale
# create 3 buckets for CMRCVD
quantiles_RMM <- data %>%
  pull(RMM) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for PIB
quantiles_VarExp <- data %>%
  pull(VarExp) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for RMM and blue for mean PIB
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high RMM, high PIB
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low RMM, high PIB
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium RMM, medium PIB
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high RMM, low PIB
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low RMM, low PIB
) %>%
  gather("group", "fill")

### Join Color Codes to the Data
###  The municipalities are put into the appropriate class corresponding to their average income and income (in-)equality.
# cut into groups defined above and join fill
Macro %<>%
  mutate(
    RMM_quantiles = cut(
      RMM,
      breaks = quantiles_RMM,
      include.lowest = TRUE
    ),
    VarExp_quantiles = cut(
      VarExp,
      breaks = quantiles_VarExp,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(RMM_quantiles), "-",
      as.numeric(VarExp_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")

### Draw the Map
map <- ggplot(
  # use the same dataset as before
  data = Macro
) +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # color municipalities according to their gini / income combination
  geom_sf(
    aes(
      fill = fill
    ),
    # use thin white stroke for municipalities
    color = "white",
    size = 0.1
  ) +
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # use thicker white stroke for cantons
  geom_sf(
    data = states,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Mapa Bivariado da Razão de Mortalidade Materna e IDH em 2010",
       subtitle = paste0("Macroregiões de Saúde, 2014-2018")) +
  
  # add the theme
  theme_map()

### Draw the Legend
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("RMM", "VarExp"), sep = " - ") %>%
  mutate(RMM = as.integer(RMM),
         VarExp = as.integer(VarExp))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = RMM,
      y = VarExp,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Maior RMM",
       y = "Maior IDH2010") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8)
  ) +
  # quadratic tiles
  coord_fixed()

### Combine Map and Legend
ggdraw( ) +
  draw_plot(map, 0, 0, 1, 1) + 
  draw_plot(legend, 0.55, 0.1, 0.22, 0.22)
