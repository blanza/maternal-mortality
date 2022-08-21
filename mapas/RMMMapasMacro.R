### ------------------------------------------- ###
###  Mapeamento da Razão de Mortalidade Materna ###  
###  por Macro Região de Saúde                  ###
### ------------------------------------------- ###

# Carrega pacotes Requeridos
library(tidyverse)
library(ggthemes)
library(sf)
library(geobr)

# Definir diretório onde programa e bases de dados estao localizados
setwd("C:/Users/pinhe/OneDrive/Desktop/Mortalidade Materna/MapasMacro")

## Ler Base de Dados
OMMacro <- read.csv("OMMacro.csv", header=T, sep=",", dec=".", stringsAsFactors = FALSE)
NVMacro <- read.csv("NVMacro.csv", header=T, sep=",", dec=".", stringsAsFactors = FALSE)
NVMacro <- subset(NVMacro,select= c("macrocode","NV2009","NV2010","NV2011","NV2012","NV2013",
                       "NV2014","NV2015","NV2016","NV2017","NV2018","NV2019"))

#------------------------------------#
# Juntando dados em um unico arquivo #
# -----------------------------------#
OMNV <- left_join(OMMacro, NVMacro, by="macrocode")
RMM <- OMNV %>% mutate(RMM2009=OM2009/NV2009*100000, RMM2010=OM2010/NV2010*100000, 
                       RMM2011=OM2011/NV2011*100000, RMM2012=OM2012/NV2012*100000, 
                       RMM2013=OM2013/NV2013*100000, RMM2014=OM2014/NV2014*100000,
                       RMM2015=OM2015/NV2015*100000, RMM2016=OM2016/NV2016*100000, 
                       RMM2017=OM2017/NV2017*100000, RMM2018=OM2018/NV2018*100000, 
                       RMM2019=OM2019/NV2019*100000)
                       
RMM <- RMM %>% mutate_all(~replace(., is.nan(.), 0))
RMM <- subset(RMM, select=c("macrocode","macrorregiao","RMM2009","RMM2010","RMM2011","RMM2012","RMM2013",
                            "RMM2014","RMM2015","RMM2016","RMM2017","RMM2018","RMM2019"))
View(RMM)
write.csv(RMM,'RMM_Macro.csv', row.names=FALSE)

Estados = read_state(year=2013)
MacroR = read_health_region(year = 2013, macro = T, simplified = F)
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

###------------------------------###
###  Mapas da RMM anual          ###
###------------------------------###

RMM09=RMM$RMM2009
m09= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM09), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2009", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m09

RMM10=RMM$RMM2010
m10= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM10), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2010", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m10

RMM11=RMM$RMM2011
m11= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM11), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2011", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m11

RMM12=RMM$RMM2012
m12= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM12), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2012", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m12

RMM13=RMM$RMM2013
m13= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM13), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2013", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m13

RMM14=RMM$RMM2014
m14= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM14), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2014", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m14

RMM15=RMM$RMM2015
m15= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM15), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2015", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m15

RMM16=RMM$RMM2016
m16= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM16), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2016", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m16

RMM17=RMM$RMM2017
m17= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM17), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2017", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m17

RMM18=RMM$RMM2018
m18= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM18), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2018", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m18

RMM19=RMM$RMM2019
m19= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM19), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna (RMM)\nMacrorregiões de Saúde, 2019", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(0,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m19


###------------------------------###
###  Mapas da RMM por Biênio ###
###------------------------------###
RMMB <- OMNV %>% mutate(RMM20092010=(OM2009+OM2010)/(NV2009+NV2010)*100000, 
                        RMM20112012=(OM2011+OM2012)/(NV2011+NV2012)*100000,
                        RMM20132014=(OM2013+OM2014)/(NV2013+NV2014)*100000, 
                        RMM20152016=(OM2015+OM2016)/(NV2015+NV2016)*100000,
                        RMM20172018=(OM2017+OM2018)/(NV2017+NV2018)*100000,)
RMMB <- RMMB %>% mutate_all(~replace(., is.nan(.), 0))
RMMB <- subset(RMMB, select=c("macrocode","macrorregiao","RMM20092010", "RMM20112012","RMM20132014", "RMM20152016", "RMM20172018"))
View(RMMB)
write.csv(RMMB,'RMMB_Macro.csv', row.names=FALSE)

RMM20092010=RMMB$RMM20092010
m20092010= ggplot() + 
  geom_sf(data=MacroR, aes(fill= RMM20092010), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde, 2009-2010", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20092010

RMM20112012=RMMB$RMM20112012
m20112012= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM20112012), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde, 2011-2012", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20112012

RMM20132014=RMMB$RMM20132014
m20132014= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM20132014), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde, 2013-2014", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20132014

RMM20152016=RMMB$RMM20152016
m20152016= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM20152016), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde, 2015-2016", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20152016

RMM20172018=RMMB$RMM20172018
m20172018= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM20172018), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde, 2017-2018", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20172018


###------------------------------###
###  Mapas da RMM por quinquênio ###
###------------------------------###
RMMQ <- OMNV %>% mutate(RMM20092013=(OM2009+OM2010+OM2011+OM2012+OM2013)/(NV2009+NV2010+NV2011+NV2012+NV2013)*100000, 
                        RMM20142018=(OM2014+OM2015+OM2016+OM2017+OM2018)/(NV2014+NV2015+NV2016+NV2017+NV2018)*100000)
RMMQ <- RMMQ %>% mutate_all(~replace(., is.nan(.), 0))
RMMQ <- subset(RMMQ, select=c("macrocode","macrorregiao","RMM20092013", "RMM20142018"))
View(RMMQ)
write.csv(RMMQ,'RMMQ_Macro.csv', row.names=FALSE)

RMM20092013=RMMQ$RMM20092013
m20092013= ggplot() + 
  geom_sf(data=MacroR, aes(fill= RMM20092013), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde - Quinquênio 2009-2013", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20092013

RMM20142018=RMMQ$RMM20142018
m20142018= ggplot() +
  geom_sf(data=MacroR, aes(fill= RMM20142018), color= NA, size=.15) +
  geom_sf(data=Estados, fill=NA, color="white") +
  labs(title="Razão de Mortalidade Materna Média\nMacrorregiões de Saúde - Quinquênio 2014-2018", size=8, tag = "RMM = número de óbitos maternos por 100.000 nascidos vivos") +
  scale_fill_distiller(palette = "Oranges", name="RMM",direction = 1,limits = c(8,150)) +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"),plot.tag = element_text(color= "black", size = 9, face = "bold"), 
        plot.tag.position = "bottom") +
  no_axis
m20142018






