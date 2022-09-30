

library(sf)
library(raster)
library(ggplot2)
SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

RNT_Amortigua = st_read("SHP/RNT-Amortigua.geojson")  %>% st_as_sf()
RNT_Amortiguamiento <- st_transform(RNT_Amortigua ,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Marco_RNT= st_as_sfc(st_bbox(RNT_Amortiguamiento))

MDD_DIS = st_read("SHP/MDD_DIS.geojson")  %>% st_as_sf()
MDD_DI  <- st_transform(MDD_DIS,
                                    crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

CP <- st_read("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))

Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

Peru1  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru2  <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD  =  subset(Peru2 , NAME_1 == "Madre de Dios")

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru1 , fill="gray", color="black", size=0.05)+
  geom_sf(data = MDD, fill="black", color="black", size=0.01)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")

A=ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data=Peru1, fill="white", color="black", size=0.1)+
  geom_sf(data=MDD, fill="gray", color="black", size=0.05)+
  geom_sf(data = RNT_Amortiguamiento, fill="black", size=0.1)+
  geom_sf(data = Marco_RNT, fill=NA, size=1)+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+

  annotate(geom = "text", x = -71.5, y = -13.2, hjust = 0, vjust = 1, 
           label = "CUSCO",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.5, y = -13.5, hjust = 0, vjust = 1, 
           label = "PUNO",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -68.9, y = -11.5, hjust = 0, vjust = 1, angle=300,
           label = "BOLIVIA",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10.5, hjust = 0, vjust = 1, 
           label = "BRASIL",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -72, y = -10.5, hjust = 0, vjust = 1, 
           label = "UCAYALI",size = 3, family="serif", color = 
             "black",  fontface="italic")
B=ggplot()+
  geom_sf(data=Peru1, fill="white", color="black", size=0.1)+
  geom_sf(data=MDD_DI, fill="gray", color="black", size=0.1)+
  geom_sf(data = RNT_Amortiguamiento, fill="black", alpha=0.6)+
  coord_sf(xlim = c(-70.37153, -69.23279), ylim = c(-13.5 ,-12.4)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.2, y = -13.4, hjust = 0, vjust = 1, 
           label = "PUNO",size = 3, family="serif", color = 
             "black",  fontface="italic")+
    annotate(geom = "text", x = -70, y = -13.2, hjust = 0, vjust = 1, 
             label = "Inambari",size = 3, family="serif", color = 
               "black",  fontface="italic")+
    annotate(geom = "text", x = -69.4, y = -13, hjust = 0, vjust = 1, 
             label = "Tambopata",size = 3, family="serif", color = 
               "black",  fontface="italic")+
    annotate(geom = "text", x = -69.8, y = -12.6, hjust = 0, vjust = 1, 
             label = "Laberinto",size = 3, family="serif", color = 
               "black",  fontface="italic")+
    annotate(geom = "text", x = -70.3, y = -12.6, hjust = 0, vjust = 1, 
             label = "Madre de Dios",size = 3, family="serif", color = 
               "black",  fontface="italic")

library(ggspatial)

library(elevatr)
elev = get_elev_raster(RNT_Amortiguamiento, z=12)

Poligo_alt    <- crop(elev, RNT_Amortiguamiento)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, RNT_Amortiguamiento)


slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(elev)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

Cober <- raster("Raster/ESA_WorldCover_10m_2020_v100_S15W072_Map.tif")
plot(Cober)
Cober_Amor    <- crop(Cober, RNT_Amortiguamiento)                           #   
Cober_Amor   <- Cober_Amor <- mask(Cober_Amor, RNT_Amortiguamiento)
plot(Cober_Amor)

Cober.pa        <-  rasterToPoints(Cober_Amor)
Cober.pa_a      <-  data.frame(Cober.pa)
colnames(Cober.pa_a) <- c("x","y", "Cober")


col=c(
  "#006400", # Cubierta de árboles
  "#FFBB22", # Matorrales
  "#FFFF4C", # Pastizales
  "#F096FF", # Tierras de cultivo
  "#FA0000", # Construido
  "#B4B4B4", # Vegetación desnuda / escasa
  "#F0F0F0", # Masas de agua permanentes
  "#0064C8", # Humedal herbáceo
  "#0096A0", # Humedal herbáceo
  "#00CF75", # Manglares
  "#FAE6A0" # Musgos y líquenes
)



library(ggnewscale) 
DD=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = RNT_Amortiguamiento, color="black", size=0.1, fill=NA)+
  geom_sf(data=MDD_DI, fill=NA, color="black", size=0.1)+

  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  geom_sf(data = Rio_Poli , color=NA, size=0.01, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=0.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf_text(data =CetroPo, aes (label=nombre), size=2, color="black", family="serif")+
  coord_sf(xlim = c(-70.37153, -69.23279), ylim = c(-13.5 ,-12.4)) +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(
    axis.text.x  = element_text(face="bold", color="black", size=8,
                                family="serif"),
    axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                family="serif",size=8),
    axis.title = element_text(face="bold", color="black"),
    legend.background = element_rect(fill = "white"),
    legend.text=element_text(size=8, family="serif"),
    legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
    legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
    legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
    panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  annotate(geom = "text", x = -70.2, y = -13.4, hjust = 0, vjust = 1, 
           label = "PUNO",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -13.2, hjust = 0, vjust = 1, 
           label = "Inambari",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.4, y = -13, hjust = 0, vjust = 1, 
           label = "Tambopata",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.8, y = -12.6, hjust = 0, vjust = 1, 
           label = "Laberinto",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70.3, y = -12.6, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  guides(fill = guide_legend(nrow = 3, ncol=3))

library(ggpubr)
legend <- get_legend(DD)

D = DD + theme(legend.position = "none")


AA= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = RNT_Amortiguamiento, color="black", size=0.1, fill=NA)+
  geom_sf(data=MDD_DI, fill=NA, color="black", size=0.1)+
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  
  geom_sf(data = Rio_Poli , color=NA, size=0.01, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=0.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf_text(data =CetroPo, aes (label=nombre), size=2, color="black", family="serif")+
  coord_sf(xlim = c(-70.11111, -70.04), ylim = c(-12.95593 ,-12.89384))+
  theme_classic()+
  theme(
    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x =  -70.11, y = -12.9, hjust = 0, vjust = 1, 
           label = "A)",size = 3, family="serif", color = 
             "black",  fontface="italic")


BB= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = RNT_Amortiguamiento, color="black", size=0.1, fill=NA)+
  geom_sf(data=MDD_DI, fill=NA, color="black", size=0.1)+
  
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  geom_sf(data = Rio_Poli , color=NA, size=0.01, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=0.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf_text(data =CetroPo, aes (label=nombre), size=2, color="black", family="serif")+
  coord_sf(xlim = c(-70.02692,  -69.9415), ylim = c(-12.97206,-12.88804)) +
  
  theme_classic()+
  theme(
    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.02, y = -12.9, hjust = 0, vjust = 1, 
           label = "B)",size = 3, family="serif", color = 
             "black",  fontface="italic")

CC= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = RNT_Amortiguamiento, color="black", size=0.1, fill=NA)+
  geom_sf(data=MDD_DI, fill=NA, color="black", size=0.1)+
  
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  geom_sf(data = Rio_Poli , color=NA, size=0.01, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=0.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf_text(data =CetroPo, aes (label=nombre), size=2, color="black", family="serif")+
  coord_sf(xlim = c(-70.05724,  -69.93978), ylim = c(-13.00875,-12.89276)) +
  
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.06, y = -12.9, hjust = 0, vjust = 1, 
           label = "C)",size = 3, family="serif", color = 
             "black",  fontface="italic")

EE= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = RNT_Amortiguamiento, color="black", size=0.1, fill=NA)+
  geom_sf(data=MDD_DI, fill=NA, color="black", size=0.1)+
  
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  geom_sf(data = Rio_Poli , color=NA, size=0.01, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=0.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf_text(data =CetroPo, aes (label=nombre), size=2, color="black", family="serif")+
  coord_sf(xlim = c(-69.89447,  -69.80333), ylim = c(-12.98433,-12.89734)) +
  
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.88, y = -12.9, hjust = 0, vjust = 1, 
           label = "D)",size = 3, family="serif", color = 
             "black",  fontface="italic")

Alt = ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = RNT_Amortiguamiento, color="black", size=0.1, fill=NA)+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-70.4, -69.3), ylim = c(-13.174,-12.69091)) +
  
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
library(cowplot)
Mapa= ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  
  
  draw_plot(D  , width = 15, height = 15,x = -0.3, y = 0)+

  
  draw_plot(SurA , width = 6, height = 6,x = -1, y = 14.5)+
  draw_plot(A , width = 6, height = 6,x = 4, y = 14.5)+
  draw_plot(B , width = 6, height = 6,x = 10, y = 14.5)+
  draw_plot(Alt, width = 12,     height = 12,x = 16, y = 9)+
  
  draw_plot(legend , width = 9, height = 9,x = 17.8, y = 14.5)+
  

  draw_plot(AA , width = 6, height = 6,x = 16, y = 0)+
  draw_plot(BB , width = 6, height = 6,x = 22, y = 0)+
  draw_plot(CC , width = 6, height = 6,x = 16, y = 6)+
  draw_plot(EE , width = 6, height = 6,x = 22, y = 6)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Mapa ,"Mapa/Mapa de clasificacion.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)









































