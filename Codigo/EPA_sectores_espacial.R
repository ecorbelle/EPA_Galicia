## Distribución espacial das afiliacións á seguridade social por sectores da economía
## Datos: Distribución espacial das características da poboación de Galicia por 
## cuadrícula de 1km² (IGE) - https://www.ige.gal/web/mostrar_actividade_estatistica.jsp?idioma=gl&codigo=0201001009
## Eduardo Corbelle, 21 outubro 2020

library(sf)
library(readxl)
library(tmap)
library(stars)

# Mapas ----
ccaa <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/IGN/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/") |> 
  st_transform(25829)
pt <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/Portugal/PRT_adm0.shp") |> 
  st_transform(25829)

# Malla orixinal (LAEA)
malla  <- st_read("Datos/Malla1km.shp") |> 
  st_transform(25829)

# Datos de ocupación (porcentaxe de ocupados por sector)
ocupa  <- read_xls("Datos/AfilSS_2020_cuadriculakm.xls",
                   sheet = "Datos",
                   range = "a3:g10587",
                   col_names = c("cela", "concellos", "persoas",
                                 "p.agric.pesca", "p.industria",
                                 "p.construcion", "p.servizos"),
                   na = "x")

# Para cada columna, reempraza puntos (miles) por nada, logo coma (decimal) por punto
for (i in 3:7) {
  ocupa[[i]] <- ocupa[[i]] |> 
    gsub(pattern = '\\.', replacement = '')    |> 
    gsub(pattern = ',',   replacement = '\\.') |> 
    as.numeric()
}
  
# Datos de ocupación (número de ocupados por sector)
ocupa2 <- data.frame(cela  = ocupa$cela,
                    agric = ocupa$persoas * ocupa$p.agric.pesca / 100,
                    servi = ocupa$persoas * ocupa$p.servizos    / 100,
                    indus = ocupa$persoas * ocupa$p.industria   / 100,
                    const = ocupa$persoas * ocupa$p.construcion / 100)

# Xuntamos a malla con datos de persoas
malla2 <- merge(malla[,"CELLCODE"], ocupa2, 
                by.x = "CELLCODE", by.y = "cela", all = TRUE)

# Rasterizamos en 1x1 km
mallaR <- c(
  st_rasterize(malla2["agric"], dx = 1000, dy = 1000),
  st_rasterize(malla2["servi"], dx = 1000, dy = 1000),
  st_rasterize(malla2["indus"], dx = 1000, dy = 1000),
  st_rasterize(malla2["const"], dx = 1000, dy = 1000))

mallaR[is.na(mallaR)] <- NA # Iguala os píxeles nos que hai menos de 1 cos ocultos por segredo estatítico


# Producimos mapa con tmap
bb <- st_bbox(ccaa[ccaa$CODNUT2 == "ES11",])

mapa <- tm_shape(ccaa, bbox = bb) + 
  tm_polygons(col = "grey65", border.col = "white", lwd = .5) +
  tm_shape(pt) + tm_polygons(col = "grey85", border.col = "white", lwd = .5) +
tm_shape(mallaR, raster.downsample = FALSE) +
  tm_raster(breaks = c(0,1,5,10,100,1000,9000),
            palette = "-viridis",
            title = "Afiliacións/km²") +
  tm_facets(free.scales.raster = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 fun=function(x) formatC(x, digits=0, format="d")),
            panel.labels = c("Agricultura, silvicultura e pesca",
                             "Servizos",
                             "Industria",
                             "Construción"),
            panel.label.size = .8,
            bg.color = "#56B4E950", 
            main.title.size = .9,
            title.size = .9,
            legend.text.size = .9,
            main.title = "Densidade de afiliacións á Seguridade Social, 2020",
            legend.outside = TRUE,
            attr.outside = TRUE,
            attr.position = c(0, .05)) +
  tm_credits(text = "Eduardo Corbelle, 2024. CC-BY 4.0\nDatos: Distribución espacial das características da poboación de Galicia por cuadrícula de 1 km² (IGE)") 

# Gráficos para exportación ----
png("AfilSsEspacial.png", width = 15, height = 10, units = "cm", res = 300)
print(mapa)
dev.off()
