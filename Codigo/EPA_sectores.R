## Gráfico da evolución do número de ocupados por sectores da economía de Galicia
## Eduardo Corbelle, 21 outubro 2020

library(tidyr)
library(ggplot2)
library(ggokabeito)

# Datos ----
# Período 1976-1995 (datos en miles)
datos1 <- read.csv("Datos/EPA1976-2001.csv", sep=";", dec=",")

colnames(datos1)[3:6] <- c("Agricultura, silvicultura e pesca",
                           "Construción",
                           "Industria",
                           "Servizos")

## Período 1996-2008
datos2 <- read.csv("Datos/EPA1996-2008.csv", sep = ";", dec = ",", skip = 11, header = FALSE)

colnames(datos2) <- c("Periodo",
                      "Agricultura, silvicultura e pesca",
                      "Industria",
                      "Construción",
                      "Servizos")

## Período 2008-2020
datos3 <- read.csv("Datos/EPA2008-2023.csv", sep = ";", dec = ",", skip = 11, header = FALSE)

colnames(datos3) <- c("Periodo",
                      "Agricultura, silvicultura e pesca",
                      "Industria",
                      "Construción",
                      "Servizos")

## Asignación de datas ----
datos1$Ano <- datos1$Ano + as.numeric(substr(datos1$Periodo, 1, 1)) * .25 

datos2$Ano <- as.numeric(substr(datos2$Periodo, 1, 4)) + rep(1:4, 13) * .25

datos3$Ano <- as.numeric(substr(datos3$Periodo, 1, 4)) + rep(1:4, 16) * .25

## Conversión a formato longo ----
datos <- rbind(datos1[1:78, ],
               datos2[1:48, ],
               datos3)

datoslong <- gather(datos[,-2], key = "Sector", value = "Ocupados", -Ano)
datoslong$Sector <- factor(datoslong$Sector, 
                           levels = c("Agricultura, silvicultura e pesca",
                                      "Servizos",
                                      "Industria",
                                      "Construción"))
datoslong$Sector2 <- factor(datoslong$Sector, 
                           levels = rev(c("Agricultura, silvicultura e pesca",
                                      "Industria",
                                      "Construción",
                                      "Servizos")))

## Cálculos en porcentaxe ----
total <- apply(datos[,3:6], 1, sum)
datos2 <- data.frame(datos[,1:2],
                     100*datos[,3:6]/total)
colnames(datos2)[3] <- "Agricultura e pesca"
datos2long <- gather(datos2[,-2], key = "Sector", value = "Ocupados", -Ano)
datos2long$Sector <- factor(datos2long$Sector, 
                           levels = c("Agricultura e pesca",
                                      "Servizos",
                                      "Industria",
                                      "Construción"))
datos2long$Sector2 <- factor(datos2long$Sector, 
                            levels = rev(c("Agricultura silvicultura e pesca",
                                       "Servizos",
                                       "Industria",
                                       "Construción")))

## Gráficos ----
p <- ggplot(datoslong,
            aes(x = Ano, y = Ocupados, colour = Sector)) +
  geom_line() +
  expand_limits(y = 0) +
  ylab("Ocupados (miles)") +
  theme(legend.position = "bottom")

p1 <- ggplot(datoslong, 
             aes(x = Ano, y = Ocupados)) +
  geom_line() + 
  expand_limits(y = 0) +
  ylab("Persoas (miles)") +
  facet_wrap(~Sector) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle(label = "Galicia. Persoas ocupadas por sectores",
          subtitle = "Datos: Enquisa de Poboación Activa, INE") +
  xlab("")

p2 <- ggplot(datos2long,
             aes(x = Ano, y = Ocupados)) +
  geom_line() +
  expand_limits(y = 0) +
  ylab("Persoas (%)") +
  facet_wrap(~Sector) +
  theme_bw() +
  ggtitle(label = "Galicia. Persoas ocupadas por sectores",
          subtitle = "Datos: Enquisa de Poboación Activa, INE") +
  theme(legend.position = "none") +
  xlab("")


labels <- data.frame(x = c(1988, 2004, 2004, 2004),
                     y = c(  80,  800,  225, 400),
                     label = c("Agricultura, silvicultura e pesca", 
                               "Servizos",
                               "Industria",
                               "Construción"
                               ))

p3 <- ggplot(datoslong) +
  # color = "white" indicates the color of the lines between the areas
  geom_area(aes(x = Ano, y = Ocupados, fill = Sector2), color = "white", lwd = .1) +
  geom_text(data = labels, mapping = aes(x = x, y = y, label = label), col = "white") +
  scale_fill_okabe_ito(order = c(2,4,1,3)) +
  ggtitle(label = "Galicia. Persoas ocupadas por sectores (miles)",
          subtitle = "Datos: Enquisa de Poboación Activa, INE") +
  theme(legend.position = "None",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
        axis.ticks.length.y = unit(0, "mm"), 
        axis.ticks.length.x = unit(2, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black")) 


png("EPA_Galicia_1.png", width = 15, height = 10, units = "cm", res = 300)
print(p1)
dev.off()


png("EPA_Galicia_2.png", width = 15, height = 10, units = "cm", res = 300)
print(p2)
dev.off()


png("EPA_Galicia_3.png", width = 15, height = 10, units = "cm", res = 250)
print(p3)
dev.off()
