# Analisis-Sof-a-Y-nez
getwd()

if (file.exists("./datos/tsm_demo.csv")) {
  tsm <- read.csv("./datos/tsm_demo.csv", stringsAsFactors = FALSE)
} else {
  tsm <- read.csv("~/datos/tsm_demo.csv", stringsAsFactors = FALSE)
}

tsm$fecha <- as.Date(tsm$fecha)
head(tsm)


if (!requireNamespace("smooth", quietly = TRUE)) install.packages("smooth")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
library(smooth); library(lubridate)


sm <- cma(tsm$`loc.1`, order = 30)
tsm$sm <- sm$fitted

plot(tsm$fecha, tsm$`loc.1`, type="b", pch=16, col=rgb(1,0,0,0.2),
     xlab="Fecha", ylab="Temperatura potencial (°C)")
lines(tsm$fecha, tsm$sm, lwd=2)


tsm$mes  <- month(tsm$fecha)
tsm$anio <- year(tsm$fecha)

tsm_mens <- aggregate(tsm$`loc.1`, by = list(mes = tsm$mes, anio = tsm$anio), mean)

mes_pos <- as.Date(paste(tsm_mens$anio, tsm_mens$mes, 15, sep = "-"))
plot(mes_pos, tsm_mens$x, xlab = "Mes", ylab = "Temperatura (°C)", type = "b")
# (si ya tienes tsm con fecha y loc.1, puedes correr directo)
library(smooth); library(lubridate)

sm <- cma(tsm$`loc.1`, order = 30)
tsm$sm <- sm$fitted

tsm$doy <- yday(tsm$fecha)
clima_doy <- aggregate(tsm$`loc.1`, by = list(doy = tsm$doy), FUN = mean)
names(clima_doy)[2] <- "clim"

tsm <- merge(tsm, clima_doy, by = "doy", all.x = TRUE)
tsm$anom_dia <- tsm$`loc.1` - tsm$clim

# Vista rápida
plot(tsm$fecha, tsm$anom_dia, type="l", xlab="Fecha", ylab="Anomalía diaria (°C)")
abline(h=0, lty=2)
# 1) Generar rejilla y guardarla junto al tsm (si ya la tienes, salta a paso 2)
dir.create("./datos", showWarnings = FALSE, recursive = TRUE)

lons <- seq(-112.0, -111.0, length.out = 6)
lats <- seq(22.0, 23.0,   length.out = 5)
depths <- c(0.5, 1.5)
m0 <- as.Date(c("2019-01-01","2019-08-01","2020-01-01","2020-08-01"))
times <- as.Date(unlist(lapply(m0, function(x) x + 0:9)))

rows <- list()
for (t in times) {
  doy <- as.numeric(format(t, "%j"))
  seasonal <- 4.5 * sin(2 * pi * (doy / 365.25))
  for (d in depths) {
    depth_cool <- -0.4 * d
    for (lo in lons) for (la in lats) {
      val <- 24.0 + seasonal + 0.1*(lo - mean(lons)) + 0.2*(la - mean(lats)) + depth_cool
      rows[[length(rows)+1]] <- data.frame(
        lon = lo, lat = la, fecha = t, depth_m = d, thetao_degC = round(val, 4)
      )
    }
  }
}
grid <- do.call(rbind, rows)
write.csv(grid, "./datos/thetao_demo_grid.csv", row.names = FALSE)


grid <- read.csv("./datos/thetao_demo_grid.csv")
grid$fecha <- as.Date(grid$fecha)
grid$mes   <- format(grid$fecha, "%m")
grid$anio  <- format(grid$fecha, "%Y")


mens <- aggregate(thetao_degC ~ lon + lat + depth_m + anio + mes, data=grid, mean)

clim_mens <- aggregate(thetao_degC ~ lon + lat + depth_m + mes, data=grid, mean)
names(clim_mens)[names(clim_mens)=="thetao_degC"] <- "thetao_clim"


anom <- merge(mens, clim_mens, by=c("lon","lat","depth_m","mes"))
anom$anom <- anom$thetao_degC - anom$thetao_clim


ene <- subset(anom, mes=="01" & anio=="2019")
plot(ene$lon, ene$lat, pch=16, cex=1 + 0.8*scale(ene$anom),
     xlab="Lon", ylab="Lat", main="Anomalía mensual (enero 2019)")

