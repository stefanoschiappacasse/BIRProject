#importar librerías
library(stringr)
library(lubridate)
library(plyr)
library(tidyverse)

#ruta archivosTag
setwd('C:/Users/stefanosch/OneDrive - Socrates 365 Ltda/BI/Pao/TAGS')

#archivoTags
tags <- read.csv(file = 'tagsJunio.csv',
                 sep = ';',
                 dec = ',') %>% 
  mutate(Fecha = dmy(Fecha))

#data tags por imputar
tagsSinCC <- tags %>% 
  filter(Cco == '0') %>% 
  arrange(Patente, Fecha, Hora)

#funcion que importa los movimientos de los meses en los que hay movimientos de tags
importarYProcesar <- function(fechaMin, fechaMax, patentes){
  meses <- c()
  for(i in month(min(tagsSinCC$Fecha)):month(max(tagsSinCC$Fecha))){

    meses <- c(meses, 
               as.character(month(dmy(paste0('01-', 
                                str_pad(as.character(i),
                                        pad = '0',
                                        side = 'left',
                                        width = 2),
                                '-2021')),
                     label = TRUE)))
  }
  print(meses)
  for(mes in meses){
    print(mes)
    rutaArchivo <- paste0('C:/Users/stefanosch/OneDrive - Socrates 365 Ltda/BI/Valorizaciones/', mes, 'Valorizado.csv')
    dataMes <- read.csv(file = rutaArchivo,
                        sep = ';',
                        dec = ',') %>% 
      mutate(Valor = as.numeric(Valor),
             FechaHR = ymd_hms(FechaHR),
             FechaSalidaHR = ymd_hms(FechaSalidaHR),
             FechaInicioActividad = ymd_hms(FechaInicioActividad),
             MesSalida = lubridate::month(FechaSalidaHR, label = TRUE),
             DiaSalida = lubridate::day(FechaSalidaHR),
             HoraSalida = lubridate::hour(FechaSalidaHR))
    
    if(match(mes,meses) == 1){
      dataTotal <- dataMes
    } 
    else{
      dataTotal <- rbind.fill(dataTotal, dataMes)
    }
  }
  return(dataTotal %>% 
           filter(Patente %in% patentes) %>% 
           group_by(Patente, 
                    Fecha = as.Date(FechaHR),
                    CentroCostoTransporte) %>% 
           tally(name = 'Cantidad') %>% 
           ungroup())
}

#datos relevantes para el archivo tag
data <- importarYProcesar(min(tagsSinCC$Fecha),
                          max(tagsSinCC$Fecha),
                          unique(tagsSinCC$Patente))

# dataDet2 <- rbind.fill(marzo %>% filter(Patente %in% tagsSinCC$Patente),
#                    abril %>% filter(Patente %in% tagsSinCC$Patente),
#                    mayo %>% filter(Patente %in% tagsSinCC$Patente),
#                    junio %>% filter(Patente %in% tagsSinCC$Patente)) %>% 
#   mutate(Fecha = ifelse(TipoOperacion == 'ENTREGA',
#                           as.character(FechaSalidaHR),
#                           as.character(FechaHR))) %>%
#   separate(Fecha, into = c("Fecha", "HoraHR"), sep = " ") %>%
#   mutate(Fecha = as.Date(Fecha)) %>% 
#   arrange(Patente, Fecha, HoraHR) %>% 
#   distinct(Patente, Fecha, HoraHR, CentroCostoTransporte, TipoOperacion)
# 
# p2 <- tagsSinCC %>% 
#   merge(.,
#         dataDet2,
#         by = c('Patente', 'Fecha'),
#         all.x = TRUE)

#tags relevantes agrupados por fecha y patente
tagsAgrupado <- tagsSinCC %>% 
  group_by(Patente, Fecha) %>% 
  dplyr::summarise(Monto = sum(Monto))



p <- tagsAgrupado %>% 
  merge(.,
        data,
        by = c('Patente', 'Fecha'),
        all.x = TRUE)

View(p %>% 
       filter(duplicated(Patente, Fecha) | duplicated(Patente, Fecha, fromLast = TRUE)) %>% 
       arrange(Patente, Fecha))

View(p[duplicated(p[,1:2]) | duplicated(p[,1:2], fromLast = TRUE),] %>% 
  arrange(Patente, Fecha))

pRep <- p[duplicated(p[,1:2]) | duplicated(p[,1:2], fromLast = TRUE),] %>% 
  arrange(Patente, Fecha)

pNoRep <- p[!duplicated(p[,1:2]) & !duplicated(p[,1:2], fromLast = TRUE),] %>% 
  arrange(Patente, Fecha)

pRep %>% 
  dplyr::count(Patente, Fecha) %>% 
  arrange(-n)

tagsSinCC2 <- tagsSinCC %>% 
  merge(.,
        pNoRep %>% select(Patente, Fecha, CentroCostoTransporte),
        by = c('Patente', 'Fecha'),
        all.x = TRUE)

tagsSinCC2 %>% 
  filter(is.na(CentroCostoTransporte))

write.table(tagsSinCC2,
            file = 'tagPorRevisar2.csv',
            sep = ';',
            dec = ',',
            row.names = FALSE)

tagsSinCC2 <- tagsSinCC2 %>% 
  merge(.,
        lugaresCC,
        by = 'Lugar',
        all.x = TRUE) %>% 
  dplyr::mutate(CentroCosto = ifelse((PORTICO == 'Nva. Angostura Free Flow') |
                                (AUTOPISTA %in% c('AUTOPISTA STGO - LAMPA', 
                                                  'RUTA DEL MAIPO', 
                                                  'VESPUCIO SUR', 
                                                  'COSTANERA NORTE')) |
                                  ((PORTICO %in% as.character(seq(1,20))) & (AUTOPISTA == 'VESPUCIO NORTE')),
                              'DIST REGIONAL',
                              ifelse(is.na(CentroCostoTransporte),
                                     CC2,
                                     CentroCostoTransporte))) %>% 
  arrange(Patente, Fecha, Hora)


str(tagsSinCC2)
sum(is.na(tagsSinCC2$CentroCosto))

tagsSinCC2 %>% 
  filter(is.na(CentroCosto)) %>% 
  dplyr::count(Lugar)


tagsSinCC2 %>% 
  filter(is.na(CentroCosto)) %>% 
  dplyr::count(Patente, Fecha, name = 'Cantidad2') %>% 
  merge(.,
        data,
        by = c('Patente', 'Fecha'),
        all.x = TRUE) %>% 
  filter(!is.na(CentroCostoTransporte)) %>% 
  mutate(Vueltas = round(Cantidad/8)) %>%
  group_by(Patente, Fecha) %>% 
  dplyr::mutate(Proporcion = Vueltas/sum(Vueltas)) %>% 
  select(-Cantidad2, - Cantidad, - Vueltas) -> camionesDuplicados

camionesDuplicados %>% 
  reshape2::melt(., id.vars = c("Patente", 'Fecha'), 
                 measure.vars = 1:4 )


camionesDuplicados %>% 
  group_by(CentroCostoTransporte) %>% 
  dplyr::summarise(Promedio = mean(Proporcion))

sample(c("DIST REGIONAL", "DIST RM", "PUERTO"), 
       1, 
       prob=c(0.393, 0.533, 0.519), replace=TRUE)
library(tidyr)

View(camionesDuplicados %>% 
  mutate(var = 1) %>% 
  spread(key = CentroCostoTransporte, 
         value = var, 
         fill = 0, sep = "_") %>% 
  left_join(camionesDuplicados) %>% 
  select(Patente, Fecha, everything()))

View(tagsSinCC2 %>% 
  filter(is.na(CentroCosto))%>% 
  dplyr::mutate(CentroCosto2 = sample(c("DIST REGIONAL", "DIST RM", "PUERTO"), 
                                      n(),
                                      prob=c(0.393, 0.533, 0.519),
                                      replace=TRUE)))
tagsSinCC2 %>% 
  filter(is.na(CentroCosto))%>% 
  dplyr::mutate(CentroCosto2 = sample(c("DIST REGIONAL", "PUERTO"), 
                                      n(),
                                      prob=c(0.376, 0.528),
                                      replace=TRUE)) -> tagsSinCC3


tagsSinCC4 <- tagsSinCC2 %>% 
  filter(!is.na(CentroCosto)) %>% 
  rbind.fill(.,
             tagsSinCC3 %>% mutate(CentroCosto = CentroCosto2) %>% select(-CentroCosto2))

sum(is.na(tagsSinCC4$CentroCosto))

tagsConCC <- tags %>% 
  filter(Cco != '0')

tags2 <- tagsConCC %>% 
  rbind.fill(.,
             tagsSinCC4 %>% mutate(Cco = CentroCosto) %>% select(-CC2, 
                                                                 -CentroCosto, 
                                                                 -CentroCostoTransporte))
sum(is.na(tags2$Cco))

write.table(x = tags2,
            file = 'tagsConCCMayo.csv',
            sep = ';',
            dec = ',',
            row.names = FALSE)

tagsSinCC2 %>% 
  filter(is.na(CentroCosto)) %>% 
  merge(.,
        camionesDuplicados,
        by = c('Patente', 'Fecha'),
        all.x = TRUE) %>% 
  View(.)

camionesDuplicados %>% 
  mutate(Asignación)




sum(is.na(tagsSinCC2$CentroCosto))
  
centrosCostos <- c('DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   NA,
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   NA,
                   'PUERTO',
                   'PUERTO',
                   'PUERTO',
                   'PUERTO',
                   NA,
                   'PUERTO',
                   'PUERTO',
                   'PUERTO',
                   'DIST REGIONAL',
                   'DIST REGIONAL')

lugaresCC <- data.frame(Lugar = unique(tagsSinCC2$Lugar),
                        CC2 = centrosCostos)


centrosCostos <- c('DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   NA,
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   'DIST REGIONAL',
                   NA,
                   'PUERTO',
                   'PUERTO',
                   'PUERTO',
                   'PUERTO',
                   NA,
                   'PUERTO',
                   'PUERTO',
                   'PUERTO',
                   'DIST REGIONAL',
                   'DIST REGIONAL')

write.table(x = lugaresCC,
            file = 'lugaresCC.csv',
            sep = ';',
            dec = ',',
            row.names = FALSE)
