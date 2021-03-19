
library(dplyr)
library(readxl)

download.file("https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download",destfile="SINADEF.csv")
download.file("https://covid19.minsa.gob.pe/files/CASOS_05032021.xlsx",destfile="CASOS_05032021.xlsx",mode="wb")

datos.sinadef = read.csv("SINADEF.csv",sep=";",skip=2)
datos.minsa   = read_xlsx("CASOS_05032021.xlsx")

datos.minsa = datos.minsa %>% 
  add_row(Pais   = "PERÚ",
          Región = "LIMA",
          `PCR   (+)` = datos.minsa$`PCR   (+)`[1] + datos.minsa$`PCR   (+)`[26],
          `PRUEBA RÁPIDA (+)` = datos.minsa$`PRUEBA RÁPIDA (+)`[1] + datos.minsa$`PRUEBA RÁPIDA (+)`[26],
          #`PRUEBA ANTIGENO(+)` = datos.minsa$`PRUEBA ANTIGENO(+)`[1] + datos.minsa$`PRUEBA ANTIGENO(+)`[26],
          `TOTAL CASOS (+)` = datos.minsa$`TOTAL CASOS (+)`[1] + datos.minsa$`TOTAL CASOS (+)`[26],
          FALLECIDOS = datos.minsa$FALLECIDOS[1] + datos.minsa$FALLECIDOS[26])

datos.2020 = datos.sinadef %>% 
  mutate(FECHA = as.Date(FECHA)) %>% 
  filter(FECHA>=as.Date("2020-03-06") & !DEPARTAMENTO.DOMICILIO=="EXTRANJERO") %>% 
  select(FECHA,DEPARTAMENTO.DOMICILIO,CAUSA.A..CIE.X.,CAUSA.B..CIE.X.,
         CAUSA.C..CIE.X.,CAUSA.D..CIE.X.,CAUSA.E..CIE.X.,CAUSA.F..CIE.X.) %>% 
  mutate(CONF = ifelse((CAUSA.A..CIE.X.=="U071")|
                       (CAUSA.B..CIE.X.=="U071")|
                       (CAUSA.C..CIE.X.=="U071")|
                       (CAUSA.D..CIE.X.=="U071")|
                       (CAUSA.E..CIE.X.=="U071")|
                       (CAUSA.F..CIE.X.=="U071"),1,0),
         SOSP = ifelse((CAUSA.A..CIE.X.=="U072")|
                       (CAUSA.B..CIE.X.=="U072")|
                       (CAUSA.C..CIE.X.=="U072")|
                       (CAUSA.D..CIE.X.=="U072")|
                       (CAUSA.E..CIE.X.=="U072")|
                       (CAUSA.F..CIE.X.=="U072"),1,0))

library(janitor)
tabla = datos.2020 %>% 
  select(DEPARTAMENTO.DOMICILIO,CONF,SOSP) %>% 
  group_by(DEPARTAMENTO.DOMICILIO) %>% 
  summarise(Nconf = sum(CONF),
            Nsosp = sum(SOSP),
            Ntot  = Nconf+Nsosp) %>% 
  left_join(datos.minsa, by=c("DEPARTAMENTO.DOMICILIO" = "Región")) %>% 
  dplyr::select(DEPARTAMENTO.DOMICILIO,Nconf,Nsosp,Ntot,FALLECIDOS) %>% 
  dplyr::rename(NMINSA=FALLECIDOS) %>%
  mutate(Factor=Ntot/NMINSA) %>% 
  arrange(desc(Ntot)) %>% 
  adorn_totals

png("tabla07032021.png", height=630, width=600)
library(gridExtra)
tabla %>% 
  tableGrob() %>% 
  grid.arrange(top="Fallecidos confirmados y sospechosos (SINADEF) y confirmados por la Sala Situacional (MINSA) al 05/03/21",
               bottom="Factor = Total SINADEF / TOTAL MINSA")
dev.off()

write.csv(tabla,"tabla.csv")
