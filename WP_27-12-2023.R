##Trabajo Final-Tecnicas avanzadas de fecundidad##
##Cedeplar-segundo ano: 2023
##Ali M. Arrieta-Arrieta

rm(list=ls())
gc()
setwd("C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/eder2017_bases_sav")

#install.packages("ggbreak")
#install.packages("tidyverse")
#install.packages("fdth")
library(tidyverse)
library(car) 
library(fdth)
library(data.table)
library(ggplot2)
library(ggbreak) 
library(patchwork)
library(questionr)
#install.packages("pollster")
library(pollster)
library(haven)

persona <- read_sav("persona.sav")
antecedentes <- read_sav("antecedentes.sav")
historiavida <- read_sav("historiavida.sav")


persona <- filter(persona, sexo %in% c("2")) #Solo mujeres#
table(persona$edad)
table(persona$Muestra)


Aux1 <- merge(persona,antecedentes, by = c("folioviv", "foliohog", "id_pobla"))
Aux2 <- merge(Aux1,historiavida, by = c("folioviv", "foliohog", "id_pobla"), all.x=TRUE)

table(Aux2$sexo.x)
rm(historiavida) #elimino esta base porque pesa mucho#
table(Aux2$hij_vivos)
table(Aux2$anio_retro)
table(Aux2$hij_gen_1)
table(Aux2$edad)

Aux2 <- unite(Aux2, key, c("folioviv", "foliohog", "id_pobla"), sep = "", remove = T)

Aux2 <-  Aux2  %>% 
  mutate(id=1,
    Group_age=case_when(
    edad>=15 & edad<=19~1,
    edad>=20 & edad<=24~2,
    edad>=25 & edad<=29~3,
    edad>=30 & edad<=34~4,
    edad>=35 & edad<=39~5,
    edad>=40 & edad<=44~6,
    edad>=45 & edad<=49~7,
    edad>=50 & edad<=54~8),
    Group_age=factor(Group_age,levels = c("1","2","3","4", "5", "6", "7", "8"), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")))


write.csv(Aux2, file="C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/Base_Master.csv") 
#Aux2: 489,633 años persona mujer

Aux3 <- Aux2 %>% group_by(key, factor_per, Group_age) %>% 
  summarise(N_mujeres=sum(id),
            .groups = 'drop') %>%
  as.data.frame()


Aux3 <- distinct(Aux2, key, factor_per, Group_age, ) ##13.083 mujeres##
Aux3 <-  Aux3  %>% 
         mutate(id=1)
write.csv(Aux3, file="C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/Base_Master_collap.csv") 


A <- crosstab(df = Aux3, x = Group_age, y = id, weight = factor_per, format = "long")
write.csv(A, file="C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/poblaciontotal.csv") 



##Variable independiente##
##“¿ha vivido al menos un año en forma continua en otras localidades, poblados o ciudades?”; “¿Podría decirme todos los (estados o países, municipios o localidades) donde haya vivido después de (localidad de nacimiento) por lo menos un año en forma continua, y la edad que usted tenía o el año cuando llegó a estos lugares?”

Aux2 <- read.csv("C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/Base_Master.csv", header=TRUE)

table(Aux2$otra_resid)

#¿Hace 5 años en 2012, en qué estado de la  república mexicana o país vivía (NOMBRE)?

table(Aux2$residencia)
table(Aux2$geo_eder)
table(Aux2$edad_retro)

##Filtros: migro por lo menos un ano durante el periodo reproductivo#

Aux2 <-  Aux2  %>% 
         mutate(Age=edad_act,
           Edad_retr=edad_retro,
           Localidad_his=substr(Aux2$geo_eder,4,5),
           Localidad_his=force(as.numeric(Localidad_his)),
           Localidad.nac=case_when(Edad_retr==0~Localidad_his),
           Localidad.nac=as.numeric(Localidad.nac),
           Periodo.repro=case_when(Edad_retr>=15 & Edad_retr<=49~1, TRUE~0))

Aux2 <-  arrange(Aux2, key, edad_retro)

missing_rows <- !complete.cases(Aux2$Localidad.nac)
Aux2$Localidad.nac[missing_rows] <- 0

Aux2 <- Aux2 %>%
  group_by(key) %>%
  mutate(Localidad.nac1=max(Localidad.nac))

Aux2 <-  Aux2  %>% 
  mutate(year.migro=case_when(Localidad_his== Localidad.nac1~0,
                              Localidad_his!= Localidad.nac1~1),
         sample= case_when(Periodo.repro==1 & year.migro==1~1, TRUE~0))


table(Aux2$Localidad.nac)
table(Aux2$year.migro)
A <-data.frame(ftable(Aux2[,c("key","sample")]) )
B <- data.frame(ftable(Aux2[,c("edad","sample")]) )

B$sample <-factor(B$sample)

grap1 <- ggplot() + 
  geom_bar(data=B,aes(x=edad, y=(Freq),fill=sample), stat='identity', position='dodge') +
  labs(title="",x="Edad (años)", y = "Años-persona (Mujer)", 
       caption = "Fuente: EDER-2017") +
  scale_fill_manual(values=c("#56b86f", "#132f49")) +
  theme_minimal()+
  scale_x_discrete( breaks = seq(20, 60, by = 5))
ggsave( filename  = 'C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Grap1.jpg', width = 6, height = 4, dpi = 600)

##Promedio de anos de migracion por edad simple


n <-data.frame(ftable(Aux2[,c("Localidad_his","sample")]) )


n.1 <- filter(Aux2, sample==1)
n.1 <- n.1 %>%
  group_by( Group_age, key) %>%
  summarise(sum=sum(sample))
n.1$id <- 1

n2 <-data.frame(ftable(n.1[,c("Group_age","id")]) )




C <- filter(Aux2, sample==1)
C <- C %>%
  group_by( edad, key) %>%
  summarise(sum=sum(sample))

C1 <- C %>%
  group_by(edad) %>%
  summarise(mean=mean(sum))

grap2 <- ggplot(C1, aes(x = edad, y = mean)) + 
  geom_line(size=1, color="darkred") + 
  labs(title="",x="Edad (años)", y = "Promedio (años de migración)", 
       caption = "Fuente: EDER-2017") +
       theme_minimal()  +
  scale_x_continuous( breaks = seq(20, 55, by = 5))
ggsave( filename  = 'C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Grap2.jpg', width = 6, height = 4, dpi = 600)


#Anos persona meujer

E <- Aux2 %>% group_by(Group_age, .drop = FALSE) %>% count(id)

E1 <- Aux2 %>% group_by(Group_age,key,factor_per, .drop = FALSE) %>% count(id)

write.csv(E1, file="C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/APM.csv") 



##Promedio de anos de migracion por grupos de edad

D <- filter(Aux2, sample==1)
D <- D %>%
  group_by( Group_age, key) %>%
  summarise(sum=sum(sample))

D1 <- D %>%
  group_by(Group_age) %>%
  summarise(mean=mean(sum))

grap3 <- ggplot() + 
  geom_bar(data=D1,aes(x=Group_age, y=mean), stat='identity', position='dodge', fill = "#132f49") +
  labs(title="",x="Grupo de edad", y = "Promedio de anos-migración", 
       caption = "Fuente: EDER-2017") +
scale_fill_manual(values=c("#132f49"))  +
theme_minimal()  
ggsave( filename  = 'C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Grap3.jpg', width = 6, height = 4, dpi = 600)



library(patchwork)

Grap4 <- (grap2 + grap3)
ggsave( filename  = 'C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Grap4.jpg', width = 6, height = 4, dpi = 600)



#install.packages("questionr")
library(questionr)
#install.packages("pollster")
library(pollster)

#crosstab(df = D, x = Group_age, y = id, weight = factor_per, format = "long")





#Descriptivas#

table(Aux2$hijos_m)
table(Aux2$hij_vivos)
table(Aux2$hij_gen_1)
table(Aux2$hij_vid_1)
table(Aux2$factor_per)


#Agrupacion#

Hijos <- select(Aux2, key,factor_per, Group_age, edad, anio_retro, hij_vivos, hij_vid_1, hij_vid_2, hij_vid_3,
                                                         hij_vid_4, hij_vid_5, hij_vid_6,
                                                         hij_vid_7, hij_vid_8, hij_vid_9,
                                                         hij_vid_10, hij_vid_11, hij_vid_12,
                                                         hij_vid_13, hij_vid_14, hij_vid_15,)





Birth_chil <- Hijos  %>% pivot_longer(cols=c('hij_vid_1', 'hij_vid_2', "hij_vid_3",
                                             "hij_vid_4", "hij_vid_5", "hij_vid_6",
                                             "hij_vid_7", "hij_vid_8", "hij_vid_9",
                                             "hij_vid_10", "hij_vid_11", "hij_vid_12",
                                             "hij_vid_13", "hij_vid_14", "hij_vid_15",),
                                             names_to=c('hij_vid'),
                                             values_to='Birthyear')

table(Birth_chil$Birthyear)


Birth_chil <- filter(Birth_chil,   Birthyear %in% c(6))
table(Birth_chil$hij_vid)

Birth_chil$Position =substr(Birth_chil$hij_vid,9,10)
table(Birth_chil$Position)

Birth_chil <- unite(Birth_chil, key_child, c("key", "Position"), sep = "", remove = F)


Birth_chil <-  Birth_chil  %>% 
  mutate(id=1,
         Group_age1=case_when(
           edad>=15 & edad<=19~1,
           edad>=20 & edad<=24~2,
           edad>=25 & edad<=29~3,
           edad>=30 & edad<=34~4,
           edad>=35 & edad<=39~5,
           edad>=40 & edad<=44~6,
           edad>=45 & edad<=49~7,
           edad>=50 & edad<=54~8),
         Group_age1=factor(Group_age1,levels = c("1","2","3","4", "5", "6", "7", "8"), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")))




A <- crosstab(df = Birth_chil, x = Group_age1, y = hij_vivos, weight = factor_per, format = "long")
A1 <-  arrange(A, Group_age1, hij_vivos)

A1 <- select(A, -n)

A1$hij_vivos <- as.numeric(A1$hij_vivos)

A1 <- A1 %>% arrange(desc(Group_age1), desc(hij_vivos))
A1$hij_vivos1 <- factor(A1$hij_vivos)


GrapA <- ggplot() + 
  geom_bar(data=A1,aes(x=Group_age1, y=pct,fill=hij_vivos1), stat='identity', position='dodge') +
  scale_fill_brewer(palette="BrBG")+
  theme_classic()+
  labs(title="Parturicao/orden",x="", y = "Proporcion", 
       caption = "Fuente: EDER 2017, INEGI")
ggsave( filename  = 'C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/GrapA.jpg', width = 6, height = 4, dpi = 600)

##Sexo de los nacidos vivos


Sex_hijos <- select(Aux2, key,factor_per, edad, anio_retro, hij_vivos, hij_gen_1, hij_gen_2, hij_gen_3,
                                                            hij_gen_4, hij_gen_5, hij_gen_6,
                                                            hij_gen_7, hij_gen_8, hij_gen_9,
                                                            hij_gen_10, hij_gen_11, hij_gen_12,
                                                            hij_gen_13, hij_gen_14, hij_gen_15)


Sex_Birth <- Sex_hijos  %>% pivot_longer(cols=c("hij_gen_1", "hij_gen_2", "hij_gen_3",
                                                "hij_gen_4", "hij_gen_5", "hij_gen_6",
                                                "hij_gen_7", "hij_gen_8", "hij_gen_9",
                                                "hij_gen_10", "hij_gen_11", "hij_gen_12",
                                                "hij_gen_13", "hij_gen_14", "hij_gen_15",),
                                                 names_to=c("hij_gen"),
                                                 values_to='Birthsex')

Sex_Birth$Position =substr(Sex_Birth$hij_gen,9,10)
table(Sex_Birth$Birthsex)

table(Sex_Birth$Birthsex)
Sex_Birth <- filter(Sex_Birth,   Birthsex %in% c(1,2))
str(Sex_Birth)

Sex_Birth <- distinct(Sex_Birth, key,factor_per, edad , hij_vivos,  hij_gen,Birthsex, Position )
Sex_Birth <- unite(Sex_Birth, key_child, c("key", "Position"), sep = "", remove = F)


##Base total##

Birthandsex <- merge(Birth_chil,Sex_Birth, by = c("key_child"), all.x=TRUE)
table(Birthandsex$anio_retro)
table(Birthandsex$Birthsex)

#library(fdth)
library(data.table)


Birthandsex<- Birthandsex %>% 
  mutate(anio_retro=as.numeric(as.character(anio_retro)),
         Birthsex=factor(Birthsex, levels = c("1", "2"),
                          labels = c("Hombre","Mujer")))

str(Birthandsex)

C <-(ftable(Birthandsex[,c("anio_retro","Birthsex")]))
C1 <- data.frame(prop.table(C, margin = 2))

str(C1)

C1$anio_retro <- as.numeric(as.character(C1$anio_retro))

ggplot(C1, aes(x = anio_retro, y = (Freq*100), color = Birthsex)) + 
  geom_line(aes(), size=1) + 
  scale_color_manual(values = c("darkred", "steelblue"))+
  labs(title="",x="Año de nacimiento", y = "Proporcion (%)", 
       caption = "Fuente: EDER 2017, INEGI") +
  theme_minimal()+
  scale_x_continuous( breaks = seq(1977, 2017, by = 5))
ggsave( filename  =  'C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Grafica_1.jpg', width = 6, height = 4, dpi = 600)


##Amostra serian los nacimientos 15 años atras de la encuesta, de 2012 a 2017


BD <- filter(Birthandsex, anio_retro>=2012) #Solo mujeres de 15-64 years#

table(BD$anio_retro)

C <-(ftable(BD[,c("anio_retro","Birthsex")]))
C1 <- data.frame(prop.table(C, margin = 2))

str(C1)

C1$anio_retro <- as.numeric(as.character(C1$anio_retro))

ggplot(C1, aes(x = anio_retro, y = (Freq*100), color = Birthsex)) + 
  geom_line(aes(), size=1) + 
  scale_color_manual(values = c("darkred", "#5CB85C"))+
  labs(title="",x="Año de nacimiento", y = "Proporcion (%)", 
       caption = "Fuente: EDER 2017, INEGI") +
  theme_minimal()+
  scale_x_continuous( breaks = seq(2012, 2017, by = 1))


##Paso los años a columnas##


BD <- BD %>% 
      mutate(
        y2003=case_when(
        anio_retro==2003~1,
        TRUE ~0),
        sex2003=case_when(
          Birthsex=="Hombre" & anio_retro==2003~1,
          Birthsex=="Mujer" & anio_retro==2003~2,
          TRUE ~0),
        
        
        y2004=case_when(
          anio_retro==2004~1,
          TRUE ~0),
        sex2004=case_when(
          Birthsex=="Hombre" & anio_retro==2004~1,
          Birthsex=="Mujer" & anio_retro==2004~2,
          TRUE ~0),
        
        y2005=case_when(
          anio_retro==2005~1,
          TRUE ~0),
        sex2005=case_when(
          Birthsex=="Hombre" & anio_retro==2005~1,
          Birthsex=="Mujer" & anio_retro==2005~2,
          TRUE ~0),
        
        y2006=case_when(
          anio_retro==2006~1,
          TRUE ~0),
        sex2006=case_when(
          Birthsex=="Hombre" & anio_retro==2006~1,
          Birthsex=="Mujer" & anio_retro==2006~2,
          TRUE ~0),
        
        y2007=case_when(
          anio_retro==2007~1,
          TRUE ~0),
        sex2007=case_when(
          Birthsex=="Hombre" & anio_retro==2007~1,
          Birthsex=="Mujer" & anio_retro==2007~2,
          TRUE ~0),
        
        y2008=case_when(
          anio_retro==2008~1,
          TRUE ~0),
        sex2008=case_when(
          Birthsex=="Hombre" & anio_retro==2008~1,
          Birthsex=="Mujer" & anio_retro==2008~2,
          TRUE ~0),
        
        y2009=case_when(
          anio_retro==2009~1,
          TRUE ~0),
        sex2009=case_when(
          Birthsex=="Hombre" & anio_retro==2009~1,
          Birthsex=="Mujer" & anio_retro==2009~2,
          TRUE ~0),
        
        y2010=case_when(
          anio_retro==2010~1,
          TRUE ~0),
        sex2010=case_when(
          Birthsex=="Hombre" & anio_retro==2010~1,
          Birthsex=="Mujer" & anio_retro==2010~2,
          TRUE ~0),
        
        y2011=case_when(
          anio_retro==2011~1,
          TRUE ~0),
        sex2011=case_when(
          Birthsex=="Hombre" & anio_retro==2011~1,
          Birthsex=="Mujer" & anio_retro==2011~2,
          TRUE ~0),
        
        y2012=case_when(
          anio_retro==2012~1,
          TRUE ~0),
        sex2012=case_when(
          Birthsex=="Hombre" & anio_retro==2012~1,
          Birthsex=="Mujer" & anio_retro==2012~2,
          TRUE ~0),
        
        y2013=case_when(
          anio_retro==2013~1,
          TRUE ~0),
        sex2013=case_when(
          Birthsex=="Hombre" & anio_retro==2013~1,
          Birthsex=="Mujer" & anio_retro==2013~2,
          TRUE ~0),
        
        y2014=case_when(
          anio_retro==2014~1,
          TRUE ~0),
        sex2014=case_when(
          Birthsex=="Hombre" & anio_retro==2014~1,
          Birthsex=="Mujer" & anio_retro==2014~2,
          TRUE ~0),
        
        
        y2015=case_when(
          anio_retro==2015~1,
          TRUE ~0),
        sex2015=case_when(
          Birthsex=="Hombre" & anio_retro==2015~1,
          Birthsex=="Mujer" & anio_retro==2015~2,
          TRUE ~0),
        
        
        y2016=case_when(
          anio_retro==2016~1,
          TRUE ~0),
        sex2016=case_when(
          Birthsex=="Hombre" & anio_retro==2016~1,
          Birthsex=="Mujer" & anio_retro==2016~2,
          TRUE ~0),
        
        
        y2017=case_when(
          anio_retro==2017~1,
          TRUE ~0),
        sex2017=case_when(
          Birthsex=="Hombre" & anio_retro==2017~1,
          Birthsex=="Mujer" & anio_retro==2017~2,
          TRUE ~0)
        
        )
        
##Edad por quinquenios##

table(BD$edad.x)

BD <-  BD  %>% 
  mutate(Group_age=case_when(
    edad.x>=15 & edad.x<=19~1,
    edad.x>=20 & edad.x<=24~2,
    edad.x>=25 & edad.x<=29~3,
    edad.x>=30 & edad.x<=34~4,
    edad.x>=35 & edad.x<=39~5,
    edad.x>=40 & edad.x<=44~6,
    edad.x>=45 & edad.x<=49~7,
    edad.x>=50 & edad.x<=54~8),
    Group_age=factor(Group_age,levels = c("1","2","3","4", "5", "6", "7", "8"), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")))

table(BD$Group_age)

write.csv(BD, file="C:/Users/Alí/Documents/PDh_Cedeplar/Segundo_year/Segundo semestre/Fecundidad/Working_paper/Base_Master_collap.csv") 


BD2 <- BD %>% group_by(Group_age) %>% 
              summarise(sumy2003=sum(y2003),
              .groups = 'drop') %>%
              as.data.frame()
BD2





