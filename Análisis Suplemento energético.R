# Abrir base de datos

library(readxl)
Marizol_datos_último <- read_excel("F:/Daniel/Rstudio/Marizol datos último.xlsx", 
                                    range = "A1:v260")
View(Marizol_datos_último)


# Verificar valores perdidos
is.na(Marizol_datos_último) #valores perdidos en la base de datos
sum(is.na(Marizol_datos_último)) #suma valores perdidos en la base de datos
sum(complete.cases(Marizol_datos_último)) #casos completos
mean(Marizol_datos_último$`Peso al destete`)
mean(Marizol_datos_último$`Peso al destete`, na.rm=T) #Promedio sin valores perdidos

#Eliminar valores faltantes
Marizol_datos_último <- na.omit(Marizol_datos_último)
View(Marizol_datos_último)
sum(is.na(Marizol_datos_último))

#Imputar valores faltantes (no se realizÃ³)
Marizol_datos_último$`Consumo total de suplemento` [ is.na (Marizol_datos_último$`Consumo total de suplemento`)] <- mean (Marizol_datos_último$`Consumo total de suplemento`, na.rm = TRUE )
Marizol_datos_último$`Consumo total de suplemento`<- round(Marizol_datos_último$`Consumo total de suplemento`,1)

Marizol_datos_último$`Semana 1` [ is.na (Marizol_datos_último$`Semana 1`)] <- mean (Marizol_datos_último$`Semana 1`, na.rm = TRUE )
Marizol_datos_último$`Semana 1`<- round(Marizol_datos_último$`Semana 1`,1)

Marizol_datos_último$`Semana 2` [ is.na (Marizol_datos_último$`Semana 2`)] <- mean (Marizol_datos_último$`Semana 2`, na.rm = TRUE )
Marizol_datos_último$`Semana 2`<- round(Marizol_datos_último$`Semana 2`,1)

Marizol_datos_último$`Semana 3` [ is.na (Marizol_datos_último$`Semana 3`)] <- mean (Marizol_datos_último$`Semana 3`, na.rm = TRUE )
Marizol_datos_último$`Semana 3`<- round(Marizol_datos_último$`Semana 3`,1)

Marizol_datos_último$`Peso al destete` [ is.na (Marizol_datos_último$`Peso al destete`)] <- mean (Marizol_datos_último$`Peso al destete`, na.rm = TRUE )
Marizol_datos_último$`Peso al destete`<- round(Marizol_datos_último$`Peso al destete`,1)

Marizol_datos_último$`Ganancia de peso` [ is.na (Marizol_datos_último$`Ganancia de peso`)] <- mean (Marizol_datos_último$`Ganancia de peso`, na.rm = TRUE )
Marizol_datos_último$`Ganancia de peso`<- round(Marizol_datos_último$`Ganancia de peso`,1)

Marizol_datos_último$`Ganancia de peso` [ is.na (Marizol_datos_último$`Ganancia de peso`)] <- mean (Marizol_datos_último$`Ganancia de peso`, na.rm = TRUE )
Marizol_datos_último$`Ganancia de peso`<- round(Marizol_datos_último$`Ganancia de peso`,1)

Marizol_datos_último$`Ganancia media diaria` [ is.na (Marizol_datos_último$`Ganancia media diaria`)] <- mean (Marizol_datos_último$`Ganancia media diaria`, na.rm = TRUE )
Marizol_datos_último$`Ganancia media diaria`<- round(Marizol_datos_último$`Ganancia media diaria`,2)

#Pruebas de normalidad
install.packages("nortest")
library(nortest)

lillie.test(Marizol_datos_último$`Peso al nacimiento`)
lillie.test(Marizol_datos_último$`Peso al destete`)
lillie.test(Marizol_datos_último$`Ganancia de peso`)
lillie.test(Marizol_datos_último$`Ganancia media diaria`)

#Pruebas de homocedasticidad
fligner.test(Marizol_datos_último$`Peso al nacimiento`~
               Marizol_datos_último$`Tratamiento (ml)`)#Test de Fligner-Killeen
bartlett.test(Marizol_datos_último$`Peso al nacimiento`~
                Marizol_datos_último$`Tratamiento (ml)`)
bartlett.test(Marizol_datos_último$`Peso al destete`~
                Marizol_datos_último$`Tratamiento (ml)`)#Test de Bartlett
bartlett.test(Marizol_datos_último$`Ganancia de peso`~
                Marizol_datos_último$`Tratamiento (ml)`)
bartlett.test(Marizol_datos_último$`Ganancia media diaria`~
                Marizol_datos_último$`Tratamiento (ml)`)

#Valores atipicos
Marizol_datos_último$Sexo <- as.factor(Marizol_datos_último$Sexo)

Marizol_datos_último$`Tratamiento (ml)`<- as.factor(Marizol_datos_último$`Tratamiento (ml)`)
Marizol_datos_último$`Tratamiento (ml)`<- factor(Marizol_datos_último$`Tratamiento (ml)`,
                                                  levels = levels(Marizol_datos_último$`Tratamiento (ml)`),
                                                  labels = c("0 ml","1 ml","1,5 ml", "2 ml"),
                                                  ordered = T)

boxplot(Marizol_datos_último$`Peso al nacimiento`, col = 2)
t.test(Marizol_datos_último$`Peso al nacimiento`)


boxplot(Marizol_datos_último$`Semana 1`)
boxplot(Marizol_datos_último$`Semana 2`)
boxplot(Marizol_datos_último$`Semana 3`)
boxplot(Marizol_datos_último$`Peso al destete`, col=2)
t.test(Marizol_datos_último$`Peso al destete`)

boxplot(Marizol_datos_último$`Ganancia de peso`, col = 2)
t.test(Marizol_datos_último$`Ganancia de peso`)

boxplot(Marizol_datos_último$`Ganancia media diaria`, col = 2)
t.test(Marizol_datos_último$`Ganancia media diaria`)

boxplot(Marizol_datos_último$`Peso al nacimiento`~Marizol_datos_último$`Tratamiento (ml)`,
        xlab="Tratamientos", ylab="Peso al nacimiento", 
        col=c(2,3,4,5), horizontal = F, las=1)
boxplot(Marizol_datos_último$`Peso al nacimiento`~Marizol_datos_último$Sexo,
        xlab="Sexo", ylab="Peso al nacimiento", 
        col=c(2,3,4,5), horizontal = F, las=1)

boxplot(Marizol_datos_último$`Peso al destete`~Marizol_datos_último$`Tratamiento (ml)`,
        xlab="Tratamientos", ylab="Peso al destete", 
        col=c(2,3,4,5), horizontal = F, las=1)
boxplot(Marizol_datos_último$`Peso al destete`~Marizol_datos_último$Sexo,
        xlab="Sexo", ylab="Peso al destete", 
        col=c(2,3,4,5), horizontal = F, las=1)

boxplot(Marizol_datos_último$`Ganancia de peso`~Marizol_datos_último$`Tratamiento (ml)`,
        xlab="Tratamientos", ylab="Ganancia de peso", 
        col=c(2,3,4,5), horizontal = F, las=1)
boxplot(Marizol_datos_último$`Ganancia de peso`~Marizol_datos_último$Sexo,
        xlab="Sexo", ylab="Ganancia de peso", 
        col=c(2,3,4,5), horizontal = F, las=1)

boxplot(Marizol_datos_último$`Ganancia media diaria`~Marizol_datos_último$`Tratamiento (ml)`,
        xlab="Tratamientos", ylab="Ganancia media diaria", 
        col=c(2,3,4,5), horizontal = F, las=1)
boxplot(Marizol_datos_último$`Ganancia media diaria`~Marizol_datos_último$Sexo,
        xlab="Sexo", ylab="Ganancia media diaria", 
        col=c(2,3,4,5), horizontal = F, las=1)

# Imputar valores atÃ­picos
install.packages("lattice")
install.packages("survival")
install.packages("Formula")
install.packages("ggplot2")
install.packages("Hmisc")

library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)

Marizol_datos_último$`Peso al destete`<- impute(Marizol_datos_último$`Peso al destete`, mean)
Marizol_datos_último$`Ganancia de peso`<- impute(Marizol_datos_último$`Ganancia de peso`, mean)
Marizol_datos_último$`Ganancia media diaria`<- impute(Marizol_datos_último$`Ganancia media diaria`, mean)


# Anova de dos vÃ­as, duncan e intervalos de confianza
install.packages("readr")
install.packages("ggplot2")
install.packages("multcompView")
install.packages("dplyr")

library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)


modelo <- aov (Marizol_datos_último$`Peso al nacimiento`~Marizol_datos_último$`Tratamiento (ml)` * Marizol_datos_último$Sexo)
summary(anova)

t.test(subset(Marizol_datos_último$`Peso al nacimiento`, Marizol_datos_último$`Tratamiento (ml)`==0))
t.test(subset(Marizol_datos_último$`Peso al nacimiento`, Marizol_datos_último$`Tratamiento (ml)`==1))
t.test(subset(Marizol_datos_último$`Peso al nacimiento`, Marizol_datos_último$`Tratamiento (ml)`==1.5))
t.test(subset(Marizol_datos_último$`Peso al nacimiento`, Marizol_datos_último$`Tratamiento (ml)`==2))

t.test(subset(Marizol_datos_último$`Peso al nacimiento`, Marizol_datos_último$Sexo=="Hembra"))
t.test(subset(Marizol_datos_último$`Peso al nacimiento`, Marizol_datos_último$Sexo=="Macho"))

modelo <- aov (Marizol_datos_último$`Peso al destete`~Marizol_datos_último$`Tratamiento (ml)` * Marizol_datos_último$Sexo, data = Marizol_datos_último)
summary(modelo)

t.test(subset(Marizol_datos_último$`Peso al destete`, Marizol_datos_último$`Tratamiento (ml)`==0))
t.test(subset(Marizol_datos_último$`Peso al destete`, Marizol_datos_último$`Tratamiento (ml)`==1))
t.test(subset(Marizol_datos_último$`Peso al destete`, Marizol_datos_último$`Tratamiento (ml)`==1.5))
t.test(subset(Marizol_datos_último$`Peso al destete`, Marizol_datos_último$`Tratamiento (ml)`==2))

t.test(subset(Marizol_datos_último$`Peso al destete`, Marizol_datos_último$Sexo=="Hembra"))
t.test(subset(Marizol_datos_último$`Peso al destete`, Marizol_datos_último$Sexo=="Macho"))

modelo <- aov (Marizol_datos_último$`Ganancia de peso`~Marizol_datos_último$`Tratamiento (ml)` * Marizol_datos_último$Sexo, data = Marizol_datos_último)
summary(modelo)

t.test(subset(Marizol_datos_último$`Ganancia de peso`, Marizol_datos_último$`Tratamiento (ml)`==0))
t.test(subset(Marizol_datos_último$`Ganancia de peso`, Marizol_datos_último$`Tratamiento (ml)`==1))
t.test(subset(Marizol_datos_último$`Ganancia de peso`, Marizol_datos_último$`Tratamiento (ml)`==1.5))
t.test(subset(Marizol_datos_último$`Ganancia de peso`, Marizol_datos_último$`Tratamiento (ml)`==2))

t.test(subset(Marizol_datos_último$`Ganancia de peso`, Marizol_datos_último$Sexo=="Hembra"))
t.test(subset(Marizol_datos_último$`Ganancia de peso`, Marizol_datos_último$Sexo=="Macho"))

modelo <- aov (Marizol_datos_último$`Ganancia media diaria`~Marizol_datos_último$`Tratamiento (ml)`* Marizol_datos_último$Sexo, data = Marizol_datos_último)
summary(modelo)

t.test(subset(Marizol_datos_último$`Ganancia media diaria`, Marizol_datos_último$`Tratamiento (ml)`==0))
t.test(subset(Marizol_datos_último$`Ganancia media diaria`, Marizol_datos_último$`Tratamiento (ml)`==1))
t.test(subset(Marizol_datos_último$`Ganancia media diaria`, Marizol_datos_último$`Tratamiento (ml)`==1.5))
t.test(subset(Marizol_datos_último$`Ganancia media diaria`, Marizol_datos_último$`Tratamiento (ml)`==2))

t.test(subset(Marizol_datos_último$`Ganancia media diaria`, Marizol_datos_último$Sexo=="Hembra"))
t.test(subset(Marizol_datos_último$`Ganancia media diaria`, Marizol_datos_último$Sexo=="Macho"))

# Mortalidad
library(readxl)
Marizol_datos_último <- read_excel("F:/Daniel/Rstudio/Marizol datos último.xlsx", 
                                    range = "A1:v260")
View(Marizol_datos_último)

Marizol_datos_último$`Tratamiento (ml)`<- as.factor(Marizol_datos_último$`Tratamiento (ml)`)
Marizol_datos_último$`Tratamiento (ml)`<- factor(Marizol_datos_último$`Tratamiento (ml)`,
                                                  levels = levels(Marizol_datos_último$`Tratamiento (ml)`),
                                                  labels = c("0 ml","1 ml","1,5 ml", "2 ml"),
                                                  ordered = T)

mari <- as.data.frame(table(Marizol_datos_último$Mortalidad, Marizol_datos_último$`Tratamiento (ml)`,
                            Marizol_datos_último$`Mortalidad (semana)`))
View(mari)

mari <- mari[!(mari$Var1 == "No"), ] # eliminar filas
View(mari)

mari <- mari[, -1] #eliminar columna
View(mari)

install.packages("dplyr")
library(dplyr)

T0 <- filter(mari, Var2=="0 ml")
T1 <- filter(mari, Var2=="1 ml")
T2 <- filter(mari, Var2=="1,5 ml")
T3 <- filter(mari, Var2=="2 ml")

datos <- data.frame("0 ml"=T0, "1 ml"=T1, "1,5 ml"=T2, "2 ml"=T3)   
View(datos)

datos <- datos[-1, ]

install.packages("MASS")
library(MASS)

Tratamiento_0 <- c(4, 4, 5, 5)
Tratamiento_1 <- c(4, 4, 4, 4)
Tratamiento_2 <- c(1, 1, 2, 2)
Tratamiento_3 <- c(15, 16, 16, 16)
semana <- c(1,2,3,4)

datos <- data.frame(Tratamiento_0, Tratamiento_1, Tratamiento_2, Tratamiento_3)

matplot(semana, datos, type = "l", 
        xlab = "Semana", ylab = "Lechones muertos",
        col = c(1:4),
        lty = 2, lwd = 2, las = 1, xlim = c(1,4), xaxt = "n", 
        lend = par("lend"), add = FALSE, bg = NA,
        cex = NULL)

matpoints(datos, type = "p", lty = 1, lwd = 1, pch = 16,
          col = c(1:4))

axis(1, at = c(0,1, 2, 3, 4))

legend(x = "top",                            
       legend = c("0 ml", "1 ml", "1,5 ml", "2 ml"),     
       lty = 1,                                 
       col = c(1:4),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n")      

# Morbilidad
mari <- as.data.frame(table(Marizol_datos_último$Morbilidad, Marizol_datos_último$`Tratamiento (ml)`,
                            Marizol_datos_último$`Morbilidad (semana)`))
View(mari)

mari <- mari[!(mari$Var1 == "No"), ] # eliminar filas
View(mari)

mari <- mari[, -1] #eliminar columna
View(mari)

library(dplyr)

T0 <- filter(mari, Var2=="0 ml")
T1 <- filter(mari, Var2=="1 ml")
T2 <- filter(mari, Var2=="1,5 ml")
T3 <- filter(mari, Var2=="2 ml")

datos <- data.frame("0 ml"=T0, "1 ml"=T1, "1,5 ml"=T2, "2 ml"=T3)   
View(datos)

datos <- datos[-1, ]

library(MASS)

Tratamiento_0 <- c(9, 9, 13, 13)
Tratamiento_1 <- c(5, 5, 5, 5)
Tratamiento_2 <- c(1, 1, 2, 2)
Tratamiento_3 <- c(1, 1, 1, 1)
semana <- c(1,2,3,4)

datos <- data.frame(Tratamiento_0, Tratamiento_1, Tratamiento_2, Tratamiento_3)

matplot(semana, datos, type = "l", 
        xlab = "Semana", ylab = "Lechones enfermos",
        col = c(1:4),
        lty = 2, lwd = 2, las = 1, xlim = c(1,4), xaxt = "n", 
        lend = par("lend"), add = FALSE, bg = NA,
        cex = NULL)

matpoints(datos, type = "p", lty = 1, lwd = 1, pch = 16,
          col = c(1:4))

axis(1, at = c(0,1, 2, 3, 4))

legend(x = "top",                            
       legend = c("0 ml", "1 ml", "1,5 ml", "2 ml"),     
       lty = 1,                                 
       col = c(1:4),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n")      

Marizol_datos_último$`Mortalidad (causa)` <- factor(Marizol_datos_último$`Mortalidad (causa)`)
Marizol_datos_último$`Tratamiento (ml)` <- factor(Marizol_datos_último$`Tratamiento (ml)`)



install.packages("openxlsx")
library(openxlsx)

data <- as.data.frame(table(Marizol_datos_último$`Mortalidad (causa)`,
                            Marizol_datos_último$`Tratamiento (ml)`))
View(data)
write.xlsx(data, "mari.xlsx")

data <- as.data.frame(table(Marizol_datos_último$`Morbilidad (causa)`,
                            Marizol_datos_último$`Tratamiento (ml)`))
View(data)
write.xlsx(data, "marisol.xlsx")

install.packages("plotrix")
library(plotrix)


barplot(table(Marizol_datos_último$Mortalidad, 
              Marizol_datos_último$`Tratamiento (ml)`),
        col = c(4,2), xlab = "Tratamientos", ylab = "Mortalidad")

legend(x = "top",                            
       legend = c("Si", "No"),     
       lty = 1,                                 
       col = c(4,2),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n") 


barplot(table(Marizol_datos_último$Morbilidad, 
              Marizol_datos_último$`Tratamiento (ml)`),
        col = c(4,2), xlab = "Tratamientos", ylab = "Morbilidad")

legend(x = "top",                            
       legend = c("Si", "No"),     
       lty = 1,                                 
       col = c(4,2),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n") 
