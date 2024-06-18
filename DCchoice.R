library(readxl)
library(DCchoice)
library(tidyr)
library(detectseparation)

# Load the data
respuestasCuestionario <- read_excel("~/Desktop/respuestasCuestionario.xlsx")

# Prepare the DCchoice dataframe
DCchoice <- data.frame(subject = respuestasCuestionario$subject)

DCchoice$WTP <- ifelse(respuestasCuestionario$rentabilidad_asg == "Menor que la media del mercado", "Si", 
                       ifelse(respuestasCuestionario$rentabilidad_asg == "Mayor que la media del mercado", "No", NA))

DCchoice$BID1 <- as.numeric(12.3)

DCchoice$R1 <- ifelse(respuestasCuestionario$rentabilidad_asg == "Menor que la media del mercado" & respuestasCuestionario$first_dicotomic == 'Si', 1, 
                      ifelse(respuestasCuestionario$rentabilidad_asg == "Menor que la media del mercado" & respuestasCuestionario$first_dicotomic == 'No', 0, NA))

DCchoice$age <- respuestasCuestionario$edad

DCchoice$sex <- ifelse(respuestasCuestionario$genero == "Masculino", 1, 
                       ifelse(respuestasCuestionario$genero == "Femenino", 0, NA))

# Concepto ASG familiaridad
DCchoice$asg1 <- ifelse(respuestasCuestionario$concepto_asg == "Muy familiarizado", 1, 0)
DCchoice$asg2 <- ifelse(respuestasCuestionario$concepto_asg == "Algo familiarizado", 1, 0)
DCchoice$asg3 <- ifelse(respuestasCuestionario$concepto_asg == "Poco familiarizado", 1, 0)
DCchoice$asg4 <- ifelse(respuestasCuestionario$concepto_asg == "Nada familiarizado", 1, 0)

DCchoice$hrs <- as.numeric(gsub(",", ".", respuestasCuestionario$hrs_semanal))

# Drop rows with NA in R1
DCchoice <- DCchoice %>% drop_na(R1)

# Define the formula
fmsb <- R1 ~ age + sex + asg1 + asg2 + asg3 + asg4 + hrs | log(BID1)

NPsb <- sbchoice(fmsb, DCchoice)

summary(NPsb)
NPsb
bootCI(NPsb)
summary(NPsb$glm.out)
coefficients(NPsb$glm.out)


