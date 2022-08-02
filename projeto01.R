# define diretório de trabalho
setwd('myDirectory')
getwd()

# carrega bibliotecas necessárias
library(readxl)
library(caret)
library(corrplot)
library(caTools)
library(tidyverse)

# cria dataset com a planilha fornecida
df <- read_xlsx('FEV-data-Excel.xlsx')
View(df)

# verifica informações gerais do dataset
class(df)
dim(df)
str(df)


# verificando se existem valores faltantes
complete_cases <- sum(complete.cases(df))
complete_cases
incomplete_cases <- sum(!complete.cases(df))
incomplete_cases

percentual_faltantes <- (incomplete_cases / complete_cases) * 100
percentual_faltantes


# removendo os valores NA do dataset
df <- na.omit(df)
View(df)

# histograma para visualizar a média de consumo de todo o dataset
hist(df$`mean - Energy consumption [kWh/100 km]`,
     xlim = c(10, 30),
     ylim = c(0, 15),
     ylab = 'Frequência',
     xlab = 'Média',
     main = 'Média de Consumo de Energia',
     col = c('light blue', 'dark blue'))

# extraindo variáveis numéricas
numeric_variable_list <- sapply(df, is.numeric)
numerical_data <- df[numeric_variable_list]
View(numerical_data)
names(numerical_data)
class(numerical_data)

# normalizando os dados, dividindo cada valor pelo maior valor da mesma coluna
colnames(numerical_data)
norm_data <- numerical_data %>%
  mutate(Minimal_price_norm = `Minimal price (gross) [PLN]`/max(`Minimal price (gross) [PLN]`, na.rm = T)) %>%
  mutate(Engine_power_norm = `Engine power [KM]`/max(`Engine power [KM]`, na.rm = T)) %>%
  mutate(Maximum_torque_norm = `Maximum torque [Nm]`/max(`Maximum torque [Nm]`, na.rm = T)) %>%
  mutate(Battery_capacity_norm = `Battery capacity [kWh]`/max(`Battery capacity [kWh]`, na.rm = T)) %>%
  mutate(Range_norm = `Range (WLTP) [km]`/max(`Range (WLTP) [km]`, na.rm = T)) %>%
  mutate(Wheelbase_norm = `Wheelbase [cm]`/max(`Wheelbase [cm]`, na.rm = T)) %>%
  mutate(Length_norm = `Length [cm]`/max(`Length [cm]`, na.rm = T)) %>%
  mutate(Width_norm = `Width [cm]`/max(`Width [cm]`, na.rm = T)) %>%
  mutate(Height_norm = `Height [cm]`/max(`Height [cm]`, na.rm = T)) %>%
  mutate(Minimal_empty_weight_norm = `Minimal empty weight [kg]`/max(`Minimal empty weight [kg]`, na.rm = T)) %>%
  mutate(Permissable_gross_weigth_norm = `Permissable gross weight [kg]`/max(`Permissable gross weight [kg]`, na.rm = T)) %>%
  mutate(Maximum_load_capacity_norm = `Maximum load capacity [kg]`/max(`Maximum load capacity [kg]`, na.rm = T)) %>%
  mutate(Number_seats_norm = `Number of seats`/max(`Number of seats`, na.rm = T)) %>%
  mutate(Number_doors_norm = `Number of doors`/max(`Number of doors`, na.rm = T)) %>%
  mutate(Tire_size_norm = `Tire size [in]`/max(`Tire size [in]`, na.rm = T)) %>%
  mutate(Maximum_speed_norm = `Maximum speed [kph]`/max(`Maximum speed [kph]`, na.rm = T)) %>%
  mutate(Boot_capacity_norm = `Boot capacity (VDA) [l]`/max(`Boot capacity (VDA) [l]`, na.rm = T)) %>%
  mutate(Acceleration_norm = `Acceleration 0-100 kph [s]`/max(`Acceleration 0-100 kph [s]`, na.rm = T)) %>%
  mutate(Maximum_charging_power_norm = `Maximum DC charging power [kW]`/max(`Maximum DC charging power [kW]`, na.rm = T))

View(norm_data)

# selecionando as tabelas que serão usadas
select_data <- norm_data %>%
  select(`mean - Energy consumption [kWh/100 km]`,
         Minimal_price_norm,
         Engine_power_norm,
         Maximum_torque_norm, 
         Battery_capacity_norm, 
         Range_norm, 
         Wheelbase_norm,
         Length_norm,
         Width_norm,
         Height_norm,
         Minimal_empty_weight_norm,
         Permissable_gross_weigth_norm,
         Maximum_load_capacity_norm,
         Number_seats_norm,
         Number_doors_norm,
         Tire_size_norm,
         Maximum_speed_norm,
         Boot_capacity_norm,
         Acceleration_norm,
         Maximum_charging_power_norm)

View(select_data)

# matriz de correlação
cor(select_data)

# plots de correlação
pairs(select_data[1:5], labels = colnames(select_data)[1:5])
pairs(select_data[6:10], labels = colnames(select_data)[6:10])
pairs(select_data[11:15], labels = colnames(select_data)[11:15])
pairs(select_data[16:20], labels = colnames(select_data)[16:20])

# plotando tabela de correlação com corrplot
cor.data <- cor(select_data[,])
cor.data
corrplot(cor.data, method = 'color', tl.cex = 0.8)


# dividindo o dataset em treino e teste
split <- sample.split(select_data$`mean - Energy consumption [kWh/100 km]`, SplitRatio = 0.7)
train_data <- subset(select_data, split == 'TRUE') 
test_data <- subset(select_data, split == 'FALSE') 

dim(train_data)
dim(test_data)


# criando modelo de regressão e retirando variáveis que não possuam grande relevância estatística
modelo <- lm(`mean - Energy consumption [kWh/100 km]` ~ .
             - Height_norm
             - Number_doors_norm
             - Minimal_price_norm
             - Permissable_gross_weigth_norm
             - Wheelbase_norm
             - Width_norm
             - Battery_capacity_norm
             - Maximum_speed_norm
             - Acceleration_norm
             - Maximum_charging_power_norm
             - Boot_capacity_norm
             - Maximum_torque_norm
             - Number_seats_norm,
             data = train_data)
summary(modelo)
plot(modelo)

previsoes <- predict(modelo, test_data)
previsoes

# listando resíduos(score do erro da previsão)
res <- residuals(modelo)
res <- as.data.frame(res)
res

# compara o valor previsto vs valor real
results <- cbind(previsoes, test_data$`mean - Energy consumption [kWh/100 km]`)
colnames(results) <- c('predicted', 'real')
results <- as.data.frame(results)
results
correlation_accuracy <- cor(results)
correlation_accuracy

# Plotando comparação entre previstos vs valores atuais
plot(test_data$`mean - Energy consumption [kWh/100 km]`, type = 'l', lty = 1.8, col = "red")

# acrescenta linha das previsões
lines(previsoes, type = 'l', col = 'blue')

# somente previsões
plot(previsoes, type = 'l', col = 'blue')
