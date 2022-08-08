# define diretório de trabalho
setwd('C:/Users/03312022126/OneDrive/DSA/FCD/BigDataRAzure/Projetos Finais/Projeto 02/')
getwd()

# carrega bibliotecas necessárias
library(readxl)
library(caret)
library(scales) # biblioteca para normalizar dados
library(rpart)
library(rpart.plot)
library(randomForest)
library(naivebayes)
library(e1071)
library(corrplot)
library(caTools)

# cria dataset com a planilha fornecida
?read_xlsx
df <- read_xlsx('Acoustic_Extinguisher_Fire_Dataset.xlsx', sheet = 'A_E_Fire_Dataset')
View(df)


# verifica informações gerais do dataset
class(df)
dim(df)
str(df)
head(df, 10)

# alterando coluna STATUS para tipo fator
df$STATUS <- as.logical(df$STATUS)
df$FUEL <- as.factor(df$FUEL)
str(df)
View(df)

# verifica existencia de dados NA no dataset
complete_cases <- sum(complete.cases(df))
complete_cases
incomplete_cases <- sum(!complete.cases(df))
incomplete_cases


# percentual
percentual_registros_faltantes <- (incomplete_cases / complete_cases) * 100
percentual_registros_faltantes

# verifica distribuição por colunas
table(df$SIZE)
table(df$FUEL)
table(df$STATUS)
table(df$FREQUENCY)


# extraindo variáveis numéricas
numeric_variable_list <- sapply(df, is.numeric)
df_numerical <- df[numeric_variable_list]
View(df_numerical)
names(df_numerical)
class(df_numerical)


df_numerical$SIZE <- rescale(df_numerical$SIZE)
df_numerical$DISTANCE <- rescale(df_numerical$DISTANCE)
df_numerical$DESIBEL <- rescale(df_numerical$DESIBEL)
df_numerical$AIRFLOW <- rescale(df_numerical$AIRFLOW)
df_numerical$FREQUENCY <- rescale(df_numerical$FREQUENCY)
head(df_numerical, 10)
?rescale

# matriz de correlação
cor(df_numerical)

# plots de correlação
pairs(df_numerical, labels = colnames(df_numerical))


# plotando tabela de correlação com corrplot
cor.data <- cor(df_numerical[,])
cor.data
?corrplot
corrplot(cor.data, method = 'color', type = 'upper', addCoef.col = 'black', tl.srt = 30, tl.col = 'brown')
cor.test(df_numerical$AIRFLOW, df_numerical$DISTANCE, method = "pearson")


# dividindo o dataset em treino e teste, 70/30
split <- sample.split(df$STATUS, SplitRatio = 0.7)
train_data <- subset(df, split == 'TRUE') 
test_data <- subset(df, split == 'FALSE') 

dim(train_data)
str(train_data)
View(train_data)
dim(test_data)
str(test_data)
View(test_data)






######### criando modelo de regressão #####################################
# acurácia 0.8985           

model_glm <- glm(STATUS ~ .,
             data = train_data,
             family = 'binomial')
summary(model_glm)
plot(model_glm)


# prevendo resultados nos dados de teste
pred_result_glm <- predict(model_glm, test_data, type = 'response')
View(pred_result_glm)

# convertendo resultados. Se maior que 0.5, será TRUE/1, menor FALSE/0
convert_pred_glm <- rep(FALSE, nrow(test_data))
convert_pred_glm[pred_result_glm > 0.5] = TRUE

# verifica dataset convertido
str(convert_pred_glm)

# Criando uma matrix de confusão com os dados de teste para verificar nível de acertos/erros
confusion_matrix <- confusionMatrix(table(data = convert_pred_glm, reference = test_data$STATUS))
confusion_matrix
fourfoldplot(confusion_matrix$table,
             color = c('cyan', 'pink'),
             conf.level = 0,
             margin = 1,
             main = 'Matriz de Confusão',
             
            )
?fourfoldplot



################# criando modelo de árvore de decisão ####################
# acurácia 0.8957


# cria modelo de arvore de decisão
model_tree <- rpart(
  STATUS ~ .,
  data = train_data)

# faz o plot
rpart.plot(model_tree)
printcp(model_tree)
plotcp(model_tree)

# realiza as predições com dados de teste
pred_result_tree <- predict(model_tree, test_data)
pred_result_tree

# convertendo resultados. Se maior que 0.5, será TRUE/1, menor FALSE/0
convert_pred_tree <- rep(FALSE, nrow(test_data))
convert_pred_tree[pred_result_tree > 0.5] = TRUE

# criando matriz de confusão
confusion_matrix_tree <- confusionMatrix(table(data = convert_pred_tree, reference = test_data$STATUS))
confusion_matrix_tree



################# criando modelo de random forest ####################
# acurácia 0.9633

# verifica tipo de dados do dataset
str(df)

# cria o modelo de random forest
model_rforest <- randomForest(
  STATUS ~ .,
  data = train_data,
  importance = TRUE,
  proximity = TRUE)
model_rforest
plot(model_rforest)

# faz previsões com dados de teste
pred_result_rforest <- predict(model_rforest, test_data)
head(pred_result_rforest)

# convertendo resultados. Se maior que 0.5, será TRUE/1, menor FALSE/0
convert_pred_rforest <- rep(FALSE, nrow(test_data))
convert_pred_rforest[pred_result_rforest > 0.5] = TRUE

# cria matriz de confusão
confusion_matrix_tree <- confusionMatrix(table(data = convert_pred_rforest, reference = test_data$STATUS))
confusion_matrix_tree





################# criando modelo de Naive Bayes ####################
# acurácia 0.8714   

# infs dos dados
str(df)

# cria modelo de naive bayes
model_nbayes <- naive_bayes(
  as.character(STATUS) ~ .,
  data = train_data)
summary(model_nbayes)

# plota
plot(model_nbayes)

# faz previsões com dados de teste
pred_result_nbayes <- predict(model_nbayes, test_data)
head(pred_result_nbayes)

# cria matriz de confusão
confusion_matrix_nbayes <- confusionMatrix(table(data = pred_result_nbayes, reference = test_data$STATUS))
confusion_matrix_nbayes





################# criando modelo SVM ####################
#  Accuracy : 0.9197     

# infs dos dados
str(df)

# cria modelo de naive bayes
model_svm <- svm(
  as.numeric(STATUS) ~ .,
  data = train_data)
summary(model_svm)

# plota
plot(model_svm)

# faz previsões com dados de teste
pred_result_svm <- predict(model_svm, test_data)
head(pred_result_svm)

# convertendo resultados. Se maior que 0.5, será TRUE/1, menor FALSE/0
convert_pred_svm <- rep(FALSE, nrow(test_data))
convert_pred_svm[pred_result_svm > 0.5] = TRUE

# cria matriz de confusão
confusion_matrix_svm <- confusionMatrix(table(data = convert_pred_svm, reference = test_data$STATUS))
confusion_matrix_svm

# cria matriz de confusão
confusion_matrix_svm <- confusionMatrix(table(data = convert_pred_svm, reference = test_data$STATUS))
confusion_matrix_svm
