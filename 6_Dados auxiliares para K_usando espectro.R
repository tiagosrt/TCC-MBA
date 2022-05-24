##################################################################################
#                 PACOTES NECESSÁRIOS PARA Random Forest                        #
##################################################################################
#Pacotes utilizados
pacotes <- c(
  'tidyverse',  # Pacote básico de datawrangling
  'rpart',      # Biblioteca de árvores
  'rpart.plot', # Conjunto com Rpart, plota a parvore
  'gtools',     # funções auxiliares como quantcut,
  'Rmisc',      # carrega a função sumarySE para a descritiva
  'scales',     # importa paletas de cores
  'viridis',    # Escalas 'viridis' para o ggplot2
  'caret',       # Funções úteis para machine learning
  'AMR',
  'randomForest',
  'fastDummies',
  'rattle',
  'xgboost',
  'readxl',
  'readr',
  'dplyr'
  
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) }

############################################################
###### Carregando planilha excel e visualizando dados ##########
##########################################################
dataset <- read_excel("DF_Comp_Modelos_Completo.xlsx")
db2 <- read_csv("db_PCs_Text.csv")

#ajustando Datasets
db2 %>% select(ID,Clay, Sand, Score_PC1, Score_PC2, Score_PC3) -> a

colunas_spectra <- make.names(names(dataset[,c(32:1489)])) #coloca um X no nome das colunas
spectra_data <- dataset[,c(32:1489)]
spectra_data <- rename_with(spectra_data, .cols = everything(), 
                            .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

dataset %>% select(ID, Set, Field, K) %>% left_join(a, by = "ID") %>% bind_cols(spectra_data) -> dataset2

str(dataset2)


#janitor::clean_names(dataset)-> dataset2 #funcao para limpar nome de variaveis

#definindo a coluna ID como row names
row.names(dataset) <- dataset$ID

#deicando nome compativeis com RF
colunas_spectra <- make.names(names(dataset[,c(32:1489)]))
spectra_data <- dataset[,c(32:1489)]

spectra_data <- rename_with(spectra_data, .cols = everything(), 
                            .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

rm(a, db2)
########################################################
### Estimando o modelo 6 (spectra + textura) para K ###
########################################################

#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
rm(dataset_rf_Cal, dataset_rf_Val)

dataset_rf_Cal <- dataset2 %>% 
  select('K', 'Set', 'Clay', 'Sand', 10:1467) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset2 %>% 
  select('K', 'Set', 'Clay', 'Sand', 10:1467) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 


# Semente aleatória para buscar a reprodutibilidade
set.seed(2360873)

# medir tempo de execução (iniciar o cronometro)
tempo_ini <- Sys.time()

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- caret::trainControl( #objeto controle tem as especificações e como sera o treinamento
  method='repeatedcv', # Solicita um K-Fold com repetições
  number=4, # Número de FOLDS (o k do k-fold)
  repeats=3, # Número de repetições
  search='grid', # especifica o grid-search
  #summaryFunction = twoClassSummary, # Função de avaliação de performance que sera por curva ROC
  #classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
grid <- base::expand.grid(.mtry=c(1236) ) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10
#579, 652, 725, 798, 871, 944, 1017, 1090, 1163, 1236, 1310, 1384, 1458

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo6_K <- caret::train(K ~ .,         # Fórmula (todas as variáveis)
                          data = dataset_rf_Cal,       # Base de dados
                          method = 'rf',        # Random-forest
                          #metric='ROC',         # Escolhe o melhor por essa métrica
                          trControl = controle, # Parâmetros de controle do algoritmo
                          ntree=300,            # Numero de árvores
                          tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo6_K)
plot(modelo6_K)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M6_K <- predict(modelo6_K, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M6_K <- dataset_rf_Cal$fitted_M6_K - dataset_rf_Cal$K #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M6_K, y = residuos_M6_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 6 para K ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M6_K - mean(dataset_rf_Cal$K))^2))/
  ((sum((dataset_rf_Cal$fitted_M6_K - mean(dataset_rf_Cal$K))^2)) + (sum((dataset_rf_Cal$residuos_M6_K)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M6_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$K))

#RPD
d <- sd(dataset_rf_Cal$K)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$K, probs = 0.75)-quantile(dataset_rf_Cal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M6K_Cal <- data.frame(ID = 'M6_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 6 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M6_K <- predict(modelo6_K, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M6_K <- dataset_rf_Val$fitted_M6_K - dataset_rf_Val$K #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M6_K - mean(dataset_rf_Val$K))^2))/
  ((sum((dataset_rf_Val$fitted_M6_K - mean(dataset_rf_Val$K))^2)) + (sum((dataset_rf_Val$residuos_M6_K)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M6_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$K))

#RPD
d <- sd(dataset_rf_Val$K)/b

#RPIQ
e <- (quantile(dataset_rf_Val$K, probs = 0.75)-quantile(dataset_rf_Val$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 6 - K - validacao
Result_M6K_Val <- data.frame(ID = 'M6_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 6 - K 
Result_M6K <- bind_rows(Result_M6K_Cal, Result_M6K_Val)
rm(Result_M6K_Cal,Result_M6K_Val)

#write.table(Result_M6K, file='Result_M6_K.csv',row.names = F, sep=',')


#Dataset com fitted values da Cal e Val
dataset %>% select("ID","Field","Set","K") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M6_K) %>% dplyr::rename(Fit_M6_K = "dataset_rf_Cal$fitted_M6_K") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","K") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M6_K) %>% dplyr::rename(Fit_M6_K = "dataset_rf_Val$fitted_M6_K") -> Res_fit_rf_Val



pacotes <- c("plotly","gridExtra", "tidyverse","ggrepel","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "readxl","rqPen","ggrepel", "rayshader","psych","pracma",
             "polynom", "prospectr", "pls", "janitor")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



### Grafico de pontos para K

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M6_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration - RF(spec + text) for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M6_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "RF (spec + text) for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

grid.arrange(G3 , G4 ,
             G3, G4,
             ncol=2, nrow=2) #do pacote gridExtra


########################################################
### Estimando o modelo 7 (spectra + spectra) para K ###
########################################################

#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
rm(dataset_rf_Cal, dataset_rf_Val)

dataset_rf_Cal <- dataset2 %>% 
  select('K', 'Set', 7:1467) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset2 %>% 
  select('K', 'Set', 7:1467) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 


# Semente aleatória para buscar a reprodutibilidade
set.seed(2360873)

# medir tempo de execução (iniciar o cronometro)
tempo_ini <- Sys.time()

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- caret::trainControl( #objeto controle tem as especificações e como sera o treinamento
  method='repeatedcv', # Solicita um K-Fold com repetições
  number=4, # Número de FOLDS (o k do k-fold)
  repeats=3, # Número de repetições
  search='grid', # especifica o grid-search
  #summaryFunction = twoClassSummary, # Função de avaliação de performance que sera por curva ROC
  #classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
grid <- base::expand.grid(.mtry=c(1236) ) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10
#579, 652, 725, 798, 871, 944, 1017, 1090, 1163, 1236, 1310

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo7_K <- caret::train(K ~ .,         # Fórmula (todas as variáveis)
                          data = dataset_rf_Cal,       # Base de dados
                          method = 'rf',        # Random-forest
                          #metric='ROC',         # Escolhe o melhor por essa métrica
                          trControl = controle, # Parâmetros de controle do algoritmo
                          ntree=500,            # Numero de árvores
                          tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo7_K)
plot(modelo7_K)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M7_K <- predict(modelo7_K, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M7_K <- dataset_rf_Cal$fitted_M7_K - dataset_rf_Cal$K #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M7_K, y = residuos_M7_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 7 para K ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M7_K - mean(dataset_rf_Cal$K))^2))/
  ((sum((dataset_rf_Cal$fitted_M7_K - mean(dataset_rf_Cal$K))^2)) + (sum((dataset_rf_Cal$residuos_M7_K)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M7_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$K))

#RPD
d <- sd(dataset_rf_Cal$K)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$K, probs = 0.75)-quantile(dataset_rf_Cal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 7 - Ca - Calibracao
Result_M7K_Cal <- data.frame(ID = 'M7_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 7 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M7_K <- predict(modelo7_K, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M7_K <- dataset_rf_Val$fitted_M7_K - dataset_rf_Val$K #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 7 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M7_K - mean(dataset_rf_Val$K))^2))/
  ((sum((dataset_rf_Val$fitted_M7_K - mean(dataset_rf_Val$K))^2)) + (sum((dataset_rf_Val$residuos_M7_K)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M7_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$K))

#RPD
d <- sd(dataset_rf_Val$K)/b

#RPIQ
e <- (quantile(dataset_rf_Val$K, probs = 0.75)-quantile(dataset_rf_Val$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 7 - K - validacao
Result_M7K_Val <- data.frame(ID = 'M7_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 7 - K 
Result_M7K <- bind_rows(Result_M7K_Cal, Result_M7K_Val)
rm(Result_M7K_Cal,Result_M7K_Val)

#write.table(Result_M7K, file='Result_M7_K.csv',row.names = F, sep=',')

#Dataset com fitted values da Cal e Val
dataset %>% select("ID","Field","Set","K") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M7_K) %>% dplyr::rename(Fit_M7_K = "dataset_rf_Cal$fitted_M7_K") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","K") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M7_K) %>% dplyr::rename(Fit_M7_K = "dataset_rf_Val$fitted_M7_K") -> Res_fit_rf_Val



pacotes <- c("plotly","gridExtra", "tidyverse","ggrepel","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "readxl","rqPen","ggrepel", "rayshader","psych","pracma",
             "polynom", "prospectr", "pls", "janitor")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



### Grafico de pontos para K

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M7_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration RF (spec + vis-NIR) for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M7_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "RF (spec + vis-NIR) for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

grid.arrange(G3 , G4 ,
             G3, G4,
             ncol=2, nrow=2) #do pacote gridExtra

########################################################
### Estimando o modelo 8 (spectra + Text + spectra) para K ###
########################################################

#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
rm(dataset_rf_Cal, dataset_rf_Val)

dataset_rf_Cal <- dataset2 %>% 
  select('K', 'Set', 5:1467) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset2 %>% 
  select('K', 'Set', 5:1467) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 


# Semente aleatória para buscar a reprodutibilidade
set.seed(2360873)

# medir tempo de execução (iniciar o cronometro)
tempo_ini <- Sys.time()

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- caret::trainControl( #objeto controle tem as especificações e como sera o treinamento
  method='repeatedcv', # Solicita um K-Fold com repetições
  number=4, # Número de FOLDS (o k do k-fold)
  repeats=3, # Número de repetições
  search='grid', # especifica o grid-search
  #summaryFunction = twoClassSummary, # Função de avaliação de performance que sera por curva ROC
  #classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
grid <- base::expand.grid(.mtry=1090 ) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10
#579, 652, 725, 798, 871, 944, 1017, 1090, 1163, 1236, 1310

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo8_K <- caret::train(K ~ .,         # Fórmula (todas as variáveis)
                          data = dataset_rf_Cal,       # Base de dados
                          method = 'rf',        # Random-forest
                          #metric='ROC',         # Escolhe o melhor por essa métrica
                          trControl = controle, # Parâmetros de controle do algoritmo
                          ntree=500,            # Numero de árvores
                          tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo8_K)
plot(modelo8_K)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M8_K <- predict(modelo8_K, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M8_K <- dataset_rf_Cal$fitted_M8_K - dataset_rf_Cal$K #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M8_K, y = residuos_M8_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 8 para K ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M8_K - mean(dataset_rf_Cal$K))^2))/
  ((sum((dataset_rf_Cal$fitted_M8_K - mean(dataset_rf_Cal$K))^2)) + (sum((dataset_rf_Cal$residuos_M8_K)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M8_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$K))

#RPD
d <- sd(dataset_rf_Cal$K)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$K, probs = 0.75)-quantile(dataset_rf_Cal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 8 - Ca - Calibracao
Result_M8K_Cal <- data.frame(ID = 'M8_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 8 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M8_K <- predict(modelo8_K, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M8_K <- dataset_rf_Val$fitted_M8_K - dataset_rf_Val$K #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 8 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M8_K - mean(dataset_rf_Val$K))^2))/
  ((sum((dataset_rf_Val$fitted_M8_K - mean(dataset_rf_Val$K))^2)) + (sum((dataset_rf_Val$residuos_M8_K)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M8_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$K))

#RPD
d <- sd(dataset_rf_Val$K)/b

#RPIQ
e <- (quantile(dataset_rf_Val$K, probs = 0.75)-quantile(dataset_rf_Val$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 8 - K - validacao
Result_M8K_Val <- data.frame(ID = 'M8_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 8 - K 
Result_M8K <- bind_rows(Result_M8K_Cal, Result_M8K_Val)
rm(Result_M8K_Cal,Result_M8K_Val)

#write.table(Result_M8K, file='Result_M8_K.csv',row.names = F, sep=',')


#Dataset com fitted values da Cal e Val
dataset %>% select("ID","Field","Set","K") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M8_K) %>% dplyr::rename(Fit_M8_K = "dataset_rf_Cal$fitted_M8_K") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","K") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M8_K) %>% dplyr::rename(Fit_M8_K = "dataset_rf_Val$fitted_M8_K") -> Res_fit_rf_Val



pacotes <- c("plotly","gridExtra", "tidyverse","ggrepel","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "readxl","rqPen","ggrepel", "rayshader","psych","pracma",
             "polynom", "prospectr", "pls", "janitor")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



### Grafico de pontos para K

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M8_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration - RF (spec + text + vis-NIR) for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M8_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "RF (spec + text + vis-NIR) for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

grid.arrange(G3 , G4 ,
             G3, G4,
             ncol=2, nrow=2) #do pacote gridExtra
