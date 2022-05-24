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
  'readxl'
  
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
str(dataset)
names(dataset)

#janitor::clean_names(dataset)-> dataset2 #funcao para limpar nome de variaveis

#definindo a coluna ID como row names
row.names(dataset) <- dataset$ID

#deicando nome compativeis com RF
colunas_spectra <- make.names(names(dataset[,c(32:1489)]))
spectra_data <- dataset[,c(32:1489)]

spectra_data <- rename_with(spectra_data, .cols = everything(), 
.fn = str_replace, pattern = ".*", replacement = colunas_spectra)

#Exemplo: names(flight) <- make.names(names(flight))

##deixando apenas dois digitos apos a virgula para variavies espectrais ##Semelhante PLS

#colunas_spectra <- as.character(round(as.numeric(names(dataset[,c(32:1489)])), digits = 2))
#spectra_data <- dataset[,c(32:1489)]

#spectra_data <- rename_with(spectra_data, .cols = everything(), 
#                            .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
names(dataset)

dataset_rf_Cal <- dataset %>% 
  select('Ca', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

names(dataset_rf_Cal)

dataset_rf_Val <- dataset %>% 
  select('Ca', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 

#write.table(datasetCal, file='datasetCal_spectra.csv',row.names = F, sep=',') 

########################################################################
######         Modelo 5 - RF com espectro inteiro          #########
######                 (apos alinhar os espectros)         #########
########################################################################

####################################
### Estimando o modelo 5 para Ca ###
####################################

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
grid <- base::expand.grid(.mtry=798) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10
  #mtry=c(579, 652, 725, 798, 871, 944, 1017, 1090)

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo5_Ca <- caret::train(Ca ~ .,         # Fórmula (todas as variáveis)
                              data = dataset_rf_Cal,       # Base de dados
                              method = 'rf',        # Random-forest
                              #metric='ROC',         # Escolhe o melhor por essa métrica
                              trControl = controle, # Parâmetros de controle do algoritmo
                              ntree=500,            # Numero de árvores
                              tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo5_Ca)
plot(modelo5_Ca)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M5_Ca <- predict(modelo5_Ca, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M5_Ca <- dataset_rf_Cal$fitted_M5_Ca - dataset_rf_Cal$Ca #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M5_Ca, y = residuos_M5_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 5 para Ca ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M5_Ca - mean(dataset_rf_Cal$Ca))^2))/
  ((sum((dataset_rf_Cal$fitted_M5_Ca - mean(dataset_rf_Cal$Ca))^2)) + (sum((dataset_rf_Cal$residuos_M5_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M5_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$Ca))

#RPD
d <- sd(dataset_rf_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$Ca, probs = 0.75)-quantile(dataset_rf_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M5Ca_Cal <- data.frame(ID = 'M5_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 5 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M5_Ca <- predict(modelo5_Ca, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M5_Ca <- dataset_rf_Val$fitted_M5_Ca - dataset_rf_Val$Ca #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M5_Ca - mean(dataset_rf_Val$Ca))^2))/
  ((sum((dataset_rf_Val$fitted_M5_Ca - mean(dataset_rf_Val$Ca))^2)) + (sum((dataset_rf_Val$residuos_M5_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M5_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$Ca))

#RPD
d <- sd(dataset_rf_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Val$Ca, probs = 0.75)-quantile(dataset_rf_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - validacao
Result_M5Ca_Val <- data.frame(ID = 'M5_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - Ca 
Result_M5Ca <- bind_rows(Result_M5Ca_Cal, Result_M5Ca_Val)
rm(Result_M5Ca_Cal,Result_M5Ca_Val)

#AQUI AQUI AQUI
#Dataset com fitted values da Cal e Val

dataset %>% select("ID","Field","Set","Ca","K") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M5_Ca) %>% dplyr::rename(Fit_M5_Ca = "dataset_rf_Cal$fitted_M5_Ca") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","Ca","K") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M5_Ca) %>% dplyr::rename(Fit_M5_Ca = "dataset_rf_Val$fitted_M5_Ca") -> Res_fit_rf_Val

###Graficos
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


### Grafico de pontos para Ca

G1 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M5_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration using RF for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
         axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
         axis.title = element_text(size = 13),
         legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))
#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

G2 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M5_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "RF for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

### Grafico de pontos para Ca

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M5_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration using RF for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
         axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
         axis.title = element_text(size = 13),
         legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))
#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M5_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "RF for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

grid.arrange(G1 , G2 , #do pacote gridExtra
             G3 , G4 ,
             ncol=2, nrow=2)



####################################
### Estimando o modelo 5 para K ###
####################################
rm(dataset_rf_Cal, dataset_rf_Val, grid, controle)

#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val

dataset_rf_Cal <- dataset %>% 
  select('K', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset %>% 
  select('K', 'Set') %>% 
  cbind(spectra_data) %>% 
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
grid <- base::expand.grid(.mtry=652) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10
  #579, 652, 725, 798, 871, 944, 1017, 1090, 1163, 1236, 1310, 1384, 1458

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo5_K <- caret::train(K ~ .,         # Fórmula (todas as variáveis)
                           data = dataset_rf_Cal,       # Base de dados
                           method = 'rf',        # Random-forest
                           #metric='ROC',         # Escolhe o melhor por essa métrica
                           trControl = controle, # Parâmetros de controle do algoritmo
                           ntree=300,            # Numero de árvores
                           tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo5_K)
plot(modelo5_K)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M5_K <- predict(modelo5_K, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M5_K <- dataset_rf_Cal$fitted_M5_K - dataset_rf_Cal$K #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M5_K, y = residuos_M5_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 5 para K ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M5_K - mean(dataset_rf_Cal$K))^2))/
  ((sum((dataset_rf_Cal$fitted_M5_K - mean(dataset_rf_Cal$K))^2)) + (sum((dataset_rf_Cal$residuos_M5_K)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M5_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$K))

#RPD
d <- sd(dataset_rf_Cal$K)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$K, probs = 0.75)-quantile(dataset_rf_Cal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M5K_Cal <- data.frame(ID = 'M5_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 5 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M5_K <- predict(modelo5_K, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M5_K <- dataset_rf_Val$fitted_M5_K - dataset_rf_Val$K #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M5_K - mean(dataset_rf_Val$K))^2))/
  ((sum((dataset_rf_Val$fitted_M5_K - mean(dataset_rf_Val$K))^2)) + (sum((dataset_rf_Val$residuos_M5_K)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M5_K)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$K))

#RPD
d <- sd(dataset_rf_Val$K)/b

#RPIQ
e <- (quantile(dataset_rf_Val$K, probs = 0.75)-quantile(dataset_rf_Val$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 5 - K - validacao
Result_M5K_Val <- data.frame(ID = 'M5_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 5 - K 
Result_M5K <- bind_rows(Result_M5K_Cal, Result_M5K_Val)
rm(Result_M5K_Cal,Result_M5K_Val)

#Resumo geral - Resultados Modelo 5 
Result_M5 <- bind_rows(Result_M5K, Result_M5Ca)
rm(Result_M5K,Result_M5Ca)

write.table(Result_M5, file='Result_M5.csv',row.names = F, sep=',')

#Dataset com fitted values da Cal e Val

Res_fit_rf_Cal %>% cbind(dataset_rf_Cal$fitted_M5_K) %>% dplyr::rename(Fit_M5_K = "dataset_rf_Cal$fitted_M5_K") -> Res_fit_rf_Cal

Res_fit_rf_Val %>% cbind(dataset_rf_Val$fitted_M5_K) %>% dplyr::rename(Fit_M5_K = "dataset_rf_Val$fitted_M5_K") -> Res_fit_rf_Val

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


### Grafico de pontos para Ca

G1 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M5_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration using RF for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

G2 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M5_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "RF for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

### Grafico de pontos para K

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M5_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration using RF for K") +
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
  geom_point(aes(x = K, y = Fit_M5_K, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "RF for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

grid.arrange(G1 , G2 , #do pacote gridExtra
             G3 , G4 ,
             ncol=2, nrow=2)

