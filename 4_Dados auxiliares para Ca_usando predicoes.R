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

#colunas_spectra <- make.names(names(dataset[,c(32:1489)])) #coloca um X no nome das colunas
#spectra_data <- dataset[,c(32:1489)]
#spectra_data <- rename_with(spectra_data, .cols = everything(), 
#                            .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

dataset %>% select(ID, Set, Field, Ca, Pred.Ca) %>% left_join(a, by = "ID")  -> dataset2

str(dataset2)


#janitor::clean_names(dataset)-> dataset2 #funcao para limpar nome de variaveis

#definindo a coluna ID como row names
#row.names(dataset) <- dataset$ID

#deicando nome compativeis com RF
#colunas_spectra <- make.names(names(dataset[,c(32:1489)]))
#spectra_data <- dataset[,c(32:1489)]

#spectra_data <- rename_with(spectra_data, .cols = everything(), 
#                           .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

rm(a, db2)
########################################################
### Estimando o modelo 9 (Pred + textura) para Ca ###
########################################################
#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
rm(dataset_rf_Cal, dataset_rf_Val)

dataset_rf_Cal <- dataset2 %>% 
  select('Ca', 'Set', 'Clay', 'Sand', 'Pred.Ca') %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset2 %>% 
  select('Ca', 'Set', 'Clay', 'Sand', 'Pred.Ca') %>% 
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
grid <- base::expand.grid(.mtry=c(1:3) ) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo9_Ca <- caret::train(Ca ~ .,         # Fórmula (todas as variáveis)
                          data = dataset_rf_Cal,       # Base de dados
                          method = 'rf',        # Random-forest
                          #metric='ROC',         # Escolhe o melhor por essa métrica
                          trControl = controle, # Parâmetros de controle do algoritmo
                          ntree=300,            # Numero de árvores
                          tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo9_Ca)
plot(modelo9_Ca)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M9_Ca <- predict(modelo9_Ca, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M9_Ca <- dataset_rf_Cal$fitted_M9_Ca - dataset_rf_Cal$Ca #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M9_Ca, y = residuos_M9_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 9 para Ca ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M9_Ca - mean(dataset_rf_Cal$Ca))^2))/
  ((sum((dataset_rf_Cal$fitted_M9_Ca - mean(dataset_rf_Cal$Ca))^2)) + (sum((dataset_rf_Cal$residuos_M9_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M9_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$Ca))

#RPD
d <- sd(dataset_rf_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$Ca, probs = 0.75)-quantile(dataset_rf_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M9Ca_Cal <- data.frame(ID = 'M9_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 6 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M9_Ca <- predict(modelo9_Ca, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M9_Ca <- dataset_rf_Val$fitted_M9_Ca - dataset_rf_Val$Ca #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M9_Ca - mean(dataset_rf_Val$Ca))^2))/
  ((sum((dataset_rf_Val$fitted_M9_Ca - mean(dataset_rf_Val$Ca))^2)) + (sum((dataset_rf_Val$residuos_M9_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M9_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$Ca))

#RPD
d <- sd(dataset_rf_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Val$Ca, probs = 0.75)-quantile(dataset_rf_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 9 - Ca - validacao
Result_M9Ca_Val <- data.frame(ID = 'M9_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 9 - Ca 
Result_M9Ca <- bind_rows(Result_M9Ca_Cal, Result_M9Ca_Val)
rm(Result_M9Ca_Cal,Result_M9Ca_Val)

#write.table(Result_M9Ca, file='Result_M9_Ca.csv',row.names = F, sep=',')


#Dataset com fitted values da Cal e Val
dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M9_Ca) %>% dplyr::rename(Fit_M9_Ca = "dataset_rf_Cal$fitted_M9_Ca") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M9_Ca) %>% dplyr::rename(Fit_M9_Ca = "dataset_rf_Val$fitted_M9_Ca") -> Res_fit_rf_Val



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

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M9_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - RF (pred + text) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M9_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "RF (pred + text) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
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
### Estimando o modelo 10 (Pred + visNIR) para Ca ###
########################################################
#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
rm(dataset_rf_Cal, dataset_rf_Val)

dataset_rf_Cal <- dataset2 %>% 
  select('Ca', 'Set',"Score_PC1", "Score_PC2", "Score_PC3", 'Pred.Ca') %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset2 %>% 
  select('Ca', 'Set', "Score_PC1", "Score_PC2", "Score_PC3", 'Pred.Ca') %>% 
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
grid <- base::expand.grid(.mtry=c(1:4) ) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo10_Ca <- caret::train(Ca ~ .,         # Fórmula (todas as variáveis)
                           data = dataset_rf_Cal,       # Base de dados
                           method = 'rf',        # Random-forest
                           #metric='ROC',         # Escolhe o melhor por essa métrica
                           trControl = controle, # Parâmetros de controle do algoritmo
                           ntree=300,            # Numero de árvores
                           tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo10_Ca)
plot(modelo10_Ca)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M10_Ca <- predict(modelo10_Ca, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M10_Ca <- dataset_rf_Cal$fitted_M10_Ca - dataset_rf_Cal$Ca #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M10_Ca, y = residuos_M10_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 10 para Ca ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M10_Ca - mean(dataset_rf_Cal$Ca))^2))/
  ((sum((dataset_rf_Cal$fitted_M10_Ca - mean(dataset_rf_Cal$Ca))^2)) + (sum((dataset_rf_Cal$residuos_M10_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M10_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$Ca))

#RPD
d <- sd(dataset_rf_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$Ca, probs = 0.75)-quantile(dataset_rf_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M10Ca_Cal <- data.frame(ID = 'M10_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 6 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M10_Ca <- predict(modelo10_Ca, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M10_Ca <- dataset_rf_Val$fitted_M10_Ca - dataset_rf_Val$Ca #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M10_Ca - mean(dataset_rf_Val$Ca))^2))/
  ((sum((dataset_rf_Val$fitted_M10_Ca - mean(dataset_rf_Val$Ca))^2)) + (sum((dataset_rf_Val$residuos_M10_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M10_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$Ca))

#RPD
d <- sd(dataset_rf_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Val$Ca, probs = 0.75)-quantile(dataset_rf_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 10 - Ca - validacao
Result_M10Ca_Val <- data.frame(ID = 'M10_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 10 - Ca 
Result_M10Ca <- bind_rows(Result_M10Ca_Cal, Result_M10Ca_Val)
rm(Result_M10Ca_Cal,Result_M10Ca_Val)

#write.table(Result_M10Ca, file='Result_M10_Ca.csv',row.names = F, sep=',')


#Dataset com fitted values da Cal e Val
dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M10_Ca) %>% dplyr::rename(Fit_M10_Ca = "dataset_rf_Cal$fitted_M10_Ca") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M10_Ca) %>% dplyr::rename(Fit_M10_Ca = "dataset_rf_Val$fitted_M10_Ca") -> Res_fit_rf_Val



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

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M10_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - RF (pred + vis-NIR) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M10_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "RF (pred + vis-NIR) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
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
### Estimando o modelo 11 (Pred + textura + visNIR) para Ca ###
########################################################
#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
rm(dataset_rf_Cal, dataset_rf_Val)

dataset_rf_Cal <- dataset2 %>% 
  select('Ca', 'Set', "Clay", "Sand" ,"Score_PC1", "Score_PC2", "Score_PC3", 'Pred.Ca') %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

dataset_rf_Val <- dataset2 %>% 
  select('Ca', 'Set', "Clay", "Sand" ,"Score_PC1", "Score_PC2", "Score_PC3", 'Pred.Ca') %>% 
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
grid <- base::expand.grid(.mtry=c(1:6) ) #.mtry informa que estamos variando o numero de variaveis testadas de 1 a 10

# Vamos treinar todos os modelos do grid-search com cross-validation
modelo11_Ca <- caret::train(Ca ~ .,         # Fórmula (todas as variáveis)
                            data = dataset_rf_Cal,       # Base de dados
                            method = 'rf',        # Random-forest
                            #metric='ROC',         # Escolhe o melhor por essa métrica
                            trControl = controle, # Parâmetros de controle do algoritmo
                            ntree=300,            # Numero de árvores
                            tuneGrid = grid)      # Percorre o grid especificado aqui

print(modelo11_Ca)
plot(modelo11_Ca)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

# Ver VIP

### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_rf_Cal$fitted_M11_Ca <- predict(modelo11_Ca, dataset_rf_Cal) #add fitted values pro dataset
dataset_rf_Cal$residuos_M11_Ca <- dataset_rf_Cal$fitted_M11_Ca - dataset_rf_Cal$Ca #add residos pro dataset

dataset_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M11_Ca, y = residuos_M11_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Resíduos") +
  #abline(h = 0) + #nao funcionou 
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 11 para Ca ####

#R²
a <- (sum((dataset_rf_Cal$fitted_M11_Ca - mean(dataset_rf_Cal$Ca))^2))/
  ((sum((dataset_rf_Cal$fitted_M11_Ca - mean(dataset_rf_Cal$Ca))^2)) + (sum((dataset_rf_Cal$residuos_M11_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_rf_Cal$residuos_M11_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Cal$Ca))

#RPD
d <- sd(dataset_rf_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Cal$Ca, probs = 0.75)-quantile(dataset_rf_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M11Ca_Cal <- data.frame(ID = 'M11_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 6 para Ca usando o conjunto de validação ####
dataset_rf_Val$fitted_M11_Ca <- predict(modelo11_Ca, dataset_rf_Val) #add fitted values pro dataset
dataset_rf_Val$residuos_M11_Ca <- dataset_rf_Val$fitted_M11_Ca - dataset_rf_Val$Ca #add residos pro dataset

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_rf_Val$fitted_M11_Ca - mean(dataset_rf_Val$Ca))^2))/
  ((sum((dataset_rf_Val$fitted_M11_Ca - mean(dataset_rf_Val$Ca))^2)) + (sum((dataset_rf_Val$residuos_M11_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_rf_Val$residuos_M11_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_rf_Val$Ca))

#RPD
d <- sd(dataset_rf_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_rf_Val$Ca, probs = 0.75)-quantile(dataset_rf_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 11 - Ca - validacao
Result_M11Ca_Val <- data.frame(ID = 'M11_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 11 - Ca 
Result_M11Ca <- bind_rows(Result_M11Ca_Cal, Result_M11Ca_Val)
rm(Result_M11Ca_Cal,Result_M11Ca_Val)

write.table(Result_M11Ca, file='Result_M11_Ca.csv',row.names = F, sep=',')


#Dataset com fitted values da Cal e Val
dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "CAL") %>% cbind(dataset_rf_Cal$fitted_M11_Ca) %>% dplyr::rename(Fit_M11_Ca = "dataset_rf_Cal$fitted_M11_Ca") -> Res_fit_rf_Cal

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "VAL") %>% cbind(dataset_rf_Val$fitted_M11_Ca) %>% dplyr::rename(Fit_M11_Ca = "dataset_rf_Val$fitted_M11_Ca") -> Res_fit_rf_Val



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

G3 <- Res_fit_rf_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M11_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - RF (pred + text + vis-NIR) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_rf_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M11_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "RF (pred + text + vis-NIR) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

grid.arrange(G3 , G4 ,
             G3, G4,
             ncol=2, nrow=2) #do pacote gridExtra
