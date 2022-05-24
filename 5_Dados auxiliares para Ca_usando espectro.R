##################################################################################
#                 PACOTES NECESSÁRIOS PARA REGRESSAO SIMPLES, MULTIPLA e PLS    #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly", "gridExtra", "tidyverse","ggrepel","knitr","kableExtra",
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

############################################################
###### Carregando planilha excel e visualizando dados ##########
##########################################################
dataset <- read_excel("DF_Comp_Modelos_Completo.xlsx")

db2 <- read_csv("db_PCs_Text.csv")

#ajustando Datasets
db2 %>% select(ID, Clay, Sand, Score_PC1, Score_PC2, Score_PC3) -> a

colunas_spectra <- make.names(names(dataset[,c(32:1489)])) #coloca um X no nome das colunas
spectra_data <- dataset[,c(32:1489)]
spectra_data <- rename_with(spectra_data, .cols = everything(), 
                            .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

dataset %>% select(ID, Set, Field, Ca) %>% left_join(a, by = "ID") %>% bind_cols(spectra_data) -> dataset2
#str(dataset)
#names(dataset)

#janitor::clean_names(dataset)-> dataset2 #funcao para limpar nome de variaveis

#limpando colunas que nao serao necessarias, juntando com spectra data e definindo set de cal e val
names(dataset)

dataset_pls_Cal <- dataset2 %>% 
  select('Ca', 'Set', 'Clay', 'Sand') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

names(dataset_pls_Cal)

dataset_pls_Val <- dataset2 %>% 
  select('Ca', 'Set', 'Clay', 'Sand') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 

#write.table(datasetCal, file='datasetCal_spectra.csv',row.names = F, sep=',') 

#definindo o espectro
energy <- names(dataset[ ,c(2:1459)])

#Uma forma de plotar os espectros
#matplot(x = energy, y = t(dataset[ ,c(6:1463)]),
#        xlab = "Energy (keV)",
#        ylab = "Intensity (cps)",
#        type = "l",
#        lty = 1,
#        col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 0.3))

########################################################################
######         Modelo 6 - PLS com Textura + espectro inteiro   #########
######                 (apos alinhar os espectros)            #########
########################################################################

####################################
### Estimando o modelo 6 para Ca ###
####################################

maxc <- 15

modelo6_Ca <- plsr(Ca ~ . ,
                   data = dataset_pls_Cal,
                   method = "simpls", #usar "oscorespls"  ou "simpls", o primeiro esta no livro do R
                   scale = FALSE,
                   ncomp = maxc,
                   validation = "LOO") #CV or LOO for leave one out
print(modelo6_Ca)
summary(modelo6_Ca)

#?plsr
plot(modelo6_Ca, "val",
     main = " ",
     xlab = "Number of components",
     legendpos="topright")

# number of components to use
ncomp.onesigma <- selectNcomp(modelo6_Ca, method = "onesigma", plot = TRUE,
)
nc <- ncomp.onesigma

#plotando observed vs. predicted
plot(modelo6_Ca,
     ncomp = nc,
     main = " ",
     xlab = "Observed",
     ylab = "Predicted")

#ploting the first three loadings of the principal components of the spectra (falta colocar legenda)
#plot(modelo6_Ca,
 #    "loadings",
  #   comps = 1:3,
  #   xlab = "Index of the wavelength",
  #   ylab = "Loading value")

# Standardized regression coefficient of the PLSR model
#plot(energy, modelo6_Ca$coefficients[,1,nc],
#     main = " ",
#     type = "l",
#     xlab = "Wavelength /nm",
#     ylab = "Regression coefficient")
#abline(h = 0)

# compute the variable importance, see Wold et al., (1993)
# take the loadings, loading weights and scores
#W <- modelo4_Ca$loading.weights
#Q <- modelo4_Ca$Yloadings
#TT <- modelo4_Ca$scores

#Q2 <- as.numeric(Q) * as.numeric(Q)
#Q2TT <- Q2[1:nc] * diag(crossprod(TT))[1:nc]
#WW <- W * W/apply(W, 2, function(x) sum(x * x))
#vip <- sqrt(length(energy) * apply(sweep(WW[, 1:nc], 2, Q2TT, "*"),
#                                   1, sum)/sum(Q2TT))
# display the variable importance
#plot(energy, vip,
#     xlab = "Energy(keV)",
#     ylab = "Importance",
#     type = "l",
#     lty = 1,
#     col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 1))
#abline(h = 1)

######################################################################################
### Verificação da normalidade dos residuos e homocedasticidade o modelo 4 para Ca ###
#######################################################################################

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###
#verificando para o numero de variaveis latentes escolhido

sf.test(modelo6_Ca$residuals[,,nc]) #função sf.test do pacote nortest 
#Resultado: p-value foi maior que 0.05 - Os residos são normais e nao precisam de transformação

#Histograma dos resíduos do modelo PLSR
dataset_pls_Cal %>%
  mutate(residuos = modelo6_Ca$residuals[,,nc]) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo6_Ca$residuals[,,nc]),
                            sd = sd(modelo6_Ca$residuals[,,nc]),
                            aes(color = "Curva Normal Teórica"),
                            size = 2)) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###  AQUI!!
predict(object = modelo6_Ca,dataset_pls_Cal) -> a
dataset_pls_Cal$Ca.pls <- a[,,nc]
modelo6_Ca_lm <- lm(formula = Ca ~ Ca.pls , data = dataset_pls_Cal)

ols_test_breusch_pagan(modelo6_Ca_lm) # Esta funcao do pacote olsrr nao funciona para PLSR

#NAO DEU CERTO - VOLTAR AQUI

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_pls_Cal$fitted_M6_Ca <- modelo6_Ca$fitted.values[,,nc] #modelo6_Ca$fitted.values[,,nc] #add fitted values pro dataset
dataset_pls_Cal$residuos_M6_Ca <- modelo6_Ca$residuals[,,nc] #add residos pro dataset



dataset_pls_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M6_Ca, y = residuos_M6_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise novo",
       y = "Resíduos do Modelo Stepwise") +
  abline(h = 0) + #nao funcionou 
  theme_bw()



### Calculo dos parametros de qualidade para conj. de CAL modelo 4 para Ca ####

#R²
a <- (sum((dataset_pls_Cal$fitted_M6_Ca - mean(dataset_pls_Cal$Ca))^2))/
  ((sum((dataset_pls_Cal$fitted_M6_Ca - mean(dataset_pls_Cal$Ca))^2)) + (sum((dataset_pls_Cal$residuos_M6_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_pls_Cal$residuos_M6_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Cal$Ca))

#RPD
d <- sd(dataset_pls_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Cal$Ca, probs = 0.75)-quantile(dataset_pls_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M6Ca_Cal <- data.frame(ID = 'M6_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 1 para Ca usando o conjunto de validação ####
predict(object = modelo6_Ca,
        dataset_pls_Val) -> f
f[,,nc] -> dataset_pls_Val$fitted_M6_Ca

dataset_pls_Val$fitted_M6_Ca - dataset_pls_Val$Ca -> dataset_pls_Val$residuos_M6_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_pls_Val$fitted_M6_Ca - mean(dataset_pls_Val$Ca))^2))/
  ((sum((dataset_pls_Val$fitted_M6_Ca - mean(dataset_pls_Val$Ca))^2)) + (sum((dataset_pls_Val$residuos_M6_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_pls_Val$residuos_M6_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Val$Ca))

#RPD
d <- sd(dataset_pls_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Val$Ca, probs = 0.75)-quantile(dataset_pls_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - validacao
Result_M6Ca_Val <- data.frame(ID = 'M6_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - Ca 
Result_M6Ca <- bind_rows(Result_M6Ca_Cal, Result_M6Ca_Val)
rm(Result_M6Ca_Cal,Result_M6Ca_Val)

#write.table(Result_M6Ca, file='Result_M6_Ca.csv',row.names = F, sep=',')

#Dataset com fitted values da Cal e Val

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "CAL") %>% cbind(dataset_pls_Cal$fitted_M6_Ca) %>% rename(Fit_M6_Ca = "dataset_pls_Cal$fitted_M6_Ca") -> Res_fit_PLS_Cal

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "VAL") %>% cbind(dataset_pls_Val$fitted_M6_Ca) %>% rename(Fit_M6_Ca = "dataset_pls_Val$fitted_M6_Ca") -> Res_fit_PLS_Val

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

G3 <- Res_fit_PLS_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M6_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - PLS (spec + text) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_PLS_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M6_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "PLS (spec + text) for Ca") +
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

########################################################################
######         Modelo 7 - PLS espectro inteiro  + visNIR #########
######                 (apos alinhar os espectros)                     #########
########################################################################
rm(dataset_pls_Cal, dataset_pls_Val)

dataset_pls_Cal <- dataset2 %>% 
  select('Ca', 'Set', "Score_PC1", "Score_PC2", "Score_PC3") %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

names(dataset_pls_Cal)

dataset_pls_Val <- dataset2 %>% 
  select('Ca', 'Set', "Score_PC1", "Score_PC2", "Score_PC3") %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 

####################################
### Estimando o modelo 7 para Ca ###
####################################

maxc <- 15

modelo7_Ca <- plsr(Ca ~ . ,
                   data = dataset_pls_Cal,
                   method = "simpls", #usar "oscorespls"  ou "simpls", o primeiro esta no livro do R
                   scale = FALSE,
                   ncomp = maxc,
                   validation = "LOO") #CV or LOO for leave one out
print(modelo7_Ca)
summary(modelo7_Ca)

#?plsr
plot(modelo7_Ca, "val",
     main = " ",
     xlab = "Number of components",
     legendpos="topright")

# number of components to use
ncomp.onesigma <- selectNcomp(modelo7_Ca, method = "onesigma", plot = TRUE,
)
nc <- ncomp.onesigma

#plotando observed vs. predicted
plot(modelo7_Ca,
     ncomp = nc,
     main = " ",
     xlab = "Observed",
     ylab = "Predicted")

#ploting the first three loadings of the principal components of the spectra (falta colocar legenda)
#plot(modelo6_Ca,
#    "loadings",
#   comps = 1:3,
#   xlab = "Index of the wavelength",
#   ylab = "Loading value")

# Standardized regression coefficient of the PLSR model
#plot(energy, modelo6_Ca$coefficients[,1,nc],
#     main = " ",
#     type = "l",
#     xlab = "Wavelength /nm",
#     ylab = "Regression coefficient")
#abline(h = 0)

# compute the variable importance, see Wold et al., (1993)
# take the loadings, loading weights and scores
#W <- modelo4_Ca$loading.weights
#Q <- modelo4_Ca$Yloadings
#TT <- modelo4_Ca$scores

#Q2 <- as.numeric(Q) * as.numeric(Q)
#Q2TT <- Q2[1:nc] * diag(crossprod(TT))[1:nc]
#WW <- W * W/apply(W, 2, function(x) sum(x * x))
#vip <- sqrt(length(energy) * apply(sweep(WW[, 1:nc], 2, Q2TT, "*"),
#                                   1, sum)/sum(Q2TT))
# display the variable importance
#plot(energy, vip,
#     xlab = "Energy(keV)",
#     ylab = "Importance",
#     type = "l",
#     lty = 1,
#     col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 1))
#abline(h = 1)

######################################################################################
### Verificação da normalidade dos residuos e homocedasticidade o modelo 7 para Ca ###
#######################################################################################

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###
#verificando para o numero de variaveis latentes escolhido

sf.test(modelo7_Ca$residuals[,,nc]) #função sf.test do pacote nortest 
#Resultado: p-value foi maior que 0.05 - Os residos são normais e nao precisam de transformação

#Histograma dos resíduos do modelo PLSR
dataset_pls_Cal %>%
  mutate(residuos = modelo7_Ca$residuals[,,nc]) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo7_Ca$residuals[,,nc]),
                            sd = sd(modelo7_Ca$residuals[,,nc]),
                            aes(color = "Curva Normal Teórica"),
                            size = 2)) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###  AQUI!!
predict(object = modelo7_Ca,dataset_pls_Cal) -> a
dataset_pls_Cal$Ca.pls <- a[,,nc]
modelo7_Ca_lm <- lm(formula = Ca ~ Ca.pls , data = dataset_pls_Cal)

ols_test_breusch_pagan(modelo7_Ca_lm) # Esta funcao do pacote olsrr nao funciona para PLSR

#NAO DEU CERTO - VOLTAR AQUI

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_pls_Cal$fitted_M7_Ca <- modelo7_Ca$fitted.values[,,nc] #modelo7_Ca$fitted.values[,,nc] #add fitted values pro dataset
dataset_pls_Cal$residuos_M7_Ca <- modelo7_Ca$residuals[,,nc] #add residos pro dataset



dataset_pls_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M7_Ca, y = residuos_M7_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise novo",
       y = "Resíduos do Modelo Stepwise") +
  abline(h = 0) + #nao funcionou 
  theme_bw()



### Calculo dos parametros de qualidade para conj. de CAL modelo 4 para Ca ####

#R²
a <- (sum((dataset_pls_Cal$fitted_M7_Ca - mean(dataset_pls_Cal$Ca))^2))/
  ((sum((dataset_pls_Cal$fitted_M7_Ca - mean(dataset_pls_Cal$Ca))^2)) + (sum((dataset_pls_Cal$residuos_M7_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_pls_Cal$residuos_M7_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Cal$Ca))

#RPD
d <- sd(dataset_pls_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Cal$Ca, probs = 0.75)-quantile(dataset_pls_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M7Ca_Cal <- data.frame(ID = 'M7_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 1 para Ca usando o conjunto de validação ####
predict(object = modelo7_Ca,
        dataset_pls_Val) -> f
f[,,nc] -> dataset_pls_Val$fitted_M7_Ca

dataset_pls_Val$fitted_M7_Ca - dataset_pls_Val$Ca -> dataset_pls_Val$residuos_M7_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_pls_Val$fitted_M7_Ca - mean(dataset_pls_Val$Ca))^2))/
  ((sum((dataset_pls_Val$fitted_M7_Ca - mean(dataset_pls_Val$Ca))^2)) + (sum((dataset_pls_Val$residuos_M7_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_pls_Val$residuos_M7_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Val$Ca))

#RPD
d <- sd(dataset_pls_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Val$Ca, probs = 0.75)-quantile(dataset_pls_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - validacao
Result_M7Ca_Val <- data.frame(ID = 'M7_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - Ca 
Result_M7Ca <- bind_rows(Result_M7Ca_Cal, Result_M7Ca_Val)
rm(Result_M7Ca_Cal,Result_M7Ca_Val)

#write.table(Result_M7Ca, file='Result_M7_Ca.csv',row.names = F, sep=',')

#Dataset com fitted values da Cal e Val

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "CAL") %>% cbind(dataset_pls_Cal$fitted_M7_Ca) %>% rename(Fit_M7_Ca = "dataset_pls_Cal$fitted_M7_Ca") -> Res_fit_PLS_Cal

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "VAL") %>% cbind(dataset_pls_Val$fitted_M7_Ca) %>% rename(Fit_M7_Ca = "dataset_pls_Val$fitted_M7_Ca") -> Res_fit_PLS_Val

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

G3 <- Res_fit_PLS_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M7_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - PLS (spec + vis-NIR) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_PLS_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M7_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "PLS (spec + vis-NIR) for Ca") +
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

########################################################################
######         Modelo 8 - PLS espectro inteiro + Text  + visNIR #########
######                 (apos alinhar os espectros)                     #########
########################################################################
rm(dataset_pls_Cal, dataset_pls_Val)

dataset_pls_Cal <- dataset2 %>% 
  select('Ca', 'Set', "Clay", "Sand", "Score_PC1", "Score_PC2", "Score_PC3") %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

#names(dataset_pls_Cal)

dataset_pls_Val <- dataset2 %>% 
  select('Ca', 'Set', "Clay", "Sand", "Score_PC1", "Score_PC2", "Score_PC3") %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 

####################################
### Estimando o modelo 8 para Ca ###
####################################

maxc <- 15

modelo8_Ca <- plsr(Ca ~ . ,
                   data = dataset_pls_Cal,
                   method = "simpls", #usar "oscorespls"  ou "simpls", o primeiro esta no livro do R
                   scale = FALSE,
                   ncomp = maxc,
                   validation = "LOO") #CV or LOO for leave one out
print(modelo8_Ca)
summary(modelo8_Ca)

#?plsr
plot(modelo8_Ca, "val",
     main = " ",
     xlab = "Number of components",
     legendpos="topright")

# number of components to use
ncomp.onesigma <- selectNcomp(modelo8_Ca, method = "onesigma", plot = TRUE,
)
nc <- ncomp.onesigma

#plotando observed vs. predicted
plot(modelo8_Ca,
     ncomp = nc,
     main = " ",
     xlab = "Observed",
     ylab = "Predicted")

#ploting the first three loadings of the principal components of the spectra (falta colocar legenda)
#plot(modelo6_Ca,
#    "loadings",
#   comps = 1:3,
#   xlab = "Index of the wavelength",
#   ylab = "Loading value")

# Standardized regression coefficient of the PLSR model
#plot(energy, modelo6_Ca$coefficients[,1,nc],
#     main = " ",
#     type = "l",
#     xlab = "Wavelength /nm",
#     ylab = "Regression coefficient")
#abline(h = 0)

# compute the variable importance, see Wold et al., (1993)
# take the loadings, loading weights and scores
#W <- modelo4_Ca$loading.weights
#Q <- modelo4_Ca$Yloadings
#TT <- modelo4_Ca$scores

#Q2 <- as.numeric(Q) * as.numeric(Q)
#Q2TT <- Q2[1:nc] * diag(crossprod(TT))[1:nc]
#WW <- W * W/apply(W, 2, function(x) sum(x * x))
#vip <- sqrt(length(energy) * apply(sweep(WW[, 1:nc], 2, Q2TT, "*"),
#                                   1, sum)/sum(Q2TT))
# display the variable importance
#plot(energy, vip,
#     xlab = "Energy(keV)",
#     ylab = "Importance",
#     type = "l",
#     lty = 1,
#     col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 1))
#abline(h = 1)

######################################################################################
### Verificação da normalidade dos residuos e homocedasticidade o modelo 4 para Ca ###
#######################################################################################

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###
#verificando para o numero de variaveis latentes escolhido

sf.test(modelo8_Ca$residuals[,,nc]) #função sf.test do pacote nortest 
#Resultado: p-value foi maior que 0.05 - Os residos são normais e nao precisam de transformação

#Histograma dos resíduos do modelo PLSR
dataset_pls_Cal %>%
  mutate(residuos = modelo8_Ca$residuals[,,nc]) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo8_Ca$residuals[,,nc]),
                            sd = sd(modelo8_Ca$residuals[,,nc]),
                            aes(color = "Curva Normal Teórica"),
                            size = 2)) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###  AQUI!!
predict(object = modelo8_Ca,dataset_pls_Cal) -> a
dataset_pls_Cal$Ca.pls <- a[,,nc]
modelo8_Ca_lm <- lm(formula = Ca ~ Ca.pls , data = dataset_pls_Cal)

ols_test_breusch_pagan(modelo8_Ca_lm) # Esta funcao do pacote olsrr nao funciona para PLSR

#NAO DEU CERTO - VOLTAR AQUI

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_pls_Cal$fitted_M8_Ca <- modelo8_Ca$fitted.values[,,nc] #modelo7_Ca$fitted.values[,,nc] #add fitted values pro dataset
dataset_pls_Cal$residuos_M8_Ca <- modelo8_Ca$residuals[,,nc] #add residos pro dataset



dataset_pls_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M8_Ca, y = residuos_M8_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise novo",
       y = "Resíduos do Modelo Stepwise") +
  abline(h = 0) + #nao funcionou 
  theme_bw()



### Calculo dos parametros de qualidade para conj. de CAL modelo 4 para Ca ####

#R²
a <- (sum((dataset_pls_Cal$fitted_M8_Ca - mean(dataset_pls_Cal$Ca))^2))/
  ((sum((dataset_pls_Cal$fitted_M8_Ca - mean(dataset_pls_Cal$Ca))^2)) + (sum((dataset_pls_Cal$residuos_M8_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_pls_Cal$residuos_M8_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Cal$Ca))

#RPD
d <- sd(dataset_pls_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Cal$Ca, probs = 0.75)-quantile(dataset_pls_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M8Ca_Cal <- data.frame(ID = 'M8_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 1 para Ca usando o conjunto de validação ####
predict(object = modelo8_Ca,
        dataset_pls_Val) -> f
f[,,nc] -> dataset_pls_Val$fitted_M8_Ca

dataset_pls_Val$fitted_M8_Ca - dataset_pls_Val$Ca -> dataset_pls_Val$residuos_M8_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_pls_Val$fitted_M8_Ca - mean(dataset_pls_Val$Ca))^2))/
  ((sum((dataset_pls_Val$fitted_M8_Ca - mean(dataset_pls_Val$Ca))^2)) + (sum((dataset_pls_Val$residuos_M8_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_pls_Val$residuos_M8_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Val$Ca))

#RPD
d <- sd(dataset_pls_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Val$Ca, probs = 0.75)-quantile(dataset_pls_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - validacao
Result_M8Ca_Val <- data.frame(ID = 'M8_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - Ca 
Result_M8Ca <- bind_rows(Result_M8Ca_Cal, Result_M8Ca_Val)
rm(Result_M8Ca_Cal,Result_M8Ca_Val)

#write.table(Result_M8Ca, file='Result_M8_Ca.csv',row.names = F, sep=',')

#Dataset com fitted values da Cal e Val

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "CAL") %>% cbind(dataset_pls_Cal$fitted_M8_Ca) %>% rename(Fit_M8_Ca = "dataset_pls_Cal$fitted_M8_Ca") -> Res_fit_PLS_Cal

dataset %>% select("ID","Field","Set","Ca") %>% 
  filter(Set == "VAL") %>% cbind(dataset_pls_Val$fitted_M8_Ca) %>% rename(Fit_M8_Ca = "dataset_pls_Val$fitted_M8_Ca") -> Res_fit_PLS_Val

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

G3 <- Res_fit_PLS_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M8_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - PLS (spec + text + vis-NIR) for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 13, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position=c(0.85, 0.23), legend.text = element_text(size = 11), legend.title = element_text(size = 11))

G4 <- Res_fit_PLS_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M8_Ca, color = Field),
             size = 2,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "PLS (spec + text + vis-NIR) for Ca") +
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
