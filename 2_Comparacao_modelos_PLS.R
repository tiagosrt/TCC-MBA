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
str(dataset)
names(dataset)

#janitor::clean_names(dataset)-> dataset2 #funcao para limpar nome de variaveis

#definindo a coluna ID como row names
row.names(dataset) <- dataset$ID

#deixando apenas dois digitos apos a virgula para variavies espectrais
colunas_spectra <- as.character(round(as.numeric(names(dataset[,c(32:1489)])), digits = 2))
spectra_data <- dataset[,c(32:1489)]

spectra_data <- rename_with(spectra_data, .cols = everything(), 
                       .fn = str_replace, pattern = ".*", replacement = colunas_spectra)

#limpando colunas que nao seram necessarias, juntando com spectra data e definindo set de cal e val
names(dataset)

dataset_pls_Cal <- dataset %>% 
  select('Ca', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

names(dataset_pls_Cal)

dataset_pls_Val <- dataset %>% 
  select('Ca', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 

#write.table(datasetCal, file='datasetCal_spectra.csv',row.names = F, sep=',') 

#definindo o espectro
energy <- names(dataset[ ,c(2:1459)])

#Uma forma de plotar os espectros
matplot(x = energy, y = t(dataset[ ,c(6:1463)]),
        xlab = "Energy (keV)",
        ylab = "Intensity (cps)",
        type = "l",
        lty = 1,
        col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 0.3))

########################################################################
######         Modelo 4 - PLS com espectro inteiro         #########
######                 (apos alinhar os espectros)         #########
########################################################################

####################################
### Estimando o modelo 4 para Ca ###
####################################

maxc <- 15

modelo4_Ca <- plsr(Ca ~ . ,
                      data = dataset_pls_Cal,
                      method = "simpls", #usar "oscorespls"  ou "simpls", o primeiro esta no livro do R
                      scale = FALSE,
                      ncomp = maxc,
                      validation = "LOO") #CV or LOO for leave one out
print(modelo4_Ca)
summary(modelo4_Ca)

?plsr
plot(modelo4_Ca, "val",
     main = " ",
     xlab = "Number of components",
     legendpos="topright")

# number of components to use
ncomp.onesigma <- selectNcomp(modelo4_Ca, method = "onesigma", plot = TRUE,
                              )
nc <- ncomp.onesigma

#plotando observed vs. predicted
plot(modelo4_Ca,
     ncomp = nc,
     main = " ",
     xlab = "Observed",
     ylab = "Predicted")

#ploting the first three loadings of the principal components of the spectra (falta colocar legenda)
plot(modelo4_Ca,
     "loadings",
     comps = 1:3,
     xlab = "Index of the wavelength",
     ylab = "Loading value")

# Standardized regression coefficient of the PLSR model
plot(energy, modelo4_Ca$coefficients[,1,nc],
     main = " ",
     type = "l",
     xlab = "Wavelength /nm",
     ylab = "Regression coefficient")
abline(h = 0)

# compute the variable importance, see Wold et al., (1993)
# take the loadings, loading weights and scores
W <- modelo4_Ca$loading.weights
Q <- modelo4_Ca$Yloadings
TT <- modelo4_Ca$scores

Q2 <- as.numeric(Q) * as.numeric(Q)
Q2TT <- Q2[1:nc] * diag(crossprod(TT))[1:nc]
WW <- W * W/apply(W, 2, function(x) sum(x * x))
vip <- sqrt(length(energy) * apply(sweep(WW[, 1:nc], 2, Q2TT, "*"),
                                 1, sum)/sum(Q2TT))
# display the variable importance
plot(energy, vip,
     xlab = "Energy(keV)",
     ylab = "Importance",
     type = "l",
     lty = 1,
     col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 1))
abline(h = 1)

######################################################################################
### Verificação da normalidade dos residuos e homocedasticidade o modelo 4 para Ca ###
#######################################################################################

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###
#verificando para o numero de variaveis latentes escolhido

sf.test(modelo4_Ca$residuals[,,nc]) #função sf.test do pacote nortest 
#Resultado: p-value foi maior que 0.05 - Os residos são normais e nao precisam de transformação

#Histograma dos resíduos do modelo PLSR
dataset_pls_Cal %>%
  mutate(residuos = modelo4_Ca$residuals[,,nc]) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo4_Ca$residuals[,,nc]),
                            sd = sd(modelo4_Ca$residuals[,,nc]),
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
ols_test_breusch_pagan(modelo4_Ca) # Esta funcao do pacote olsrr nao funciona para PLSR

library(lmtest)
dataset_pls_Cal %>% bptest(Ca ~ .)
?bptest
#NAO DEU CERTO - VOLTAR AQUI

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_pls_Cal$fitted_M4_Ca <- modelo4_Ca$fitted.values[,,nc] #add fitted values pro dataset
dataset_pls_Cal$residuos_M4_Ca <- modelo4_Ca$residuals[,,nc] #add residos pro dataset



dataset_pls_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M4_Ca, y = residuos_M4_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  abline(h = 0) + #nao funcionou 
  theme_bw()



### Calculo dos parametros de qualidade para conj. de CAL modelo 4 para Ca ####

#R²
a <- (sum((dataset_pls_Cal$fitted_M4_Ca - mean(dataset_pls_Cal$Ca))^2))/
  ((sum((dataset_pls_Cal$fitted_M4_Ca - mean(dataset_pls_Cal$Ca))^2)) + (sum((dataset_pls_Cal$residuos_M4_Ca)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_pls_Cal$residuos_M4_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Cal$Ca))

#RPD
d <- sd(dataset_pls_Cal$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Cal$Ca, probs = 0.75)-quantile(dataset_pls_Cal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M4Ca_Cal <- data.frame(ID = 'M4_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 1 para Ca usando o conjunto de validação ####
predict(object = modelo4_Ca,
        dataset_pls_Val) -> f
  f[,,nc] -> dataset_pls_Val$fitted_M4_Ca

dataset_pls_Val$fitted_M4_Ca - dataset_pls_Val$Ca -> dataset_pls_Val$residuos_M4_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((dataset_pls_Val$fitted_M4_Ca - mean(dataset_pls_Val$Ca))^2))/
  ((sum((dataset_pls_Val$fitted_M4_Ca - mean(dataset_pls_Val$Ca))^2)) + (sum((dataset_pls_Val$residuos_M4_Ca)^2)))

#RMSE
b <- mean(sqrt((dataset_pls_Val$residuos_M4_Ca)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Val$Ca))

#RPD
d <- sd(dataset_pls_Val$Ca)/b

#RPIQ
e <- (quantile(dataset_pls_Val$Ca, probs = 0.75)-quantile(dataset_pls_Val$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - validacao
Result_M4Ca_Val <- data.frame(ID = 'M4_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - Ca 
Result_M4Ca <- bind_rows(Result_M4Ca_Cal, Result_M4Ca_Val)
rm(Result_M4Ca_Cal,Result_M4Ca_Val)

#Dataset com fitted values da Cal e Val

dataset %>% select("ID","Field","Set","Ca","K") %>% 
  filter(Set == "CAL") %>% cbind(dataset_pls_Cal$fitted_M4_Ca) %>% rename(Fit_M4_Ca = "dataset_pls_Cal$fitted_M4_Ca") -> Res_fit_PLS_Cal
  
dataset %>% select("ID","Field","Set","Ca","K") %>% 
  filter(Set == "VAL") %>% cbind(dataset_pls_Val$fitted_M4_Ca) %>% rename(Fit_M4_Ca = "dataset_pls_Val$fitted_M4_Ca") -> Res_fit_PLS_Val


####################################
### Estimando o modelo 4 para K ###
####################################

#organizando datasets
names(dataset)

dataset_pls_Cal <- dataset %>% 
  select('K', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'CAL') %>%
  select(-'Set') 

names(dataset_pls_Cal)

dataset_pls_Val <- dataset %>% 
  select('K', 'Set') %>% 
  cbind(spectra_data) %>% 
  filter(Set == 'VAL') %>%
  select(-'Set') 

#calibrando o PLSR

maxc <- 15

modelo4_K <- plsr(K ~ . ,
                   data = dataset_pls_Cal,
                   method = "simpls", #usar "oscorespls"  ou "simpls", o primeiro esta no livro do R
                   scale = FALSE,
                   ncomp = maxc,
                   validation = "LOO") #CV or LOO for leave one out
print(modelo4_K)
summary(modelo4_K)

?plsr
plot(modelo4_K, "val",
     main = " ",
     xlab = "Number of components",
     legendpos="topright")

# number of components to use
ncomp.onesigma <- selectNcomp(modelo4_K, method = "onesigma", plot = TRUE,
)
nc <- ncomp.onesigma

#plotando observed vs. predicted
plot(modelo4_K,
     ncomp = nc,
     main = " ",
     xlab = "Observed",
     ylab = "Predicted")

#ploting the first three loadings of the principal components of the spectra (falta colocar legenda)
plot(modelo4_K,
     "loadings",
     comps = 1:3,
     xlab = "Index of the wavelength",
     ylab = "Loading value")

# Standardized regression coefficient of the PLSR model
energy <- names(dataset[ ,c(2:1459)])
plot(energy, modelo4_K$coefficients[,1,nc],
     main = " ",
     type = "l",
     xlab = "Wavelength /nm",
     ylab = "Regression coefficient")
abline(h = 0)

# compute the variable importance, see Wold et al., (1993)
# take the loadings, loading weights and scores
W <- modelo4_K$loading.weights
Q <- modelo4_K$Yloadings
TT <- modelo4_K$scores

Q2 <- as.numeric(Q) * as.numeric(Q)
Q2TT <- Q2[1:nc] * diag(crossprod(TT))[1:nc]
WW <- W * W/apply(W, 2, function(x) sum(x * x))
vip <- sqrt(length(energy) * apply(sweep(WW[, 1:nc], 2, Q2TT, "*"),
                                   1, sum)/sum(Q2TT))
# display the variable importance
plot(energy, vip,
     xlab = "Energy(keV)",
     ylab = "Importance",
     type = "l",
     lty = 1,
     col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 1))
abline(h = 1)

######################################################################################
### Verificação da normalidade dos residuos e homocedasticidade o modelo 4 para K ###
#######################################################################################

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###
#verificando para o numero de variaveis latentes escolhido

sf.test(modelo4_K$residuals[,,nc]) #função sf.test do pacote nortest 
#Resultado: p-value foi maior que 0.05 - Os residos são normais e nao precisam de transformação

#Histograma dos resíduos do modelo PLSR
dataset_pls_Cal %>%
  mutate(residuos = modelo4_K$residuals[,,nc]) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo4_K$residuals[,,nc]),
                            sd = sd(modelo4_K$residuals[,,nc]),
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
ols_test_breusch_pagan(modelo4_K) # Esta funcao do pacote olsrr nao funciona para PLSR

library(lmtest)
dataset_pls_Cal %>% bptest(Ca ~ .)
?bptest
#NAO DEU CERTO - VOLTAR AQUI

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
dataset_pls_Cal$fitted_M4_K <- modelo4_K$fitted.values[,,nc] #add fitted values pro dataset
dataset_pls_Cal$residuos_M4_K <- modelo4_K$residuals[,,nc] #add residos pro dataset



dataset_pls_Cal %>%
  ggplot() +
  geom_point(aes(x = fitted_M4_K, y = residuos_M4_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  abline(h = 0) + #nao funcionou 
  theme_bw()



### Calculo dos parametros de qualidade para conj. de CAL modelo 4 para Ca ####

#R²
a <- (sum((dataset_pls_Cal$fitted_M4_K - mean(dataset_pls_Cal$K))^2))/
  ((sum((dataset_pls_Cal$fitted_M4_K - mean(dataset_pls_Cal$K))^2)) + (sum((dataset_pls_Cal$residuos_M4_K)^2)))
#round(a, digits = 4)

#RMSE
b <- mean(sqrt((dataset_pls_Cal$residuos_M4_K)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Cal$K))

#RPD
d <- sd(dataset_pls_Cal$K)/b

#RPIQ
e <- (quantile(dataset_pls_Cal$K, probs = 0.75)-quantile(dataset_pls_Cal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 4 - Ca - Calibracao
Result_M4K_Cal <- data.frame(ID = 'M4_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 4 para Ca usando o conjunto de validação ####
predict(object = modelo4_K,
        dataset_pls_Val) -> f
f[,,nc] -> dataset_pls_Val$fitted_M4_K

dataset_pls_Val$fitted_M4_K - dataset_pls_Val$K -> dataset_pls_Val$residuos_M4_K

### Calculo dos parametros de qualidade para conj. de VAL modelo 4 para Ca ####

#R²
a <- (sum((dataset_pls_Val$fitted_M4_K - mean(dataset_pls_Val$K))^2))/
  ((sum((dataset_pls_Val$fitted_M4_K - mean(dataset_pls_Val$K))^2)) + (sum((dataset_pls_Val$residuos_M4_K)^2)))

#RMSE
b <- mean(sqrt((dataset_pls_Val$residuos_M4_K)^2))

#RMSE%
c <- b*(100/mean(dataset_pls_Val$K))

#RPD
d <- sd(dataset_pls_Val$K)/b

#RPIQ
e <- (quantile(dataset_pls_Val$K, probs = 0.75)-quantile(dataset_pls_Val$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 4 - Ca - validacao
Result_M4K_Val <- data.frame(ID = 'M4_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 4 - Ca 
Result_M4K <- bind_rows(Result_M4K_Cal, Result_M4K_Val)
rm(Result_M4K_Cal,Result_M4K_Val)

#Resumo geral - Resultados Modelo 4 
Result_M4 <- bind_rows(Result_M4K, Result_M4Ca)
rm(Result_M4K,Result_M4Ca)

#write.table(Result_M4, file='Result_M4.csv',row.names = F, sep=',')

#Dataset com fitted values da Cal e Val
Res_fit_PLS_Cal %>% cbind(dataset_pls_Cal$fitted_M4_K) %>% 
  rename(Fit_M4_K = "dataset_pls_Cal$fitted_M4_K") -> Res_fit_PLS_Cal

Res_fit_PLS_Val %>% cbind(dataset_pls_Val$fitted_M4_K) %>% 
  rename(Fit_M4_K = "dataset_pls_Val$fitted_M4_K") -> Res_fit_PLS_Val

#write.table(Res_fit_PLS_Cal, file='Res_fit_PLS_Cal.csv',row.names = F, sep=',')
#write.table(Res_fit_PLS_Val, file='Res_fit_PLS_Val.csv',row.names = F, sep=',')

#### Plotando graficos de dispersao ####

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

 G1 <- Res_fit_PLS_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M4_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
       labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration - PLS for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))
#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

 G2 <- Res_fit_PLS_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = Fit_M4_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Validation - PLS for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))
        
### Grafico de pontos para K
 
 G3 <- Res_fit_PLS_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M4_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration - PLS for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

 G4 <- Res_fit_PLS_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = Fit_M4_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Validation - PLS for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

 grid.arrange(G1 , G2 , #do pacote gridExtra
              G3 , G4 ,
              ncol=2, nrow=2)
 
 