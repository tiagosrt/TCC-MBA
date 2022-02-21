##################################################################################
#                 PACOTES NECESSÁRIOS PARA REGRESSAO SIMPLES E MULTIPLA         #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggpubr","gapminder", "gridExtra", "ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "readxl","rqPen","ggrepel", "rayshader","psych","pracma",
             "polynom")

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
#removendo variaveis que nao vou usar
dataset$Mg <- NULL
dataset$P <- NULL
str(dataset)
names(dataset) #para ver a posição das variaveis

#simplificando nome das variaveis
dataset <- dataset %>% rename(AlK = 9,
                              SiK = 10,
                              KK = 11,
                              CaK = 12,
                              TiK = 13,
                              MnK = 14,
                              FeK = 15,
                              NiK = 16,
                              CuK = 17,
                              RhL = 18,
                              RhKT = 19,
                              RhKC = 20,
                              AlKN = 21,
                              SiKN = 22,
                              KKN = 23,
                              CaKN = 24,
                              TiKN = 25,
                              MnKN = 26,
                              FeKN = 27,
                              NiKN = 28,
                              CuKN = 29
)


names(dataset)

#Separando dataset de Calibracao e Validacao
datasetCal <- dataset %>% filter(Set == 'CAL')
datasetVal <- dataset %>% filter(Set == 'VAL')

#str(dataset) # Mostra a estrutura da base de dados
#glimpse(dataset) # Função parecida com a str
#print(dataset) # Apresenta a base de dados no console
#dim(dataset) # As dimensões do dataset: linhas e colunas, respectivamente
#names(dataset) # Para ver os nomes das variáveis
#unique(dataset$Class)# Para ver os valores das variáveis
#typeof(dataset$Class)# Para vero tipo das variáveis

########################################################################
###### Observando os teores de Ca e K e sua relação com as    #########
######         demais variáveis                               #########
########################################################################
summary(dataset$K)
summary(dataset$Ca)

#plot the value of Ca and K contents for both calibration and validation
par(mfrow=c(2,1))
# calibration 
hist(datasetCal$Ca,
     main = "",
     xlab = "Ca - Calibration Set") 
# validation
hist(datasetVal$Ca,
     main = "",
     xlab = "Ca - Validation Set") 
# calibration
hist(datasetCal$K,
     main = "",
     xlab = "K - Calibration Set") 
# validation
hist(datasetVal$K,
     main = "",
     xlab = "K - Validation Set")

## boxplot para Ca e K nos conjuntos de Cal e Val

G1 <- dataset %>% 
  ggplot(aes(x=Set, y = Ca, fill=Set)) +
  geom_boxplot()+ xlab(" ") + ylab("Ca (mg/kg)")+ 
  theme(legend.position="none") + geom_jitter(width=0.25,alpha=0.3) 

G2 <- dataset %>% 
  ggplot(aes(x=Set, y = K, fill=Set)) +
  geom_boxplot() + xlab(" ") + ylab("K (mg/kg)")+
  theme(legend.position="none") + geom_jitter(width=0.25,alpha=0.3)

#grid.arrange(G1 , G2 , #do pacote gridExtra
             ncol=2, nrow=1)

# boxplot para Ca e K nos Field 1 e 2
G3 <- dataset %>% 
  ggplot(aes(x=Field, y = Ca, fill=Field)) +
  geom_boxplot()+ xlab(" ") + ylab("Ca (mg/kg)")+ 
  theme(legend.position="none") + geom_jitter(width=0.25,alpha=0.3) 

G4 <- dataset %>% 
  ggplot(aes(x=Field, y = K, fill=Field)) +
  geom_boxplot() + xlab(" ") + ylab("K (mg/kg)")+
  theme(legend.position="none") + geom_jitter(width=0.25,alpha=0.3)

grid.arrange(G1 , G2 , G3, G4, #do pacote gridExtra
             ncol=4, nrow=1)

# Observando as correlações entre variáveis
chart.Correlation(dataset[,5:20])  #para todas as observações
chart.Correlation(dataset[dataset$Set == "CAL",5:20]) #para as observações do set de Cal
chart.Correlation(dataset[dataset$Set == "VAL",5:20]) #para as observações do set de Val
chart.Correlation(dataset[dataset$Field == "A",5:20]) #para as observações da área A
chart.Correlation(dataset[dataset$Field == "B",5:20]) #para as observações da área B


########################################################################
######         Modelo 1 - Reg. Linear Simples com linha     #########
######               de emissão do elemento                 #########
########################################################################

####################################
### Estimando o modelo 1 para Ca ###
####################################
modelo1_Ca <- lm(formula = Ca ~ CaK,
                       data = datasetCal)

summary(modelo1_Ca)
summ(modelo1_Ca, confint = T, digits = 4, ci.width = .95)
export_summs(modelo1_Ca, scale = F, digits = 4)

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###

sf.test(modelo1_Ca$residuals) #função sf.test do pacote nortest
#Resultado: p-value foi maior que 0.05 - Os residos são normais e nao precisam de transformação

#Histograma dos resíduos do modelo OLS linear
datasetCal %>%
  mutate(residuos = modelo1_Ca$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo1_Ca$residuals),
                            sd = sd(modelo1_Ca$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###
ols_test_breusch_pagan(modelo1_Ca) # do pacote olsrr

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
datasetCal$fitted_M1_Ca <- modelo1_Ca$fitted.values #add fitted values pro dataset
datasetCal$residuos_M1_Ca <- modelo1_Ca$residuals #add residos pro dataset



datasetCal %>%
  ggplot() +
  geom_point(aes(x = fitted_M1_Ca, y = residuos_M1_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()



### Calculo dos parametros de qualidade para conj. de CAL modelo 1 para Ca ####

#R²
a <- (sum((datasetCal$fitted_M1_Ca - mean(datasetCal$Ca))^2))/
  ((sum((datasetCal$fitted_M1_Ca - mean(datasetCal$Ca))^2)) + (sum((datasetCal$residuos_M1_Ca)^2)))
#round(R2_M1Ca_Cal, digits = 4)

#RMSE
b <- mean(sqrt((datasetCal$residuos_M1_Ca)^2))

#RMSE%
c <- b*(100/mean(datasetCal$Ca))

#RPD
d <- sd(datasetCal$Ca)/b

#RPIQ
e <- (quantile(datasetCal$Ca, probs = 0.75)-quantile(datasetCal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M1Ca_Cal <- data.frame(ID = 'M1_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 1 para Ca usando o conjunto de validação ####
predict(object = modelo1_Ca,
        datasetVal) -> datasetVal$fitted_M1_Ca

datasetVal$fitted_M1_Ca - datasetVal$Ca -> datasetVal$residuos_M1_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((datasetVal$fitted_M1_Ca - mean(datasetVal$Ca))^2))/
  ((sum((datasetVal$fitted_M1_Ca - mean(datasetVal$Ca))^2)) + (sum((datasetVal$residuos_M1_Ca)^2)))

#RMSE
b <- mean(sqrt((datasetVal$residuos_M1_Ca)^2))

#RMSE%
c <- b*(100/mean(datasetVal$Ca))

#RPD
d <- sd(datasetVal$Ca)/b

#RPIQ
e <- (quantile(datasetVal$Ca, probs = 0.75)-quantile(datasetVal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - validacao
Result_M1Ca_Val <- data.frame(ID = 'M1_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - Ca 
Result_M1Ca <- bind_rows(Result_M1Ca_Cal, Result_M1Ca_Val)
rm(Result_M1Ca_Cal,Result_M1Ca_Val)

####################################
### Estimando o modelo 1 para K ###
####################################
str(datasetCal)

modelo1_K <- lm(formula = K ~ KK,
                 data = datasetCal)

summary(modelo1_K)
summ(modelo1_K, confint = T, digits = 4, ci.width = .95)
export_summs(modelo1_K, scale = F, digits = 4)

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###

sf.test(modelo1_K$residuals) #função sf.test do pacote nortest
#Resultado: p-value foi menor que 0.05 - Os residos NÃO são normais e nao precisam de transformação

FAZER TRANSFORMACAO BOXCOX

#Histograma dos resíduos do modelo OLS linear
datasetCal %>%
  mutate(residuos = modelo1_K$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo1_K$residuals),
                            sd = sd(modelo1_K$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###
ols_test_breusch_pagan(modelo1_K) # do pacote olsrr

#Resultado: p-value foi maior que 0.05, indicando que o modelo é homocedastico, ou seja, a variancia dos residuos 
# é constante. Nã existe correlação entre os residuos (termos de erro) e a variavel X, bem como
# não há omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
datasetCal$fitted_M1_K <- modelo1_K$fitted.values #add fitted values pro dataset
datasetCal$residuos_M1_K <- modelo1_K$residuals #add residos pro dataset


datasetCal %>%
  ggplot() +
  geom_point(aes(x = fitted_M1_K, y = residuos_M1_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 1 para K ####

#R²
a <- (sum((datasetCal$fitted_M1_K - mean(datasetCal$K))^2))/
  ((sum((datasetCal$fitted_M1_K - mean(datasetCal$K))^2)) + (sum((datasetCal$residuos_M1_K)^2)))
#round(R2_M1Ca_Cal, digits = 4)

#RMSE
b <- mean(sqrt((datasetCal$residuos_M1_K)^2))

#RMSE%
c <- b*(100/mean(datasetCal$K))

#RPD
d <- sd(datasetCal$K)/b

#RPIQ
e <- (quantile(datasetCal$K, probs = 0.75)-quantile(datasetCal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - Ca - Calibracao
Result_M1K_Cal <- data.frame(ID = 'M1_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 1 para Ca usando o conjunto de validação ####
predict(object = modelo1_K,
        datasetVal) -> datasetVal$fitted_M1_K

datasetVal$fitted_M1_K - datasetVal$K -> datasetVal$residuos_M1_K

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para K ####

#R²
a <- (sum((datasetVal$fitted_M1_K - mean(datasetVal$K))^2))/
  ((sum((datasetVal$fitted_M1_K - mean(datasetVal$K))^2)) + (sum((datasetVal$residuos_M1_K)^2)))

#RMSE
b <- mean(sqrt((datasetVal$residuos_M1_K)^2))

#RMSE%
c <- b*(100/mean(datasetVal$K))

#RPD
d <- sd(datasetVal$K)/b

#RPIQ
e <- (quantile(datasetVal$K, probs = 0.75)-quantile(datasetVal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 1 - K - Validacao
Result_M1K_Val <- data.frame(ID = 'M1_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 1 - K
Result_M1K <- bind_rows(Result_M1K_Cal, Result_M1K_Val)
rm(Result_M1K_Cal,Result_M1K_Val)

####Resumo geral - Resultados Modelo 1 ####
Result_M1 <- bind_rows(Result_M1K, Result_M1Ca)
rm(Result_M1K,Result_M1Ca)
#write.table(Result_M1, file='Result_M1.csv',row.names = F, sep=',')

####Dataset com fitted values da Cal e Val####
Res_fit_M1_Cal <- datasetCal %>% 
  select("ID","Field","Set","Ca","K","fitted_M1_Ca","residuos_M1_Ca","fitted_M1_K", "residuos_M1_K") 
    

Res_fit_M1_Val <- datasetVal %>% 
  select("ID","Field","Set","Ca","K","fitted_M1_Ca","residuos_M1_Ca","fitted_M1_K", "residuos_M1_K")

#write.table(Res_fit_M1_Cal, file='Res_fit_M1_Cal.csv',row.names = F, sep=',')
#write.table(Res_fit_M1_Val, file='Res_fit_M1_Val.csv',row.names = F, sep=',')

sd(datasetVal$Ca)
#### Plotando graficos de dispersao ####
### Grafico de pontos para Ca
names(Res_fit_M1_Cal)

G1 <- Res_fit_M1_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = fitted_M1_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration using RS1 for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))
#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

G2 <- Res_fit_M1_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = fitted_M1_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Validation using RS1 for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

### Grafico de pontos para K

G3 <- Res_fit_M1_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = fitted_M1_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration using RS1 for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

G4 <- Res_fit_M1_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = fitted_M1_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Validation using RS1 for K") +
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

########################################################################
######         Modelo 2 - Reg. Linear Simples com linha     #########
######               de emissão normalizada                 #########
########################################################################

####################################
### Estimando o modelo 2 para Ca ###
####################################
modelo2_Ca <- lm(formula = Ca ~ CaKN,
                 data = datasetCal)

summary(modelo2_Ca)
sum(modelo2_Ca, confint = T, digits = 4, ci.width = .95)
export_summs(modelo2_Ca, scale = F, digits = 4)

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###

sf.test(modelo2_Ca$residuals) #função sf.test do pacote nortest
#Resultado: p-value foi menor que 0.05 - Os residos NÂO são normais e nao precisam de transformação

FAZER TRANSFORMACAO BOXCOX

#Histograma dos resíduos do modelo OLS linear
datasetCal %>%
  mutate(residuos = modelo2_Ca$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo2_Ca$residuals),
                            sd = sd(modelo2_Ca$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###
ols_test_breusch_pagan(modelo2_Ca) # do pacote olsrr

#Resultado: p-value foi menor que 0.05, indicando que o modelo é heterocedastico, ou seja, a variancia dos residuos 
#não é constante. Existe correlação entre os residuos (termos de erro) e a variavel X.
#Indica também a omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
datasetCal$fitted_M2_Ca <- modelo2_Ca$fitted.values #add fitted values pro dataset
datasetCal$residuos_M2_Ca <- modelo2_Ca$residuals #add residos pro dataset



datasetCal %>%
  ggplot() +
  geom_point(aes(x = fitted_M2_Ca, y = residuos_M2_Ca),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 2 para Ca ####

#R²
a <- (sum((datasetCal$fitted_M2_Ca - mean(datasetCal$Ca))^2))/
  ((sum((datasetCal$fitted_M2_Ca - mean(datasetCal$Ca))^2)) + (sum((datasetCal$residuos_M2_Ca)^2)))
#round(R2_M1Ca_Cal, digits = 4)

#RMSE
b <- mean(sqrt((datasetCal$residuos_M2_Ca)^2))

#RMSE%
c <- b*(100/mean(datasetCal$Ca))

#RPD
d <- sd(datasetCal$Ca)/b

#RPIQ
e <- (quantile(datasetCal$Ca, probs = 0.75)-quantile(datasetCal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 2 - Ca - Calibracao
Result_M2Ca_Cal <- data.frame(ID = 'M2_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 2 para Ca usando o conjunto de validação ####
predict(object = modelo2_Ca,
        datasetVal) -> datasetVal$fitted_M2_Ca

datasetVal$fitted_M2_Ca - datasetVal$Ca -> datasetVal$residuos_M2_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 2 para Ca ####

#R²
a <- (sum((datasetVal$fitted_M2_Ca - mean(datasetVal$Ca))^2))/
  ((sum((datasetVal$fitted_M2_Ca - mean(datasetVal$Ca))^2)) + (sum((datasetVal$residuos_M2_Ca)^2)))

#RMSE
b <- mean(sqrt((datasetVal$residuos_M2_Ca)^2))

#RMSE%
c <- b*(100/mean(datasetVal$Ca))

#RPD
d <- sd(datasetVal$Ca)/b

#RPIQ
e <- (quantile(datasetVal$Ca, probs = 0.75)-quantile(datasetVal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 2 - Ca - validacao
Result_M2Ca_Val <- data.frame(ID = 'M2_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 2 - Ca 
Result_M2Ca <- bind_rows(Result_M2Ca_Cal, Result_M2Ca_Val)
rm(Result_M2Ca_Cal,Result_M2Ca_Val)

####################################
### Estimando o modelo 2 para K ###
####################################
str(datasetCal)

modelo2_K <- lm(formula = K ~ KKN,
                data = datasetCal)

summary(modelo2_K)
summ(modelo2_K, confint = T, digits = 4, ci.width = .95)
export_summs(modelo2_K, scale = F, digits = 4)

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###

sf.test(modelo2_K$residuals) #função sf.test do pacote nortest
#Resultado: p-value foi menor que 0.05 - Os residos NÃO são normais e nao precisam de transformação

FAZER TRANSFORMACAO BOXCOX

#Histograma dos resíduos do modelo OLS linear
datasetCal %>%
  mutate(residuos = modelo2_K$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo2_K$residuals),
                            sd = sd(modelo2_K$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###
ols_test_breusch_pagan(modelo2_K) # do pacote olsrr

#Resultado: p-value foi maior que 0.05, indicando que o modelo é homocedastico, ou seja, a variancia dos residuos 
# é constante. Nã existe correlação entre os residuos (termos de erro) e a variavel X, bem como
# não há omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
datasetCal$fitted_M2_K <- modelo2_K$fitted.values #add fitted values pro dataset
datasetCal$residuos_M2_K <- modelo2_K$residuals #add residos pro dataset


datasetCal %>%
  ggplot() +
  geom_point(aes(x = fitted_M2_K, y = residuos_M2_K),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 2 para K ####

#R²
a <- (sum((datasetCal$fitted_M2_K - mean(datasetCal$K))^2))/
  ((sum((datasetCal$fitted_M2_K - mean(datasetCal$K))^2)) + (sum((datasetCal$residuos_M2_K)^2)))
#round(R2_M1Ca_Cal, digits = 4)

#RMSE
b <- mean(sqrt((datasetCal$residuos_M2_K)^2))

#RMSE%
c <- b*(100/mean(datasetCal$K))

#RPD
d <- sd(datasetCal$K)/b

#RPIQ
e <- (quantile(datasetCal$K, probs = 0.75)-quantile(datasetCal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 2 - Ca - Calibracao
Result_M2K_Cal <- data.frame(ID = 'M2_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 2 para Ca usando o conjunto de validação ####
predict(object = modelo2_K,
        datasetVal) -> datasetVal$fitted_M2_K

datasetVal$fitted_M2_K - datasetVal$K -> datasetVal$residuos_M2_K

### Calculo dos parametros de qualidade para conj. de VAL modelo 2 para K ####

#R²
a <- (sum((datasetVal$fitted_M2_K - mean(datasetVal$K))^2))/
  ((sum((datasetVal$fitted_M2_K - mean(datasetVal$K))^2)) + (sum((datasetVal$residuos_M2_K)^2)))

#RMSE
b <- mean(sqrt((datasetVal$residuos_M2_K)^2))

#RMSE%
c <- b*(100/mean(datasetVal$K))

#RPD
d <- sd(datasetVal$K)/b

#RPIQ
e <- (quantile(datasetVal$K, probs = 0.75)-quantile(datasetVal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 2 - K - Validacao
Result_M2K_Val <- data.frame(ID = 'M2_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 2 - K
Result_M2K <- bind_rows(Result_M2K_Cal, Result_M2K_Val)
rm(Result_M2K_Cal,Result_M2K_Val)

####Resumo geral - Resultados Modelo 2 ####
Result_M2 <- bind_rows(Result_M2K, Result_M2Ca)
rm(Result_M2K,Result_M2Ca)
#write.table(Result_M2, file='Result_M2.csv',row.names = F, sep=',')

####Dataset com fitted values da Cal e Val####
Res_fit_M2_Cal <- datasetCal %>% 
  select("ID","Field","Set","Ca","K","fitted_M2_Ca","residuos_M2_Ca","fitted_M2_K", "residuos_M2_K") 


Res_fit_M2_Val <- datasetVal %>% 
  select("ID","Field","Set","Ca","K","fitted_M2_Ca","residuos_M2_Ca","fitted_M2_K", "residuos_M2_K")

#write.table(Res_fit_M1_Cal, file='Res_fit_M1_Cal.csv',row.names = F, sep=',')
#write.table(Res_fit_M1_Val, file='Res_fit_M1_Val.csv',row.names = F, sep=',')

#### Plotando graficos de dispersao ####
### Grafico de pontos para Ca
names(Res_fit_M2_Cal)

G1 <- Res_fit_M2_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = fitted_M2_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration using RS2 for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))
#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

G2 <- Res_fit_M2_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = fitted_M2_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Validation using RS2 for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

### Grafico de pontos para K

G3 <- Res_fit_M2_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = fitted_M2_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration using RS2 for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

G4 <- Res_fit_M2_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = fitted_M2_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Validation using RS2 for K") +
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



########################################################################
######         Modelo 3 - Reg. Linear multipla com CaK +     #########
######   Pico Compton e Elementos de matriz (Al,Si,Fe e Ti)  #########
########################################################################

####################################
### Estimando o modelo 3 para Ca ###
####################################
str(datasetCal)

modelo3_Ca <- lm(formula = Ca ~ CaK + AlK + SiK + FeK + TiK + RhKC,
                 data = datasetCal)
summary(modelo3_Ca)
sum(modelo3_Ca, confint = T, digits = 4, ci.width = .95)
export_summs(modelo3_Ca, scale = F, digits = 4)

## Aplicando o procedimento Stepwise, temos o seguinte código:##
modelo3_Ca <- step(modelo3_Ca, k = 3.841459)

summary(modelo3_Ca)

#Foram mantidas apenas as variáveis Fe e Ca, nao usou nem a variável Compton (e se normalizarmos e usar MLR?)

export_summs(modelo3_Ca, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(modelo3_Ca, level = 0.95) # siginificância 5%
plot_summs(modelo3_Ca, colors = "#440154FF") #função plot_summs do pacote ggstance

#Parâmetros padronizados
plot_summs(modelo3_Ca, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(modelo3_Ca, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
#plot_summs(modelo_empresas, step_empresas, scale = TRUE, plot.distributions = TRUE,
#           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###

sf.test(modelo3_Ca$residuals) #função sf.test do pacote nortest
#Resultado: p-value foi menor que 0.05 - Os residos NÂO são normais e nao precisam de transformação

FAZER TRANSFORMACAO BOXCOX e RODAR STEP DE NOVO

#Histograma dos resíduos do modelo OLS linear
datasetCal %>%
  mutate(residuos = modelo3_Ca$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo3_Ca$residuals),
                            sd = sd(modelo3_Ca$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###
ols_test_breusch_pagan(modelo3_Ca) # do pacote olsrr

#Resultado: p-value foi menor que 0.05, indicando que o modelo NÃO é homocedastico, ou seja, a variancia dos residuos 
# NÃO é constante. Existe correlação entre os residuos (termos de erro) e a variavel X, bem como
# há omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
datasetCal$fitted_M3_Ca <- modelo3_Ca$fitted.values #add fitted values pro dataset
datasetCal$residuos_M3_Ca <- modelo3_Ca$residuals #add residos pro dataset

datasetCal %>%
  ggplot() +
  geom_point(aes(x = fitted_M3_Ca, y = residuos_M3_Ca, color = Field),
            size = 3) +
  geom_text_repel(aes(x = fitted_M3_Ca, y = residuos_M3_Ca, label = ID), 
            color = "black", size = 3.5)+
  labs(x = "Fitted Values",
       y = "Resíduos") +
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 3 para Ca ####

#R²
a <- (sum((datasetCal$fitted_M3_Ca - mean(datasetCal$Ca))^2))/
  ((sum((datasetCal$fitted_M3_Ca - mean(datasetCal$Ca))^2)) + (sum((datasetCal$residuos_M3_Ca)^2)))
#round(R2_M1Ca_Cal, digits = 4)

#RMSE
b <- mean(sqrt((datasetCal$residuos_M3_Ca)^2))

#RMSE%
c <- b*(100/mean(datasetCal$Ca))

#RPD
d <- sd(datasetCal$Ca)/b

#RPIQ
e <- (quantile(datasetCal$Ca, probs = 0.75)-quantile(datasetCal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 3 - Ca - Calibracao
Result_M3Ca_Cal <- data.frame(ID = 'M3_Ca_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 3 para Ca usando o conjunto de validação ####
predict(object = modelo3_Ca,
        datasetVal) -> datasetVal$fitted_M3_Ca

datasetVal$fitted_M3_Ca - datasetVal$Ca -> datasetVal$residuos_M3_Ca

### Calculo dos parametros de qualidade para conj. de VAL modelo 1 para Ca ####

#R²
a <- (sum((datasetVal$fitted_M3_Ca - mean(datasetVal$Ca))^2))/
  ((sum((datasetVal$fitted_M3_Ca - mean(datasetVal$Ca))^2)) + (sum((datasetVal$residuos_M3_Ca)^2)))

#RMSE
b <- mean(sqrt((datasetVal$residuos_M3_Ca)^2))

#RMSE%
c <- b*(100/mean(datasetVal$Ca))

#RPD
d <- sd(datasetVal$Ca)/b

#RPIQ
e <- (quantile(datasetVal$Ca, probs = 0.75)-quantile(datasetVal$Ca, probs = 0.25))/b

#Resumo dos Resultados Modelo 3 - Ca - validacao
Result_M3Ca_Val <- data.frame(ID = 'M3_Ca_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 3 - Ca 
Result_M3Ca <- bind_rows(Result_M3Ca_Cal, Result_M3Ca_Val)
rm(Result_M3Ca_Cal,Result_M3Ca_Val)

####################################
### Estimando o modelo 3 para K ###
####################################
str(datasetCal)

modelo3_K <- lm(formula = K ~ KK + AlK + SiK + FeK + TiK + RhKC,
                data = datasetCal)

summary(modelo3_K)
summ(modelo3_K, confint = T, digits = 4, ci.width = .95)
export_summs(modelo3_K, scale = F, digits = 4)

## Aplicando o procedimento Stepwise, temos o seguinte código:##
modelo3_K <- step(modelo3_K, k = 3.841459)

summary(modelo3_K)

#Foram mantidas apenas as variáveis Fe e Ca, nao usou nem a variável Compton (e se normalizarmos e usar MLR?)

export_summs(modelo3_K, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(modelo3_K, level = 0.95) # siginificância 5%
plot_summs(modelo3_K, colors = "#440154FF") #função plot_summs do pacote ggstance

#Parâmetros padronizados
plot_summs(modelo3_K, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(modelo3_K, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

### Verificação da aderência dos residuos à normalidade via Shapiro-Francia (n>30) ###

sf.test(modelo3_K$residuals) #função sf.test do pacote nortest
#Resultado: p-value foi menor que 0.05 - Os residos NÃO são normais e nao precisam de transformação

FAZER TRANSFORMACAO BOXCOX

#Histograma dos resíduos do modelo OLS linear
datasetCal %>%
  mutate(residuos = modelo3_K$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo3_K$residuals),
                            sd = sd(modelo3_K$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

### Diagnóstico de heterocedasticidade via teste de Breusch-Pagan ###
ols_test_breusch_pagan(modelo3_K) # do pacote olsrr

#Resultado: p-value foi menor que 0.05, indicando que o modelo NAO é homocedastico, ou seja, a variancia dos residuos 
# NAO é constante. Existe correlação entre os residuos (termos de erro) e a variavel X, bem como
#  há omissão variáveis explicativas relevantes no modelo.


### Plotando grafico que relaciona resíduos e fitted values do modelo
datasetCal$fitted_M3_K <- modelo3_K$fitted.values #add fitted values pro dataset
datasetCal$residuos_M3_K <- modelo3_K$residuals #add residos pro dataset

datasetCal %>%
  ggplot() +
  geom_point(aes(x = fitted_M3_K, y = residuos_M3_K, color = Field),
             size = 3) +
  geom_text_repel(aes(x = fitted_M3_K, y = residuos_M3_K, label = ID), 
                  color = "black", size = 3.5)+
  labs(x = "Fitted Values",
       y = "Resíduos") +
  theme_bw()

### Calculo dos parametros de qualidade para conj. de CAL modelo 2 para K ####

#R²
a <- (sum((datasetCal$fitted_M3_K - mean(datasetCal$K))^2))/
  ((sum((datasetCal$fitted_M3_K - mean(datasetCal$K))^2)) + (sum((datasetCal$residuos_M3_K)^2)))
#round(R2_M1Ca_Cal, digits = 4)

#RMSE
b <- mean(sqrt((datasetCal$residuos_M3_K)^2))

#RMSE%
c <- b*(100/mean(datasetCal$K))

#RPD
d <- sd(datasetCal$K)/b

#RPIQ
e <- (quantile(datasetCal$K, probs = 0.75)-quantile(datasetCal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 3 - Ca - Calibracao
Result_M3K_Cal <- data.frame(ID = 'M3_K_Cal', R2 = a, RMSE = b, RMSEp = c,RPD = d, RPIQ = e, row.names = NULL)

### Validando o modelo 3 para Ca usando o conjunto de validação ####
predict(object = modelo3_K,
        datasetVal) -> datasetVal$fitted_M3_K

datasetVal$fitted_M3_K - datasetVal$K -> datasetVal$residuos_M3_K

### Calculo dos parametros de qualidade para conj. de VAL modelo 3 para K ####

#R²
a <- (sum((datasetVal$fitted_M3_K - mean(datasetVal$K))^2))/
  ((sum((datasetVal$fitted_M3_K - mean(datasetVal$K))^2)) + (sum((datasetVal$residuos_M3_K)^2)))

#RMSE
b <- mean(sqrt((datasetVal$residuos_M3_K)^2))

#RMSE%
c <- b*(100/mean(datasetVal$K))

#RPD
d <- sd(datasetVal$K)/b

#RPIQ
e <- (quantile(datasetVal$K, probs = 0.75)-quantile(datasetVal$K, probs = 0.25))/b

#Resumo dos Resultados Modelo 3 - K - Validacao
Result_M3K_Val <- data.frame(ID = 'M3_K_Val',R2 = a, RMSE = b, RMSEp = c, RPD = d, RPIQ = e, row.names = NULL)

#Resumo geral - Resultados Modelo 3 - K
Result_M3K <- bind_rows(Result_M3K_Cal, Result_M3K_Val)
rm(Result_M3K_Cal,Result_M3K_Val)

####Resumo geral - Resultados Modelo 3 ####
Result_M3 <- bind_rows(Result_M3K, Result_M3Ca)
rm(Result_M3K,Result_M3Ca)
#write.table(Result_M3, file='Result_M3.csv',row.names = F, sep=',')

####Dataset com fitted values da Cal e Val####
Res_fit_M3_Cal <- datasetCal %>% 
  select("ID","Field","Set","Ca","K","fitted_M3_Ca","residuos_M3_Ca","fitted_M3_K", "residuos_M3_K") 


Res_fit_M3_Val <- datasetVal %>% 
  select("ID","Field","Set","Ca","K","fitted_M3_Ca","residuos_M3_Ca","fitted_M3_K", "residuos_M3_K")

#write.table(Res_fit_M1_Cal, file='Res_fit_M1_Cal.csv',row.names = F, sep=',')
#write.table(Res_fit_M1_Val, file='Res_fit_M1_Val.csv',row.names = F, sep=',')

#### Plotando graficos de dispersao ####
### Grafico de pontos para Ca
names(Res_fit_M2_Cal)

G1 <- Res_fit_M3_Cal %>%
  ggplot() +
  geom_point(aes(x = Ca, y = fitted_M3_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Calibration using RLM for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))
#theme_light() theme_bw() theme(legend.position=c(0.9, 0.1))

G2 <- Res_fit_M3_Val %>%
  ggplot() +
  geom_point(aes(x = Ca, y = fitted_M3_Ca, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Ca measured",
       y = "Ca predicted",
       title = "Validation using RLM for Ca") +
  theme_bw() +
  geom_smooth(aes(x = Ca, y = Ca), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

### Grafico de pontos para K

G3 <- Res_fit_M3_Cal %>%
  ggplot() +
  geom_point(aes(x = K, y = fitted_M3_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Calibration using RLM for K") +
  theme_bw() +
  geom_smooth(aes(x = K, y = K), method = "lm", 
              color = "grey", size = 1.05,
              linetype = "longdash")+
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

G4 <- Res_fit_M3_Val %>%
  ggplot() +
  geom_point(aes(x = K, y = fitted_M3_K, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "K measured",
       y = "K predicted",
       title = "Validation using RLM for K") +
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