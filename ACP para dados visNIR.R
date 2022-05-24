#####
#####Pacotes para PCA
#####

pacotes <- c("plotly","readxl","olsrr","grid", "nortest","jtools","tidyverse","reshape2","plotly", "gridExtra", "tidyverse","ggrepel","knitr","kableExtra","ggrepel","rgl","knitr","kableExtra","PerformanceAnalytics",
             "factoextra","reshape2","psych","ggrepel", "REdaS", "FactoMineR", "cluster", "dendextend", "NbClust", "corrplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#importando dados
db <- read_excel("Dataframe_visNIR.xlsx")
glimpse(db)

#Criando variavel ID 
row.names(db) <- db$ID
#db <- db %>% column_to_rownames("ID")

#
#
#
######
# Analise de componente principal e cluster para TODAS as variáveis juntas
######
#
#
#

# Salvando a Matriz de Correlações -----------------------------------

db3 <- db %>% select(ID, 7:357) #selecionando apenas spectra
rho_data <- cor(db3[,2:352]) #fazendo matriz de correlacao 

## calculando KMO a partir da biblioteca REdaS ------------------
#library(REdaS)
#KMOS(db3[,2:19])
#KMOS(mcdonalds[,3:19]) #tirando carboidrato que tem KMO < 0.5
#KMOS(mcdonalds[,4:12]) #tirando fibra alim que tem KMO < 0.5

# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho_data) 
#options(scipen = 999)

##4. Teste de Bartlett
# H0: a matriz de correlacao e igual a matriz identidade
# H1: a matriz de correlacao e diferente a matriz identidade
# queremos rejeitar H0 para poder fazer a analise fatoria,se o valor calculado é maior que o tabelado entao rejeita-se H0.
# Neste caso,o p-Value é menor que 0.05 entao rejeita-se H0 e podemos fazer PCA


# Rodando a PCA com Script Pratica

afpc <- prcomp(db3[,2:352], rank. = 6, scale. = T)
summary(afpc) #ele solta o SD, para chegar aos eigenvalues é só eleva-los ao quadrado

#Visualizando os pesos que cada variável tem em cada componente principal 
#obtido pela PCA #####TIRAR
data.frame(afpc$rotation) %>%
  mutate(var = names(db3[,2:352])) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

# Scree Plot - apenas ignorar os warnings
ggplotly(
  fviz_eig(X = afpc, #funcao do pacote factoextra
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod3")
)

# Extraindo as Cargas Fatoriais (acima de 80% da variancia explicada)
#k <- sum((afpc$sdev ^ 2) > 1) #número de variáveis presentes na base de dados
k <- 3
cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k]) #%*% indica multiplicação matricial

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) #%>%
#kable() %>%
#kable_styling(bootstrap_options = "striped", 
#             full_width = T, 
#            font_size = 12)

#Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) #%>%
# kable() %>%
#kable_styling(bootstrap_options = "striped", 
#             full_width = T, 
#            font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("Principal Component 1", paste0("(",
                                                 round(summary(afpc)$importance[2,1] * 100,
                                                       digits = 2),
                                                 "%)")),
       y = paste("Principal Component 2", paste0("(",
                                                 round(summary(afpc)$importance[2,2] * 100,
                                                       digits = 2),
                                                 "%)"))) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme_bw()



# Grafico de cargas fatoriais de outra forma com library FactoMineR
acp3 <- PCA(db3[,2:352], scale.unit = T, graph = T, ncp=3)
summary(acp3)


# Calculo do fator 1, 2, 3 e 4 ------------------------------------

#Rodando PCA com pacote psych

acp3 <- principal(db3[,2:352], nfactors=3, rotate="varimax", scores=T)
acp3

db3$Score_PC1 <- acp3$scores[,1]
db3$Score_PC2 <- acp3$scores[,2]
db3$Score_PC3 <- acp3$scores[,3]

#juntando variaveis pelo ID
db %>% select(ID, Field, Clay, Sand) -> a

db3 %>% select(ID, 353:355) -> db4
a %>% left_join(db4, by = "ID") -> db4

write.table(db4, file='db_PCs_Text.csv',row.names = F, sep=',')

#grafico scores
db4 %>% 
  ggplot() +
  geom_point(aes(x = Score_PC1, y = Score_PC2, color = Field),
             size = 3,) + #color = "#55C667FF"
  labs(x = "Principal Component 1",
       y = "Principal Component 2") +
  geom_text_repel(aes(x = Score_PC1, y = Score_PC2, label = ID), 
                  color = "black", size = 4)+
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5), #ajustes nos titulo
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)#,legend.position=c(0.9, 0.18), legend.text = element_text(size = 10), legend.title = element_text(size = 10)
  )
