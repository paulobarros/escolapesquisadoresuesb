######################################################
##      I ESCOLA DE PESQUISADORES DA UESB           ##
##                                                  ##  
##  Técnicas Instrumentais na Pesquisa Científica   ##
##                Linguagem R                       ##
##                                                  ##
##                Paulo Barros                      ##
###################################################### 

#### PARTE 1 ####

# Operações matemáticas ####

5 + 3
2 - 1
4 / 2
3 * 7

# Objetos ####

# Vetores

a <- c(1,2,3,4,5)

b <- 1:5


# Matrizes

A <- matrix(c(1:9),nrow = 3)

B <- matrix(c(1:9),nrow = 3, byrow = T)

# Listas

minha_lista <- list(c(1:5), c("a","b"), "biologia")

lista_nomeada <- list(A = c(3:6), B = "estatistica")

listao <- list(A,a,B,b, minha_lista, lista_nomeada)

listao[[6]][2]


# Removendo objetos do ambiente

# Remove um objeto especifico
rm(A)

# Remove todos os objetos do ambiente
rm(list=ls())



#### PARTE 2 ####


# Diretorio de trabalho (Ctrl + Shift + H) ####

setwd("~/Documents/PALESTRA-R/EscolaPesquisadores/escolapesquisadoresuesb/IntroR")

getwd()


# Pacotes ####

library(tidyverse)

install.packages("nome_do_pacote")


# Datasets ####

data(package = .packages(all.available = TRUE))

library(nlme)
data(BodyWeight)


# Importando dados ####

# Arquivos CSV

ratos <- read.csv("PesosRatos.csv", header = TRUE)

# Arquivos Texto

ratos2 <-  read.table("PesosRatosMod.txt", header = TRUE)



## PARTE 3 - Explorando os dados ####

library(tidyverse)

## Subseting e Indexação ####

## dados[linha,coluna]

ratos[1:10]

ratos[1:10,1:2]

ratos[1:10,c('peso','tempo')]

head(ratos)
tail(ratos)

dim(ratos)

ratos$rato == 1


ratos[ratos$rato == 1,]

### Explorando dados "the tidy way..." ####


ratos %>% slice(1:10)

ratos %>% select(peso,rato) %>% slice(20:30)

ratos %>% filter(rato == 1)

ratos %>% filter(tempo >= 40 & peso > 400) %>% select(rato) %>% distinct()

# Uma visão geral dos dados

ratos %>% summary()

# Agrupando os dados por animais

ratos %>% group_by(rato) %>% summarise(media = mean(peso), dp = sd(peso), min = min(peso), max = max(peso))

# Descritivas desconsiderando dados perdidos (NA's)

ratos %>% group_by(rato) %>% summarise(media = mean(peso, na.rm = TRUE), dp = sd(peso, na.rm = TRUE), min = min(peso, na.rm = TRUE), max = max(peso, na.rm = TRUE))



# Como lidar com dados perdidos?? ####

teste <- c(1,3,4,7,NA,5,2)

mean(teste)

mean(teste, na.rm = TRUE)


# Efeito de substituição por Zero

testeZero <- c(1,3,4,7,0,5,2)

mean(testeZero)

# Contando dados perdidos

ratos %>% select(everything()) %>% summarise_all(list(~sum(is.na(.))))

# Contando o número de individuos

ratos %>% select(rato) %>% distinct()

ratos %>% select(rato) %>% distinct() %>% tally()

# Eliminando individuos com NA

# Quais animais possuem dados perdidos?

ratos %>% filter(is.na(peso)) %>% select(rato) %>% distinct()

# Filtrando animais com dados perdidos. Ainda algum animal com NA?

ratos %>% filter(rato != 17 & rato != 18) %>% select(everything()) %>% summarise_all(list(~sum(is.na(.))))

# Quantos animais após filtro?

ratos %>% filter(rato != 17 & rato != 18) %>% select(rato) %>% distinct() %>% tally()


# Criando outro data frame com os dados sem NAs

ratos_full <- ratos %>% filter(rato != 17 & rato != 18)



## Lidando com Outliers  ####

ratos_full %>% group_by(rato) %>% summarise(media = mean(peso), dp = sd(peso), min = min(peso), max = max(peso))


ratos_full %>% filter(rato == 19)


## Uma imagem vale mais que mil tabelas... ####

boxplot(peso ~ rato, ratos_full)


ratos_full %>% 
  ggplot(aes(y = peso, x = as.factor(rato))) +
  geom_boxplot(aes(fill = as.factor(dieta))) +
  labs(title = "Pesos de Ratos submetidos a diferentes dietas",
       x = "Indivíduos",
       y = "Peso(g)") + theme_classic()


ratos_full %>% 
  ggplot(aes(y = peso, x = as.factor(rato))) +
  geom_boxplot(aes(fill = as.factor(dieta))) +
  labs(title = "Pesos de Ratos submetidos a diferentes dietas",
       x = "Indivíduos",
       y = "Peso(g)") + 
  scale_fill_grey() + theme_classic()
 

ratos %>% 
  ggplot(aes(y = peso, x = as.factor(dieta))) +
  geom_boxplot(aes(fill = as.factor(dieta))) +
  labs(title = "Pesos de Ratos submetidos a diferentes dietas",
       x = "Indivíduos",
       y = "Peso(g)", fill = "Dietas") + theme_classic()


 
# Como lidar com outliers?? ####

ratos_full %>% filter(rato == 19)


ratos_clean <- ratos_full %>% filter(rato != 19)

ratos_clean %>% 
  ggplot(aes(y = peso, x = as.factor(dieta))) +
  geom_boxplot(aes(fill = as.factor(dieta))) +
  labs(title = "Pesos de Ratos submetidos a diferentes dietas",
       x = "Indivíduos",
       y = "Peso(g)", fill="Dietas") + theme_classic()



# Criando novas variaveis a partir de existentes

head(ratos_clean)

pi <- ratos_clean %>% filter(tempo == 1) %>% select(rato,dieta,peso) %>% rename(.,p_inicial = peso )
pf <- ratos_clean %>% filter(tempo == 64) %>% select(rato,dieta,peso) %>% rename(.,p_final = peso )
  

ratos_clean %>% 
  select(rato, dieta) %>% 
  distinct() %>% 
  left_join(., pi, by = c("rato","dieta")) %>%
  left_join(., pf, by = c("rato","dieta")) %>%
  mutate(ganho = p_final  - p_inicial)

ratos_final <- 
  ratos_clean %>% 
  select(rato, dieta) %>% 
  distinct() %>% 
  left_join(., pi, by = c("rato","dieta")) %>%
  left_join(., pf, by = c("rato","dieta")) %>%
  mutate(ganho = p_final  - p_inicial)


ratos_final %>%
  ggplot(aes(x = as.factor(dieta), y = ganho)) +
  geom_boxplot() +
  theme_classic()

# Obtendo uma tabela de dados médios ####


library(rstatix)
library(ggpubr)

ratos_final %>% group_by(dieta) %>% get_summary_stats(p_inicial,p_final,ganho,type="common")


ratos_final %>% 
  ggplot(aes(y = ganho, x = as.factor(dieta))) +
  geom_boxplot(aes(fill = as.factor(dieta))) +
  labs(title = "Pesos de Ratos submetidos a diferentes dietas",
       x = "Dietas",
       y = "Ganho de peso(g)", fill="Dietas") +
  theme_classic()
  


## ANOVA ####
# Analise de Variância

# Testando pressuposições ####

# Normalidade ####

# Teste de Shapiro-Wilk
# Ho (Hipotese Nula) = a amostra segue distribuição normal.

ratos_final %>% group_by(dieta) %>% shapiro_test(ganho)

ratos_clean %>% ggqqplot(.,"peso", facet.by= c("dieta","rato")) 



# Homogeneidade de variâncias

# Levene Test
# Ho = as variâncias são iguais (homocedasticas/homogêneas).

ratos_final %>% levene_test(ganho ~ as.factor(dieta))


ratos_final %>% dplyr::filter(rato != 11) %>% levene_test(ganho ~ as.factor(dieta))



# Realizando ANOVA 

ratos_final %>%  
  aov(ganho ~ as.factor(dieta), data = .) %>%
  summary.aov()


# Comparando Tratamentos

library(agricolae)

hsd <- ratos_final %>%  
  aov(ganho ~ as.factor(dieta), data = .) %>% 
  HSD.test(., "as.factor(dieta)", group = TRUE, console=TRUE)
  

# Plotando resultador do teste de Tukey


ratos_final %>% 
  ggplot(aes(y = ganho, x = as.factor(dieta), fill = as.factor(dieta)),color = "black") +
  geom_boxplot(alpha  = 0.8) +
  scale_fill_manual(values = c("#A0815F", "#5FA081", "#815FA0")) +
  theme_classic() +
  annotate("text",label = "b", x = 1, y = 40, size = 4.5, vjust = -0.1) +
  annotate("text",label = "a", x = 2, y = 93,size = 4.5, vjust = -0.1) +
  annotate("text",label = "ab", x = 3, y = 61,size = 4.5, vjust = -0.1) +
  theme(legend.position="bottom",text = element_text(family = "Source Sans Pro", size = 14), 
        plot.title = element_text(face = "bold"),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = element_blank(), y = "Ganho de peso (g)", 
       title = "Ratos submetidos a diferentes dietas",
       fill = "Dietas",
       subtitle = expression(paste("Anova, ",italic(F),"(2,13) = 8.06, ",italic(p < 0.05))),
       caption = expression(paste(italic('post-hoc test'),":",bold('Tukey HSD'))))















#Cause and Effect Diagram or Fish- bone Sample
#install.packages("qcc")
library("qcc")

#Effect
effect<-"Less Productivity"

#Causes
causes.head<-c("Measurement", "Material", "Methods", "Environment", "Manpower", "Machines")

#Individual Causes
causes<-vector(mode = "list", length = length(causes.head))
causes[1]<-list(c("Lab error", "Contamination"))
causes[2]<-list(c("Raw Material", "Additive"))
causes[3]<-list(c("Sampling", "Analytical Procedure"))
causes[4]<-list(c("Rust near sample point"))
causes[5]<-list(c("Poor analyst","No guidance"))
causes[6]<-list(c("Leakage", "breakdown"))

#Fishbone Diagram
#install.packages("SixSigma")
library("SixSigma")
ss.ceDiag(effect,causes.head,causes,sub="ABC Pvt. Ltd",ss.col = c("","red"))



