install.packages("read.dbc", dependencies = T)
library("read.dbc")


DN2014<-read.dbc(file.choose())

View(DN2014)


str(DN2014)


teste<-require("descr")
if(teste==FALSE){
  install.packages("descr");rm(teste);} # Instala o pacote para a visualização de tabelas de frequencias

library(descr)


### Vamos começar a análise exploratória pela variável de escolaridade da mãe (ESCMAE)
#ESCMAE: Escolaridade, Anos de estudo concluídos (1: Nenhuma; 2: 1 a 3 anos; 3:4 a 7 anos; 4: 8 a 11 anos; 5: 12 e mais; 9: Ignorado )

freq(DN2014$ESCMAE)

attributes(DN2014$ESCMAE)


#Transformando a variável em numérica
DN2014$ESCMAE2<-as.numeric(as.character(DN2014$ESCMAE))

#Substituindo o 0 e o 9 por NA
DN2014$ESCMAE2 [DN2014$ESCMAE2 == 0]<-NA
DN2014$ESCMAE2 [DN2014$ESCMAE2 == 9]<-NA

#Trasnformando a variável em qualitativa e atribuindo rótulos para as categorias

DN2014$ESCMAE_C3<-factor(DN2014$ESCMAE2)
levels(DN2014$ESCMAE_C3)[1]<-"Nenhuma"
levels(DN2014$ESCMAE_C3)[2]<-"1 a 3 anos"
levels(DN2014$ESCMAE_C3)[3]<-"4 a 7 anos"
levels(DN2014$ESCMAE_C3)[4]<-"8 a 11 anos"
levels(DN2014$ESCMAE_C3)[5]<-"12 e mais"

t1<-freq(DN2014$ESCMAE_C3)
t1

#Calculando a frequencia acumulada
pct_acumulado<-cumsum(t1[,3])
pct_acumulado

cbind(t1,pct_acumulado)


# Análise exploratória da variável idade da mãe (IDADEMAE)

freq(DN2014$IDADEMAE)

attributes(DN2014$IDADEMAE)


#Transformando a variável em numérica
DN2014$IDADEMAE2<-as.numeric(as.character(DN2014$IDADEMAE))

#Substituindo o 99 por NA
DN2014$IDADEMAE2 [DN2014$IDADEMAE == 99]<-NA

mean(DN2014$IDADEMAE2, na.rm = TRUE)

summary(DN2014$IDADEMAE2)

var(DN2014$IDADEMAE2, na.rm = TRUE)
sd(DN2014$IDADEMAE2, na.rm = TRUE)

#Trsnformando a variável em qualitativa e atribuindo rótulos para as categorias
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 < 15]<-1
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 15 & DN2014$IDADEMAE2 < 20]<-2
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 20 & DN2014$IDADEMAE2 < 25]<-3
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 25 & DN2014$IDADEMAE2 < 30]<-4
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 30 & DN2014$IDADEMAE2 < 35]<-5
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 35 & DN2014$IDADEMAE2 < 40]<-6
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 40 & DN2014$IDADEMAE2 < 45]<-7
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 45 & DN2014$IDADEMAE2 < 50]<-8
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 50 & DN2014$IDADEMAE2 < 55]<-9
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 55 & DN2014$IDADEMAE2 < 60]<-10
DN2014$FAIXA_ETARIA_MAE[DN2014$IDADEMAE2 >= 60]<-11

DN2014$FAIXA_ETARIA_MAE<-factor(DN2014$FAIXA_ETARIA_MAE)
levels(DN2014$FAIXA_ETARIA_MAE)[1]<-"Até 14 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[2]<-"15-19 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[3]<-"20-24 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[4]<-"25-29 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[5]<-"30-34 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[6]<-"35-39 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[7]<-"40-44 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[8]<-"45-49 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[9]<-"50-54 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[10]<-"55-59 anos"
levels(DN2014$FAIXA_ETARIA_MAE)[11]<-"60 anos e mais"

t2<-freq(DN2014$FAIXA_ETARIA_MAE)
pct_acumulado<-cumsum(t2[,3])

cbind(t2,pct_acumulado)

#Outra forma de fazer as tabelas de frequencia

frequencia <- table(DN2014$FAIXA_ETARIA_MAE) 
pct_valido<-prop.table(frequencia)*100
pc_acumulado<-cumsum(pct_valido)

t3<-cbind(frequencia,pct_valido,pc_acumulado)
t3


# Transformando fatores em variáveis numéricas

DN2014$PESO2<-as.numeric(as.character(DN2014$PESO))
DN2014$SEMAGESTAC2<-as.numeric(as.character(DN2014$SEMAGESTAC))
DN2014$RACACORMAE2<-as.numeric(as.character(DN2014$RACACORMAE))
DN2014$CODESTAB2<-as.numeric(as.character(DN2014$CODESTAB))   
DN2014$APGAR1_2<-as.numeric(as.character(DN2014$APGAR1))   
DN2014$APGAR5_2<-as.numeric(as.character(DN2014$APGAR5))  
DN2014$CONSPRENAT2<-as.numeric(as.character(DN2014$CONSPRENAT)) 
DN2014$PARTO2<-as.numeric(as.character(DN2014$PARTO)) 


# Raça/cor

DN2014$RACACORMAE_C<-factor(DN2014$RACACORMAE2)
levels(DN2014$RACACORMAE_C)[1]<-"Branca"
levels(DN2014$RACACORMAE_C)[2]<-"Preta"
levels(DN2014$RACACORMAE_C)[3]<-"Amarela"
levels(DN2014$RACACORMAE_C)[4]<-"Parda"
levels(DN2014$RACACORMAE_C)[5]<-"Indígena"


#Baixo peso ao nascer (<2500g)

DN2014$BPN [DN2014$PESO2 < 2500]<-1
DN2014$BPN [DN2014$PESO2 >= 2500]<-0

DN2014$BPN_C<-factor(DN2014$BPN)
levels(DN2014$BPN_C)[1]<-">=2500g"
levels(DN2014$BPN_C)[2]<-"<2500g"

#Parto prematuro (<37 semanas)

DN2014$PREMATUR [DN2014$SEMAGESTAC2 < 37]<-1
DN2014$PREMATUR [DN2014$SEMAGESTAC2 >= 37]<-0

#Tipo de parto
#Substituindo o 99 por NA

DN2014$PARTO2 [DN2014$PARTO == 9]<-NA

DN2014$PARTO_C<-factor(DN2014$PARTO2)
levels(DN2014$PARTO_C)[1]<-"Vaginal"
levels(DN2014$PARTO_C)[2]<-"Cesáreo"



# ATENÇÃO: Executar os comandos do arquivo Recode_esfera_adm.R 

#Esfera administrativa públuca vs privada

DN2014$ESFERA_PUBLICA [DN2014$ESFERA_ADM < 5]<-1
DN2014$ESFERA_PUBLICA [DN2014$ESFERA_ADM == 5]<-0

DN2014$ESFERA_PUBLICA_C<-factor(DN2014$ESFERA_PUBLICA)
levels(DN2014$ESFERA_PUBLICA_C)[1]<-"Privado"
levels(DN2014$ESFERA_PUBLICA_C)[2]<-"Público"




#Medidas de resumo

#Média e mediana

mean(DN2014$IDADEMAE2, na.rm = TRUE)
mean(DN2014$PESO2, na.rm = TRUE)
mean(DN2014$SEMAGESTAC2, na.rm = TRUE)
mean(DN2014$CONSPRENAT2, na.rm = TRUE)

summary(DN2014$IDADEMAE2)
summary(DN2014$PESO2)
summary(DN2014$SEMAGESTAC2)
summary(DN2014$CONSPRENAT2)



#Exemplo média e mediana da altura de nove estudantes (Cap. 18 livro)

amostra1<-c(1.20,1.22,1.23,1.25,1.26,1.27,1.28,1.29,1.30)
summary(amostra1)

amostra2<-c(1.20,1.22,1.23,1.25,1.26,1.27,1.28,1.29,1.80)
summary(amostra2)


# Quantis

quantile(DN2014$IDADEMAE2, na.rm = TRUE)     
quantile(DN2014$PESO2, na.rm = TRUE)         
quantile(DN2014$SEMAGESTAC2, na.rm = TRUE)   
quantile(DN2014$CONSPRENAT2, na.rm = TRUE)   



quantile(DN2014$IDADEMAE2,seq(0.10,0.9,0.1), na.rm = TRUE)     
quantile(DN2014$PESO2,seq(0.10,0.9,0.1), na.rm = TRUE)         
quantile(DN2014$SEMAGESTAC2,seq(0.10,0.9,0.1), na.rm = TRUE)   
quantile(DN2014$CONSPRENAT2,seq(0.10,0.9,0.1), na.rm = TRUE) 


# Medidas de dispersão

# Mínimo e máximo

min(DN2014$IDADEMAE2, na.rm = TRUE)     
min(DN2014$PESO2, na.rm = TRUE)         
min(DN2014$SEMAGESTAC2, na.rm = TRUE)   
min(DN2014$CONSPRENAT2, na.rm = TRUE)  

max(DN2014$IDADEMAE2, na.rm = TRUE)     
max(DN2014$PESO2, na.rm = TRUE)         
max(DN2014$SEMAGESTAC2, na.rm = TRUE)   
max(DN2014$CONSPRENAT2, na.rm = TRUE)  


# Amplitude

range(DN2014$IDADEMAE2, na.rm = TRUE)     

max(DN2014$IDADEMAE2, na.rm = TRUE)- min(DN2014$IDADEMAE2, na.rm = TRUE) 

# Variância

var(DN2014$IDADEMAE2, na.rm = TRUE)     
var(DN2014$PESO2, na.rm = TRUE)         
var(DN2014$SEMAGESTAC2, na.rm = TRUE)   
var(DN2014$CONSPRENAT2, na.rm = TRUE)  

# Desvio-padrão

sd(DN2014$IDADEMAE2, na.rm = TRUE)     
sd(DN2014$PESO2, na.rm = TRUE)         
sd(DN2014$SEMAGESTAC2, na.rm = TRUE)   
sd(DN2014$CONSPRENAT2, na.rm = TRUE)  


# Coeficiente de variação

cv1<-sd(DN2014$IDADEMAE2, na.rm = TRUE)/mean(DN2014$IDADEMAE2, na.rm = TRUE)*100
cv2<-sd(DN2014$PESO2, na.rm = TRUE)/mean(DN2014$PESO2, na.rm = TRUE)*100
cv3<-sd(DN2014$SEMAGESTAC2, na.rm = TRUE)/mean(DN2014$SEMAGESTAC2, na.rm = TRUE)*100
cv4<-sd(DN2014$CONSPRENAT2, na.rm = TRUE)/mean(DN2014$CONSPRENAT2, na.rm = TRUE)*100

cv1
cv2
cv3
cv4


# Apresentação gráfica

par(mfrow=c(2,2)) #permite que sejam apresentados 4 gráficos na mesma janela


# Gráfico de setores


pie(table(DN2014$ESCMAE_C3))
title("Escolaridade")
pie(table(DN2014$PREMATUR))
title("Partos prematuros")
pie(table(DN2014$BPN))
title("Baixo peso ao nascer")
pie(table(DN2014$ESFERA_PUBLICA))
title("Esfera administrativa do hospital")


# Gráfico de Colunas


barplot(table(DN2014$ESCMAE_C3))
title("Escolaridade")
barplot(table(DN2014$PREMATUR))
title("Partos prematuros")
barplot(table(DN2014$BPN))
title("Baixo peso ao nascer")
barplot(table(DN2014$ESFERA_PUBLICA))
title("Esfera administrativa do hospital") 



# Gráfico de Barras


barplot(table(DN2014$ESCMAE_C3), horiz = T)
title("Escolaridade")
barplot(table(DN2014$PREMATUR), horiz = T)
title("Partos prematuros")
barplot(table(DN2014$BPN), horiz = T)
title("Baixo peso ao nascer")
barplot(table(DN2014$ESFERA_PUBLICA), horiz = T)
title("Esfera administrativa do hospital") 



# Gráfico de linhas

prematuros_ano<-read.csv(file="dados_datasus_sinasc.csv", sep=";") # Abrir arquivo com o percentual de partos prematuros por ano  

str(prematuros_ano)


plot (prematuros_ano$ano,prematuros_ano$prop_premat_BR, xlab="Ano de nascimento", ylab="%", main="% de partos prematuros", xlim=c(2003,2014), ylim=c(0,15), col="red", type=c("l"))


# Histograma

hist(DN2014$SEMAGESTAC2, prob=T,col="blue")


hist(DN2014$PESO2, prob=T,col="red")


#Boxplot



boxplot(DN2014$PESO2)

boxplot(DN2014$SEMAGESTAC2)



#cruzamento de variáveis



crostab1 <- table(DN2014$PARTO_C, DN2014$ESFERA_PUBLICA_C) 
crostab1 

margin.table(crostab1, 1) # Totais da variável escolaridade 
margin.table(crostab1, 2) # Totais da variável BPN 

prop.table(crostab1) # Percentual da tabela
prop.table(crostab1, 1) # percentual nas linhas 
prop.table(crostab1, 2) # percentual nas colunas




crostab2 <- table(DN2014$ESCMAE_C3,DN2014$BPN_C) 
crostab2 

margin.table(crostab2, 1) # Totais da variável escolaridade 
margin.table(crostab2, 2) # Totais da variável BPN 

prop.table(crostab2) # Percentual da tabela
prop.table(crostab2, 1) # percentual nas linhas 
prop.table(crostab2, 2) # percentual nas colunas




#Gráficos de cruzamentos de variáveis



graf1<-table(DN2014$BPN_C, DN2014$ESCMAE_C3)
graf1p<-prop.table(graf1, 2)*100
barplot(graf1p, xlab="Escolaridade da mãe", ylab="%", ylim=c(0,100),legend.text=rownames(graf1))


graf2<-table(DN2014$PARTO_C, DN2014$ESFERA_PUBLICA_C)
graf2p<-prop.table(graf2, 2)*100
barplot(graf2p, xlab="Esfera Administrativa", ylab="%", ylim=c(0,100),legend.text=rownames(graf2))


#Com as barras ao lado

barplot(graf2p, beside=TRUE, xlab="Esfera Administrativa", ylab="%", ylim=c(0,100),legend.text=rownames(ddd2), main="Distribuição do tipo de parto segundo esfera administrativa do hospital. Brasil, 2014", sub="fonte:SINASC/DATASUS")

#Boxplot 


boxplot(DN2014$PESO2~DN2014$ESFERA_PUBLICA_C)
boxplot(DN2014$SEMAGESTAC2~DN2014$ESFERA_PUBLICA_C)








