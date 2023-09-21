

# Dados do Sistema de Informa??es sobre Nascidos vivos (SINASC) para o ano de 2014

setwd("~/aula_2018")

install.packages("read.dbc") #Pacote necess?rio para ler os dados do SINASC dispon?veis no sitio do Datasus (http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1&acao=28&pad=31655)
library(read.dbc)

#Os dados do SINASC s?o disponibilizados por UF e ano de nascimento


DNRJ14 <- read.dbc("DNRJ2014.dbc")

par(mfrow=c(1,1))



View(DNRJ14)


str(DNRJ14)


install.packages("descr") # Instala o pacote para a visualiza??o de tabelas de frequencias
library(descr)


# Vamos come?ar a an?lise explorat?ria pela vari?vel de escolaridade da m?e (ESCMAE)
#ESCMAE: Escolaridade, Anos de estudo conclu?dos (1: Nenhuma; 2: 1 a 3 anos; 3:4 a 7 anos; 4: 8 a 11 anos; 5: 12 e mais; 9: Ignorado )


freq(DNRJ14$ESCMAE)

attributes(DNRJ14$ESCMAE)


#Transformando a vari?vel em num?rica
DNRJ14$ESCMAE2<-as.numeric(as.character(DNRJ14$ESCMAE))

#Substituindo o 0 e o 9 por NA
DNRJ14$ESCMAE2 [DNRJ14$ESCMAE2 == 0]<-NA
DNRJ14$ESCMAE2 [DNRJ14$ESCMAE2 == 9]<-NA

#Trasnformando a vari?vel em qualitativa e atribuindo r?tulos para as categorias

DNRJ14$ESCMAE_C3<-factor(DNRJ14$ESCMAE2)
levels(DNRJ14$ESCMAE_C3)[1]<-"Nenhuma"
levels(DNRJ14$ESCMAE_C3)[2]<-"1 a 3 anos"
levels(DNRJ14$ESCMAE_C3)[3]<-"4 a 7 anos"
levels(DNRJ14$ESCMAE_C3)[4]<-"8 a 11 anos"
levels(DNRJ14$ESCMAE_C3)[5]<-"12 e mais"

t1<-freq(DNRJ14$ESCMAE_C3)
t1

#Calculando a frequencia acumulada
pct_acumulado<-cumsum(t1[,3])
pct_acumulado

cbind(t1,pct_acumulado)


# An?lise explorat?ria da vari?vel idade da m?e (IDADEMAE)


freq(DNRJ14$IDADEMAE)

attributes(DNRJ14$IDADEMAE)


#Transformando a vari?vel em num?rica
DNRJ14$IDADEMAE2<-as.numeric(as.character(DNRJ14$IDADEMAE))

#Substituindo o 99 por NA
DNRJ14$IDADEMAE2 [DNRJ14$IDADEMAE == 99]<-NA

mean(DNRJ14$IDADEMAE2, na.rm = TRUE)

summary(DNRJ14$IDADEMAE2)


#Trsnformando a vari?vel em qualitativa e atribuindo r?tulos para as categorias

DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 < 15]<-1
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 15 & DNRJ14$IDADEMAE2 < 20]<-2
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 20 & DNRJ14$IDADEMAE2 < 25]<-3
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 25 & DNRJ14$IDADEMAE2 < 30]<-4
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 30 & DNRJ14$IDADEMAE2 < 35]<-5
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 35 & DNRJ14$IDADEMAE2 < 40]<-6
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 40 & DNRJ14$IDADEMAE2 < 45]<-7
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 45 & DNRJ14$IDADEMAE2 < 50]<-8
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 50 & DNRJ14$IDADEMAE2 < 55]<-9
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 55 & DNRJ14$IDADEMAE2 < 60]<-10
DNRJ14$FAIXA_ETARIA_MAE[DNRJ14$IDADEMAE2 >= 60]<-11

DNRJ14$FAIXA_ETARIA_MAE<-factor(DNRJ14$FAIXA_ETARIA_MAE)
levels(DNRJ14$FAIXA_ETARIA_MAE)[1]<-"At? 14 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[2]<-"15-19 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[3]<-"20-24 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[4]<-"25-29 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[5]<-"30-34 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[6]<-"35-39 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[7]<-"40-44 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[8]<-"45-49 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[9]<-"50-54 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[10]<-"55-59 anos"
levels(DNRJ14$FAIXA_ETARIA_MAE)[11]<-"60 anos e mais"

t2<-freq(DNRJ14$FAIXA_ETARIA_MAE)
pct_acumulado<-cumsum(t2[,3])

cbind(t2,pct_acumulado)

#Outra forma de fazer as tabelas de frequencia

frequencia <- table(DNRJ14$FAIXA_ETARIA_MAE) 
pct_valido<-prop.table(frequencia)*100
pc_acumulado<-cumsum(pct_valido)

t3<-cbind(frequencia,pct_valido,pc_acumulado)
t3


# Transformando fatores em vari?veis num?ricas

DNRJ14$PESO2<-as.numeric(as.character(DNRJ14$PESO))
DNRJ14$SEMAGESTAC2<-as.numeric(as.character(DNRJ14$SEMAGESTAC))
DNRJ14$RACACORMAE2<-as.numeric(as.character(DNRJ14$RACACORMAE))
DNRJ14$CODESTAB2<-as.numeric(as.character(DNRJ14$CODESTAB))   
DNRJ14$APGAR1_2<-as.numeric(as.character(DNRJ14$APGAR1))   
DNRJ14$APGAR5_2<-as.numeric(as.character(DNRJ14$APGAR5))  
DNRJ14$CONSPRENAT2<-as.numeric(as.character(DNRJ14$CONSPRENAT)) 
DNRJ14$PARTO2<-as.numeric(as.character(DNRJ14$PARTO)) 

# Ra?a/cor
DNRJ14$RACACORMAE_C<-factor(DNRJ14$RACACORMAE2)
levels(DNRJ14$RACACORMAE_C)[1]<-"Branca"
levels(DNRJ14$RACACORMAE_C)[2]<-"Preta"
levels(DNRJ14$RACACORMAE_C)[3]<-"Amarela"
levels(DNRJ14$RACACORMAE_C)[4]<-"Parda"
levels(DNRJ14$RACACORMAE_C)[5]<-"Ind?gena"


#Baixo peso ao nascer (<2500g)
DNRJ14$BPN [DNRJ14$PESO2 < 2500]<-1
DNRJ14$BPN [DNRJ14$PESO2 >= 2500]<-0

DNRJ14$BPN_C<-factor(DNRJ14$BPN)
levels(DNRJ14$BPN_C)[1]<-">=2500g"
levels(DNRJ14$BPN_C)[2]<-"<2500g"

#Parto prematuro (<37 semanas)
DNRJ14$PREMATUR [DNRJ14$SEMAGESTAC2 < 37]<-1
DNRJ14$PREMATUR [DNRJ14$SEMAGESTAC2 >= 37]<-0

#Tipo de parto
#Substituindo o 99 por NA
DNRJ14$PARTO2 [DNRJ14$PARTO == 9]<-NA

DNRJ14$PARTO_C<-factor(DNRJ14$PARTO2)
levels(DNRJ14$PARTO_C)[1]<-"Vaginal"
levels(DNRJ14$PARTO_C)[2]<-"Ces?reo"



# ATEN??O: Executar os comandos do arquivo Recode_esfera_adm.R 

#Esfera administrativa p?bluca vs privada
DNRJ14$ESFERA_PUBLICA [DNRJ14$ESFERA_ADM < 5]<-1
DNRJ14$ESFERA_PUBLICA [DNRJ14$ESFERA_ADM == 5]<-0

DNRJ14$ESFERA_PUBLICA_C<-factor(DNRJ14$ESFERA_PUBLICA)
levels(DNRJ14$ESFERA_PUBLICA_C)[1]<-"Privado"
levels(DNRJ14$ESFERA_PUBLICA_C)[2]<-"P?blico"




#Medidas de resumo

#M?dia e mediana

mean(DNRJ14$IDADEMAE2, na.rm = TRUE)
mean(DNRJ14$PESO2, na.rm = TRUE)
mean(DNRJ14$SEMAGESTAC2, na.rm = TRUE)
mean(DNRJ14$CONSPRENAT2, na.rm = TRUE)

summary(DNRJ14$IDADEMAE2)
summary(DNRJ14$PESO2)
summary(DNRJ14$SEMAGESTAC2)
summary(DNRJ14$CONSPRENAT2)



#Exemplo m?dia e mediana da altura de nove estudantes (Cap. 18 livro)

amostra1<-c(1.20,1.22,1.23,1.25,1.26,1.27,1.28,1.29,1.30)
summary(amostra1)

amostra2<-c(1.20,1.22,1.23,1.25,1.26,1.27,1.28,1.29,1.80)
summary(amostra2)


# Quantis

quantile(DNRJ14$IDADEMAE2, na.rm = TRUE)     
quantile(DNRJ14$PESO2, na.rm = TRUE)         
quantile(DNRJ14$SEMAGESTAC2, na.rm = TRUE)   
quantile(DNRJ14$CONSPRENAT2, na.rm = TRUE)   



quantile(DNRJ14$IDADEMAE2,seq(0.10,0.9,0.1), na.rm = TRUE)     
quantile(DNRJ14$PESO2,seq(0.10,0.9,0.1), na.rm = TRUE)         
quantile(DNRJ14$SEMAGESTAC2,seq(0.10,0.9,0.1), na.rm = TRUE)   
quantile(DNRJ14$CONSPRENAT2,seq(0.10,0.9,0.1), na.rm = TRUE) 


# Medidas de dispers?o

# M?nimo e m?ximo

min(DNRJ14$IDADEMAE2, na.rm = TRUE)     
min(DNRJ14$PESO2, na.rm = TRUE)         
min(DNRJ14$SEMAGESTAC2, na.rm = TRUE)   
min(DNRJ14$CONSPRENAT2, na.rm = TRUE)  

max(DNRJ14$IDADEMAE2, na.rm = TRUE)     
max(DNRJ14$PESO2, na.rm = TRUE)         
max(DNRJ14$SEMAGESTAC2, na.rm = TRUE)   
max(DNRJ14$CONSPRENAT2, na.rm = TRUE)  
 
 
# Amplitude

range(DNRJ14$IDADEMAE2, na.rm = TRUE)     

max(DNRJ14$IDADEMAE2, na.rm = TRUE)- min(DNRJ14$IDADEMAE2, na.rm = TRUE) 

# Vari?ncia

var(DNRJ14$IDADEMAE2, na.rm = TRUE)     
var(DNRJ14$PESO2, na.rm = TRUE)         
var(DNRJ14$SEMAGESTAC2, na.rm = TRUE)   
var(DNRJ14$CONSPRENAT2, na.rm = TRUE)  

# Desvio-padr?o

sd(DNRJ14$IDADEMAE2, na.rm = TRUE)     
sd(DNRJ14$PESO2, na.rm = TRUE)         
sd(DNRJ14$SEMAGESTAC2, na.rm = TRUE)   
sd(DNRJ14$CONSPRENAT2, na.rm = TRUE)  


# Coeficiente de varia??o

cv1<-sd(DNRJ14$IDADEMAE2, na.rm = TRUE)/mean(DNRJ14$IDADEMAE2, na.rm = TRUE)*100
cv2<-sd(DNRJ14$PESO2, na.rm = TRUE)/mean(DNRJ14$PESO2, na.rm = TRUE)*100
cv3<-sd(DNRJ14$SEMAGESTAC2, na.rm = TRUE)/mean(DNRJ14$SEMAGESTAC2, na.rm = TRUE)*100
cv4<-sd(DNRJ14$CONSPRENAT2, na.rm = TRUE)/mean(DNRJ14$CONSPRENAT2, na.rm = TRUE)*100

cv1
cv2
cv3
cv4


# Apresenta??o gr?fica

par(mfrow=c(2,2)) #permite que sejam apresentados 4 gr?ficos na mesma janela


# Gr?fico de setores

 
pie(table(DNRJ14$ESCMAE_C3))
title("Escolaridade")
pie(table(DNRJ14$PREMATUR))
title("Partos prematuros")
pie(table(DNRJ14$BPN))
title("Baixo peso ao nascer")
pie(table(DNRJ14$ESFERA_PUBLICA))
title("Esfera administrativa do hospital")


# Gr?fico de Colunas


barplot(table(DNRJ14$ESCMAE_C3))
title("Escolaridade")
barplot(table(DNRJ14$PREMATUR))
title("Partos prematuros")
barplot(table(DNRJ14$BPN))
title("Baixo peso ao nascer")
barplot(table(DNRJ14$ESFERA_PUBLICA))
title("Esfera administrativa do hospital") 
 


# Gr?fico de Barras


barplot(table(DNRJ14$ESCMAE_C3), horiz = T)
title("Escolaridade")
barplot(table(DNRJ14$PREMATUR), horiz = T)
title("Partos prematuros")
barplot(table(DNRJ14$BPN), horiz = T)
title("Baixo peso ao nascer")
barplot(table(DNRJ14$ESFERA_PUBLICA), horiz = T)
title("Esfera administrativa do hospital") 
 


# Gr?fico de linhas

prematuros_ano<-read.csv(file="~/aula_2018/dados_datasus_sinasc.csv", sep=";") # Abrir arquivo com o percentual de partos prematuros por ano  
 
str(prematuros_ano)


plot (prematuros_ano$ano,prematuros_ano$prop_premat_BR, xlab="Ano de nascimento", ylab="%", main="% de partos prematuros", xlim=c(2003,2014), ylim=c(0,15), col="red", type=c("l"))

 
# Histograma
 
hist(DNRJ14$SEMAGESTAC2, prob=T,col="blue")


hist(DNRJ14$PESO2, prob=T,col="red")


#Boxplot



boxplot(DNRJ14$PESO2)

boxplot(DNRJ14$SEMAGESTAC2)



#cruzamento de vari?veis



crostab1 <- table(DNRJ14$PARTO_C, DNRJ14$ESFERA_PUBLICA_C) 
crostab1 

margin.table(crostab1, 1) # Totais da vari?vel escolaridade 
margin.table(crostab1, 2) # Totais da vari?vel BPN 

prop.table(crostab1) # Percentual da tabela
prop.table(crostab1, 1) # percentual nas linhas 
prop.table(crostab1, 2) # percentual nas colunas




crostab2 <- table(DNRJ14$ESCMAE_C3,DNRJ14$BPN_C) 
crostab2 

margin.table(crostab2, 1) # Totais da vari?vel escolaridade 
margin.table(crostab2, 2) # Totais da vari?vel BPN 

prop.table(crostab2) # Percentual da tabela
prop.table(crostab2, 1) # percentual nas linhas 
prop.table(crostab2, 2) # percentual nas colunas




#Gr?ficos de cruzamentos de vari?veis


 
graf1<-table(DNRJ14$BPN_C, DNRJ14$ESCMAE_C3)
graf1p<-prop.table(graf1, 2)*100
barplot(graf1p, xlab="Escolaridade da m?e", ylab="%", ylim=c(0,100),legend.text=rownames(graf1))
 
 
graf2<-table(DNRJ14$PARTO_C, DNRJ14$ESFERA_PUBLICA_C)
graf2p<-prop.table(graf2, 2)*100
barplot(graf2p, xlab="Esfera Administrativa", ylab="%", ylim=c(0,100),legend.text=rownames(graf2))
 

#Com as barras ao lado

barplot(graf2p, beside=TRUE, xlab="Esfera Administrativa", ylab="%", ylim=c(0,100),legend.text=rownames(graf2), main="Distribui??o do tipo de parto segundo esfera administrativa do hospital. Brasil, 2014", sub="fonte:SINASC/DATASUS")
 
#Boxplot 


boxplot(DNRJ14$PESO2~DNRJ14$ESFERA_PUBLICA_C)
boxplot(DNRJ14$SEMAGESTAC2~DNRJ14$ESFERA_PUBLICA_C)

  

# Testes de hip?stese e intervalo de confian?a


#?ndice de Apgar no 1? minuto

DNRJ14$APGAR1_2<-as.numeric(as.character(DNRJ14$APGAR1))
DNRJ14$APGAR5_2<-as.numeric(as.character(DNRJ14$APGAR5))


#Substituindo o 99 por NA
DNRJ14$APGAR1_2 [DNRJ14$APGAR1 == 99]<-NA
DNRJ14$APGAR5_2 [DNRJ14$APGAR5 == 99]<-NA

library(descr)

# Frequencia da vari?vel APGAR

t1<-freq(DNRJ14$APGAR1_2)
pct_acumulado<-cumsum(t1[,3])
pct_acumulado
t1c<-cbind(t1,pct_acumulado)


barplot(t1c[1:11,3], beside=TRUE, xlab="?ndice de Apgar no 1o. minuto", ylab="%", ylim=c(0,100), main="Distribui??o do ?ndice de Apgar no 1o. minuto. Brasil, 2014", sub="fonte:SINASC/DATASUS")

#Peso ao nascer

DNRJ14$PESO2<-as.numeric(as.character(DNRJ14$PESO))


#Substituindo o 99 por NA
DNRJ14$PESO2 [DNRJ14$PESO == 99]<-NA


# Frequencia da vari?vel Peso ao nascer

t5<-freq(DNRJ14$PESO2)
pct_acumulado5<-cumsum(t5[,3])
pct_acumulado5
t5c<-cbind(t5,pct_acumulado5)


barplot(t5c[,3], beside=TRUE, xlab="Peso ao nascer", ylab="%",ylim=c(0,2),  main="Distribui??o do ?ndice de Apgar no 1o. minuto. Brasil, 2014", sub="fonte:SINASC/DATASUS")

#Eliminando os NA's
x<-DNRJ14$PESO2
x1<-subset(x, !is.na(x))

x<-hist(DNRJ14$PESO2, breaks = 200, xlab="Peso ao nascer", ylab="n? de casos", main="Distribui??o do peso ao nascer. Brasil, 2014", sub="fonte:SINASC/DATASUS")
curve(dnorm(x,mean=3184,sd=554.8),col=2,lty=2,lwd=2,add=TRUE)


#Eliminando os NA's e valores abaixo de 50

x<-DNRJ14$PESO2
x1<-subset(x, !is.na(x) & x > 50)

hist(x1, breaks = 50,prob= T, xlab="Peso ao nascer", ylab="Densidade", main="Distribui??o do peso ao nascer. Brasil, 2014", sub="fonte:SINASC/DATASUS")
curve(dnorm(x, mean=3184,sd=450),col=2,lwd=2,add=TRUE)

#Calculando os decis

quantile(x1,seq(0.10,0.9,0.1), na.rm = TRUE)         


#Baixo peso ao nascer (<2500g)

x2<-x1
x2 [x1 < 2500]<-1
x2 [x1 >= 2500]<-2
freq(x2)

# Idade gestacional

DNRJ14$SEMAGESTAC2<-as.numeric(as.character(DNRJ14$SEMAGESTAC))

#Parto prematuro (<37 semanas)
DNRJ14$PREMATUR [DNRJ14$SEMAGESTAC2 < 37]<-1
DNRJ14$PREMATUR [DNRJ14$SEMAGESTAC2 >= 37]<-2



#Exemplo da concentra??o de poluentes

X <- rnorm(5000, mean = 8, sd = 1.5)
Z <- rnorm(5000, mean = 8, sd = 1.5)

hist(X, prob=T, plot=F)
curve(dnorm(x, mean=8,sd=1.5),col=2,lwd=2,add=TRUE)

curve(dnorm(x, mean=8,sd=1.5),col=2,lwd=2,xlim=c(2.75,13.25), ylab="Densidade", xlab="X (em ppm)")
curve(dnorm(x, mean=0,sd=1),col=1,lwd=2,xlim=c(-3.5,3.5), ylab="Densidade", xlab="Z (em devios-padr?o)")

1-pnorm(10,mean = 8, sd = 1.5)

1-pnorm(1.33,mean = 0, sd = 1)


plot(X, dnorm(X, mean=8,sd=1.5), axes = FALSE, type = 'l', xlab = '',ylab = '') ; abline(h = 0)
X <- 0 ; lines(c(0, 0), c(dnorm(X), -0.01))
X <- -1 ; lines(c(-1, 0), c(dnorm(X), dnorm(X)))
arrows(-1, dnorm(X), 0, dnorm(X), code = 3, length = 0.1)
text(0.2, 0.2, expression(italic(mu)))
text(-0.5, 0.26,
     expression(italic(sigma)))



# Agora vamos fazer uma amostra aleat?ria de 1000 nascimentos e comparar as estimativas com os par?metros populacionais

# Seleciona uma amostra aleat?ria dos dados do SINASC de tamanho n=1000

#set.seed(10)

a_sinasc_1000 <- DNRJ14[sample(seq_len(nrow(DNRJ14)),size=1000,replace=F),]


#Par?metrso populacionais
media_pesonasc<-mean(DNRJ14$PESO2, na.rm=T)
media_idgest<-mean(DNRJ14$SEMAGESTAC2, na.rm=T)
var_pesonasc<-var(DNRJ14$PESO2, na.rm=T)
var_idgest<-var(DNRJ14$SEMAGESTAC2, na.rm=T)

parametros_pesonasc<-as.numeric(c(media_pesonasc,var_pesonasc))
parametros_idgest<-as.numeric(c(media_idgest,var_idgest))
nomes1<-c(" M?dia da popula??o","Vari?ncia da popula??o")
TAB_1<-data.frame(nomes1,parametros_pesonasc,parametros_idgest)
TAB_1

#Calcula vas estimativas amostrais 

media_pesonasc_amostra<-mean(a_sinasc_1000$PESO2, na.rm=T)
media_idgest_amostra<-mean(a_sinasc_1000$SEMAGESTAC2, na.rm=T)
var_pesonasc_amostra<-var(a_sinasc_1000$PESO2, na.rm=T)
var_idgest_amostra<-var(a_sinasc_1000$SEMAGESTAC2, na.rm=T)
dp_pesonasc_amostra<-sd(a_sinasc_1000$PESO2, na.rm=T)
dp_idgest_amostra<-sd(a_sinasc_1000$SEMAGESTAC2, na.rm=T)
ep_pesonasc_amostra<-dp_pesonasc_amostra / sqrt(1000)
ep_idgest_amostra<-dp_idgest_amostra / sqrt(1000)

estimativas_pesonasc<-as.numeric(c(media_pesonasc_amostra,var_pesonasc_amostra,dp_pesonasc_amostra,ep_pesonasc_amostra))
estimativas_idgest<-as.numeric(c(media_idgest_amostra,var_idgest_amostra,dp_idgest_amostra,ep_idgest_amostra))
nomes2<-c(" M?dia amostral","Vari?ncia amostral","Desvio-padr?o amostral","Erro-padr?o")
TAB_2<-data.frame(nomes2,estimativas_pesonasc,estimativas_idgest)
TAB_2

par(mfrow=c(1,2))
curve(dnorm(x, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra),col=2,lwd=2,xlim=c(3100,3300), ylab="Densidade", xlab="Peso ao nascer")
curve(dnorm(x, mean=media_idgest_amostra,sd=ep_idgest_amostra),col=1,lwd=2,xlim=c(38,39), ylab="Densidade", xlab="Idade gestacional")


z1p<-qnorm(0.025, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)
z2p<-qnorm(0.975, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)

z1i<-qnorm(0.025, mean=media_idgest_amostra,sd=ep_idgest_amostra)
z2i<-qnorm(0.975, mean=media_idgest_amostra,sd=ep_idgest_amostra)

par(mfrow=c(1,2))

# Gr?fico Fun??o Densidade 
x <- pretty(c(3100, 3300), 1000)

y <- dnorm(x, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightblue")
title(main = "Distribui??o Normal \nFun??o Densidade")
z1 <- qnorm(0.975, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)
z2 <- qnorm(0.025, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)
lines(c(z1, z1), y = c(dnorm(-3), dnorm(z1)))
text(x = z1 + 0.3, y = dnorm(z1) + 0.01,round( z1 , digits=1))
lines(c(z2, z2), y = c(dnorm(-3), dnorm(z2)))
text(x = z2 + 0.3, y = dnorm(z2) + 0.01,round( z2 , digits=1))


# Gr?fico Fun??o Densidade 
x <- pretty(c(38, 39), 1000)

y <- dnorm(x, mean=media_idgest_amostra,sd=ep_idgest_amostra)

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribui??o Normal \nFun??o Densidade")
z1 <- qnorm(0.975, mean=media_idgest_amostra,sd=ep_idgest_amostra)
z2 <- qnorm(0.025, mean=media_idgest_amostra,sd=ep_idgest_amostra)
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 2.5,round( z1 , digits=1))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 2.5,round( z2 , digits=1))



#Fun??o n R para c?lculo do intervalo de confian?a

ic.m = function(xb, conf = 0.95) {n <- length(na.omit(xb))
media <- mean(xb,na.rm=TRUE)
variancia <- var(xb,na.rm=TRUE)
quantis <- c(qnorm(((1-0.95)/2), mean = 0, sd = 1),-1*qnorm(((1-0.95)/2), mean = 0, sd = 1))
ic <- media + quantis * sqrt(variancia/n)
return(ic)}

IC95_pesonasc<-ic.m(a_sinasc_1000$PESO2, conf = 0.95)
IC95_idgest<-ic.m(a_sinasc_1000$SEMAGESTAC2, conf = 0.95)



est_pesonasc<-as.numeric(c(media_pesonasc,media_pesonasc_amostra,IC95_pesonasc[1],IC95_pesonasc[2]))
est_idgest<-as.numeric(c(media_idgest,media_idgest_amostra,IC95_idgest[1],IC95_idgest[2]))
nomes3<-c(" M?dia da popula??o","M?dia amostral","Limite inferior do IC(95%)", "Limite superior do IC(95%)")
TAB_3<-data.frame(nomes3,est_pesonasc,est_idgest)
TAB_3




#Teste de hip?steses

#Teste T para duas amostras independentes


#Transformando a vari?vel em num?rica
a_sinasc_1000$IDADEMAE2<-as.numeric(as.character(a_sinasc_1000$IDADEMAE))

#Substituindo o 99 por NA
a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$IDADEMAE == 99]<-NA

mean(a_sinasc_1000$IDADEMAE2, na.rm = TRUE)


a_sinasc_1000$BPN [a_sinasc_1000$PESO2 < 2500]<-1
a_sinasc_1000$BPN [a_sinasc_1000$PESO2 >= 2500]<-2


#Compara??o da idade materna entre nascimantos com BPN e nascimentos com peso adequado

a1<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$BPN == 1]
a2<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$BPN == 2]

tt1<-t.test(a1,a2,alternative="two.sided",mu=0)
tt1

par(mfrow=c(1,1))


# Gr?fico Fun??o Densidade 
x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt1[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribui??o Normal \nFun??o Densidade")
z1 <- qt(0.975, df=as.numeric(tt1[2]))
z2 <- qt(0.025, df=as.numeric(tt1[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))






#Compara??o da idade materna entre nascimantos prematuros e nascimentos a termo

a3<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$PREMATUR == 1]
a4<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$PREMATUR == 2]

tt2<-t.test(a3,a4,alternative="two.sided",mu=0)
tt2

# Gr?fico Fun??o Densidade 
x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt2[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribui??o Normal \nFun??o Densidade")
z1 <- qt(0.975, df=as.numeric(tt2[2]))
z2 <- qt(0.025, df=as.numeric(tt2[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))


tt3<-t.test(a3,a4,alternative="two.sided",mu=0, var.equal=TRUE)
tt3

# Gr?fico Fun??o Densidade 


x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt3[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribui??o Normal \nFun??o Densidade")
z1 <- qt(0.975, df=as.numeric(tt3[2]))
z2 <- qt(0.025, df=as.numeric(tt3[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))











#Compara??o entre apgar no primeiro minuto e apgar no quinto minuto

apgar1<-a_sinasc_1000$APGAR1_2 
apgar5<-a_sinasc_1000$APGAR5_2 

tt4<-t.test(apgar5,apgar1,paired=TRUE,alternative="two.sided",mu=0, var.equal=TRUE)
tt4

# Gr?fico Fun??o Densidade 
x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt4[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribui??o Normal \nFun??o Densidade")
z1 <- qt(0.975, df=as.numeric(tt4[2]))
z2 <- qt(0.025, df=as.numeric(tt4[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))












#Teste Qui-quadrado


#Teste para associa??o entre BPN e Prematuridade


a_sinasc_1000$BPN_C<-factor(a_sinasc_1000$BPN)
levels(a_sinasc_1000$BPN_C)[1]<-"<2500g"
levels(a_sinasc_1000$BPN_C)[2]<-">=2500g"



a_sinasc_1000$PREMATUR_C<-factor(a_sinasc_1000$PREMATUR)
levels(a_sinasc_1000$PREMATUR_C)[1]<-"Prematuro"
levels(a_sinasc_1000$PREMATUR_C)[2]<-"A termo"


crostab1 <- table(a_sinasc_1000$BPN_C,a_sinasc_1000$PREMATUR_C) 
crostab1 

margin.table(crostab1, 1) # Totais da vari?vel escolaridade 
margin.table(crostab1, 2) # Totais da vari?vel BPN 

prop.table(crostab1) # Percentual da tabela
prop.table(crostab1, 1) # percentual nas linhas 
prop.table(crostab1, 2) # percentual nas colunas


tt5<-chisq.test(crostab1)
tt5

curve(dchisq(x,  df=as.numeric(tt5[2])), xlim=(c(0,6)))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.2,round( z1 , digits=3))



#Teste para associa??o entre BPN e faixa et?ria da m?e

a_sinasc_1000$BPN_C<-factor(a_sinasc_1000$BPN)
levels(a_sinasc_1000$BPN_C)[1]<-"<2500g"
levels(a_sinasc_1000$BPN_C)[2]<-">=2500g"



a_sinasc_1000$PREMATUR_C<-factor(a_sinasc_1000$PREMATUR)
levels(a_sinasc_1000$PREMATUR_C)[1]<-"Prematuro"
levels(a_sinasc_1000$PREMATUR_C)[2]<-"A termo"


crostab1 <- table(a_sinasc_1000$BPN_C,a_sinasc_1000$PREMATUR_C) 
crostab1 

margin.table(crostab1, 1) # Totais da vari?vel escolaridade 
margin.table(crostab1, 2) # Totais da vari?vel BPN 

prop.table(crostab1) # Percentual da tabela
prop.table(crostab1, 1) # percentual nas linhas 
prop.table(crostab1, 2) # percentual nas colunas


chisq.test(crostab1)



a_sinasc_1000$FXET [a_sinasc_1000$IDADEMAE2 < 20]<-1
a_sinasc_1000$FXET [a_sinasc_1000$IDADEMAE2 >= 20]<-2



a_sinasc_1000$FXET_C<-factor(a_sinasc_1000$FXET)
levels(a_sinasc_1000$FXET_C)[1]<-"At? 19 anos"
levels(a_sinasc_1000$FXET_C)[2]<-"20 anos ou mais"



crostab2 <- table(a_sinasc_1000$BPN_C,a_sinasc_1000$FXET_C) 
crostab2 

margin.table(crostab2, 1) # Totais da vari?vel escolaridade 
margin.table(crostab2, 2) # Totais da vari?vel BPN 

prop.table(crostab2) # Percentual da tabela
prop.table(crostab2, 1) # percentual nas linhas 
prop.table(crostab2, 2) # percentual nas colunas


tt6<-chisq.test(crostab2)
tt6


curve(dchisq(x,  df=as.numeric(tt6[2])), xlim=(c(0,6)))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.2,round( z1 , digits=3))




