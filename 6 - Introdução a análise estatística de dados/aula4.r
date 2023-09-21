

# Dados do Sistema de Informações sobre Nascidos vivos (SINASC) para o ano de 2014

setwd("K:/PPGICS/Ciencia de dados/dn/2014") #Define o diretório de trabalho

install.packages("read.dbc") #Pacote necessário para ler os dados do SINASC disponíveis no sitio do Datasus (http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1&acao=28&pad=31655)
library(read.dbc)

#Os dados do SINASC são disponibilizados por UF e ano de nascimento

DNAC14 <- read.dbc("DNAC2014.dbc")
DNAL14 <- read.dbc("DNAL2014.dbc")
DNAM14 <- read.dbc("DNAM2014.dbc")
DNAP14 <- read.dbc("DNAP2014.dbc")
DNBA14 <- read.dbc("DNBA2014.dbc")
DNCE14 <- read.dbc("DNCE2014.dbc")
DNDF14 <- read.dbc("DNDF2014.dbc")
DNES14 <- read.dbc("DNES2014.dbc")
DNGO14 <- read.dbc("DNGO2014.dbc")
DNMA14 <- read.dbc("DNMA2014.dbc")
DNMG14 <- read.dbc("DNMG2014.dbc")
DNMS14 <- read.dbc("DNMS2014.dbc")
DNMT14 <- read.dbc("DNMT2014.dbc")
DNPA14 <- read.dbc("DNPA2014.dbc")
DNPB14 <- read.dbc("DNPB2014.dbc")
DNPE14 <- read.dbc("DNPE2014.dbc")
DNPI14 <- read.dbc("DNPI2014.dbc")
DNPR14 <- read.dbc("DNPR2014.dbc")
DNRJ14 <- read.dbc("DNRJ2014.dbc")
DNRN14 <- read.dbc("DNRN2014.dbc")
DNRO14 <- read.dbc("DNRO2014.dbc")
DNRR14 <- read.dbc("DNRR2014.dbc")
DNRS14 <- read.dbc("DNRS2014.dbc")
DNSC14 <- read.dbc("DNSC2014.dbc")
DNSE14 <- read.dbc("DNSE2014.dbc")
DNSP14 <- read.dbc("DNSP2014.dbc")
DNTO14 <- read.dbc("DNTO2014.dbc")


DN2014 <- rbind(DNAC14,  DNAL14,  DNAM14, DNAP14, DNBA14, DNCE14, DNDF14, DNES14, DNGO14, DNMA14, DNMG14, DNMS14, DNMT14, DNPA14, DNPB14, DNPE14, DNPI14, DNPR14, DNRJ14, DNRN14, DNRO14, DNRR14, DNRS14, DNSC14, DNSE14, DNSP14, DNTO14)

#Índice de Apgar no 1º minuto

DN2014$APGAR1_2<-as.numeric(as.character(DN2014$APGAR1))
DN2014$APGAR5_2<-as.numeric(as.character(DN2014$APGAR5))


#Substituindo o 99 por NA
DN2014$APGAR1_2 [DN2014$APGAR1 == 99]<-NA
DN2014$APGAR5_2 [DN2014$APGAR5 == 99]<-NA

library(descr)

# Frequencia da variável APGAR

t1<-freq(DN2014$APGAR1_2)
pct_acumulado<-cumsum(t1[,3])
pct_acumulado
t1c<-cbind(t1,pct_acumulado)


barplot(t1c[1:11,3], beside=TRUE, xlab="Índice de Apgar no 1o. minuto", ylab="%", ylim=c(0,100), main="Distribuição do Índice de Apgar no 1o. minuto. Brasil, 2014", sub="fonte:SINASC/DATASUS")

#Peso ao nascer

DN2014$PESO2<-as.numeric(as.character(DN2014$PESO))


#Substituindo o 99 por NA
DN2014$PESO2 [DN2014$PESO == 99]<-NA


# Frequencia da variável Peso ao nascer

t5<-freq(DN2014$PESO2)
pct_acumulado5<-cumsum(t5[,3])
pct_acumulado5
t5c<-cbind(t5,pct_acumulado5)


barplot(t5c[,3], beside=TRUE, xlab="Peso ao nascer", ylab="%",ylim=c(0,2),  main="Distribuição do Índice de Apgar no 1o. minuto. Brasil, 2014", sub="fonte:SINASC/DATASUS")

#Eliminando os NA's
x<-DN2014$PESO2
x1<-subset(x, !is.na(x))

x<-hist(DN2014$PESO2, breaks = 200, xlab="Peso ao nascer", ylab="nº de casos", main="Distribuição do peso ao nascer. Brasil, 2014", sub="fonte:SINASC/DATASUS")
curve(dnorm(x,mean=3184,sd=554.8),col=2,lty=2,lwd=2,add=TRUE)


#Eliminando os NA's e valores abaixo de 50

x<-DN2014$PESO2
x1<-subset(x, !is.na(x) & x > 50)

hist(x1, breaks = 50,prob= T, xlab="Peso ao nascer", ylab="Densidade", main="Distribuição do peso ao nascer. Brasil, 2014", sub="fonte:SINASC/DATASUS")
curve(dnorm(x, mean=3184,sd=450),col=2,lwd=2,add=TRUE)

#Calculando os decis

quantile(x1,seq(0.10,0.9,0.1), na.rm = TRUE)         


#Baixo peso ao nascer (<2500g)

x2<-x1
x2 [x1 < 2500]<-1
x2 [x1 >= 2500]<-2
freq(x2)

# Idade gestacional

DN2014$SEMAGESTAC2<-as.numeric(as.character(DN2014$SEMAGESTAC))

#Parto prematuro (<37 semanas)
DN2014$PREMATUR [DN2014$SEMAGESTAC2 < 37]<-1
DN2014$PREMATUR [DN2014$SEMAGESTAC2 >= 37]<-2



#Exemplo da concentração de poluentes

X <- rnorm(5000, mean = 8, sd = 1.5)
Z <- rnorm(5000, mean = 8, sd = 1.5)

hist(X, prob=T, plot=F)
curve(dnorm(x, mean=8,sd=1.5),col=2,lwd=2,add=TRUE)

curve(dnorm(x, mean=8,sd=1.5),col=2,lwd=2,xlim=c(2.75,13.25), ylab="Densidade", xlab="X (em ppm)")
curve(dnorm(x, mean=0,sd=1),col=1,lwd=2,xlim=c(-3.5,3.5), ylab="Densidade", xlab="Z (em devios-padrão)")

1-pnorm(10,mean = 8, sd = 1.5)

1-pnorm(1.33,mean = 0, sd = 1)


plot(X, dnorm(X, mean=8,sd=1.5), axes = FALSE, type = 'l', xlab = '',ylab = '') ; abline(h = 0)
X <- 0 ; lines(c(0, 0), c(dnorm(X), -0.01))
X <- -1 ; lines(c(-1, 0), c(dnorm(X), dnorm(X)))
arrows(-1, dnorm(X), 0, dnorm(X), code = 3, length = 0.1)
text(0.2, 0.2, expression(italic(mu)))
text(-0.5, 0.26,
expression(italic(sigma)))



# Agora vamos fazer uma amostra aleatória de 1000 nascimentos e comparar as estimativas com os parâmetros populacionais

# Seleciona uma amostra aleatória dos dados do SINASC de tamanho n=1000

#set.seed(10)

a_sinasc_1000 <- DN2014[sample(seq_len(nrow(DN2014)),size=1000,replace=F),]


#Parâmetrso populacionais
media_pesonasc<-mean(DN2014$PESO2, na.rm=T)
media_idgest<-mean(DN2014$SEMAGESTAC2, na.rm=T)
var_pesonasc<-var(DN2014$PESO2, na.rm=T)
var_idgest<-var(DN2014$SEMAGESTAC2, na.rm=T)

parametros_pesonasc<-as.numeric(c(media_pesonasc,var_pesonasc))
parametros_idgest<-as.numeric(c(media_idgest,var_idgest))
nomes1<-c(" Média da população","Variância da população")
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
nomes2<-c(" Média amostral","Variância amostral","Desvio-padrão amostral","Erro-padrão")
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

# Gráfico Função Densidade 
x <- pretty(c(3100, 3300), 1000)

y <- dnorm(x, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightblue")
title(main = "Distribuição Normal \nFunção Densidade")
z1 <- qnorm(0.975, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)
z2 <- qnorm(0.025, mean=media_pesonasc_amostra,sd=ep_pesonasc_amostra)
lines(c(z1, z1), y = c(dnorm(-3), dnorm(z1)))
text(x = z1 + 0.3, y = dnorm(z1) + 0.01,round( z1 , digits=1))
lines(c(z2, z2), y = c(dnorm(-3), dnorm(z2)))
text(x = z2 + 0.3, y = dnorm(z2) + 0.01,round( z2 , digits=1))


# Gráfico Função Densidade 
x <- pretty(c(38, 39), 1000)

y <- dnorm(x, mean=media_idgest_amostra,sd=ep_idgest_amostra)

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribuição Normal \nFunção Densidade")
z1 <- qnorm(0.975, mean=media_idgest_amostra,sd=ep_idgest_amostra)
z2 <- qnorm(0.025, mean=media_idgest_amostra,sd=ep_idgest_amostra)
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 2.5,round( z1 , digits=1))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 2.5,round( z2 , digits=1))



#Função n R para cálculo do intervalo de confiança

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
nomes3<-c(" Média da população","Média amostral","Limite inferior do IC(95%)", "Limite superior do IC(95%)")
TAB_3<-data.frame(nomes3,est_pesonasc,est_idgest)
TAB_3




#Teste de hipósteses

#Teste T para duas amostras independentes


#Transformando a variável em numérica
a_sinasc_1000$IDADEMAE2<-as.numeric(as.character(a_sinasc_1000$IDADEMAE))

#Substituindo o 99 por NA
a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$IDADEMAE == 99]<-NA

mean(a_sinasc_1000$IDADEMAE2, na.rm = TRUE)


a_sinasc_1000$BPN [a_sinasc_1000$PESO2 < 2500]<-1
a_sinasc_1000$BPN [a_sinasc_1000$PESO2 >= 2500]<-2


#Comparação da idade materna entre nascimantos com BPN e nascimentos com peso adequado

a1<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$BPN == 1]
a2<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$BPN == 2]

tt1<-t.test(a1,a2,alternative="two.sided",mu=0)

par(mfrow=c(1,1))


# Gráfico Função Densidade 
x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt1[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribuição Normal \nFunção Densidade")
z1 <- qt(0.975, df=as.numeric(tt1[2]))
z2 <- qt(0.025, df=as.numeric(tt1[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))






#Comparação da idade materna entre nascimantos prematuros e nascimentos a termo

a3<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$PREMATUR == 1]
a4<-a_sinasc_1000$IDADEMAE2 [a_sinasc_1000$PREMATUR == 2]

tt2<-t.test(a3,a4,alternative="two.sided",mu=0)


# Gráfico Função Densidade 
x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt2[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribuição Normal \nFunção Densidade")
z1 <- qt(0.975, df=as.numeric(tt2[2]))
z2 <- qt(0.025, df=as.numeric(tt2[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))


tt3<-t.test(a3,a4,alternative="two.sided",mu=0, var.equal=TRUE)


# Gráfico Função Densidade 


x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt3[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribuição Normal \nFunção Densidade")
z1 <- qt(0.975, df=as.numeric(tt3[2]))
z2 <- qt(0.025, df=as.numeric(tt3[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))











#Comparação entre apgar no primeiro minuto e apgar no quinto minuto

apgar1<-a_sinasc_1000$APGAR1_2 
apgar5<-a_sinasc_1000$APGAR5_2 

tt4<-t.test(apgar5,apgar1,paired=TRUE,alternative="two.sided",mu=0, var.equal=TRUE)


# Gráfico Função Densidade 
x <- pretty(c(-3.2, 3.2), 1000)

y <- dt(x, df=as.numeric(tt4[2]))

plot.new()
plot.window(xlim=range(x), ylim=range(y))
axis(1);axis(2)
polygon(x, y, col = "lightpink")
title(main = "Distribuição Normal \nFunção Densidade")
z1 <- qt(0.975, df=as.numeric(tt4[2]))
z2 <- qt(0.025, df=as.numeric(tt4[2]))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.1,round( z1 , digits=3))
lines(c(z2, z2), y = c(1, dnorm(z2)))
text(x = z2, y = dnorm(z2) + 0.1,round( z2 , digits=3))












#Teste Qui-quadrado


#Teste para associação entre BPN e Prematuridade


a_sinasc_1000$BPN_C<-factor(a_sinasc_1000$BPN)
levels(a_sinasc_1000$BPN_C)[1]<-"<2500g"
levels(a_sinasc_1000$BPN_C)[2]<-">=2500g"



a_sinasc_1000$PREMATUR_C<-factor(a_sinasc_1000$PREMATUR)
levels(a_sinasc_1000$PREMATUR_C)[1]<-"Prematuro"
levels(a_sinasc_1000$PREMATUR_C)[2]<-"A termo"


crostab1 <- table(a_sinasc_1000$BPN_C,a_sinasc_1000$PREMATUR_C) 
crostab1 

margin.table(crostab1, 1) # Totais da variável escolaridade 
margin.table(crostab1, 2) # Totais da variável BPN 

prop.table(crostab1) # Percentual da tabela
prop.table(crostab1, 1) # percentual nas linhas 
prop.table(crostab1, 2) # percentual nas colunas


tt5<-chisq.test(crostab1)

curve(dchisq(x,  df=as.numeric(tt5[2])), xlim=(c(0,6)))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.2,round( z1 , digits=3))



#Teste para associação entre BPN e faixa etária da mãe

a_sinasc_1000$BPN_C<-factor(a_sinasc_1000$BPN)
levels(a_sinasc_1000$BPN_C)[1]<-"<2500g"
levels(a_sinasc_1000$BPN_C)[2]<-">=2500g"



a_sinasc_1000$PREMATUR_C<-factor(a_sinasc_1000$PREMATUR)
levels(a_sinasc_1000$PREMATUR_C)[1]<-"Prematuro"
levels(a_sinasc_1000$PREMATUR_C)[2]<-"A termo"


crostab1 <- table(a_sinasc_1000$BPN_C,a_sinasc_1000$PREMATUR_C) 
crostab1 

margin.table(crostab1, 1) # Totais da variável escolaridade 
margin.table(crostab1, 2) # Totais da variável BPN 

prop.table(crostab1) # Percentual da tabela
prop.table(crostab1, 1) # percentual nas linhas 
prop.table(crostab1, 2) # percentual nas colunas


chisq.test(crostab1)



a_sinasc_1000$FXET [a_sinasc_1000$IDADEMAE2 < 20]<-1
a_sinasc_1000$FXET [a_sinasc_1000$IDADEMAE2 >= 20]<-2



a_sinasc_1000$FXET_C<-factor(a_sinasc_1000$FXET)
levels(a_sinasc_1000$FXET_C)[1]<-"Até 19 anos"
levels(a_sinasc_1000$FXET_C)[2]<-"20 anos ou mais"



crostab2 <- table(a_sinasc_1000$BPN_C,a_sinasc_1000$FXET_C) 
crostab2 

margin.table(crostab2, 1) # Totais da variável escolaridade 
margin.table(crostab2, 2) # Totais da variável BPN 

prop.table(crostab2) # Percentual da tabela
prop.table(crostab2, 1) # percentual nas linhas 
prop.table(crostab2, 2) # percentual nas colunas


tt6<-chisq.test(crostab2)



curve(dchisq(x,  df=as.numeric(tt6[2])), xlim=(c(0,6)))
lines(c(z1, z1), y = c(1, dnorm(z1)))
text(x = z1, y = dnorm(z1) + 0.2,round( z1 , digits=3))



