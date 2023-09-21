
# Modelo de regressão linear simples (exemplo dos pardais)

pardais<-structure(list(idade = c(3,4,5,6,8,9,10,11,12,14,15,16,17), comp.asa = c(1.4,1.5,2.2,2.4,3.1,3.2,3.2,3.9,4.1,4.7,4.5,5.2,5)
), .Names = c("Idade", "Comprimento da asa"), class = "data.frame", row.names = c(NA,-13L))


#Modelo de regressão linear Simples (Exemplo dos pardais)

plot(pardais$Idade,pardais$`Comprimento da asa`,  ylab="Idade", xlab="Comprimento da asa")

mod_exempl<-lm(`Comprimento da asa` ~ Idade, pardais)


anova(mod_exempl)

mod_exempl<-lm(`Comprimento da asa` ~ Idade, pardais)
summary(mod_exempl)


plot(pardais$Idade,pardais$`Comprimento da asa`,  ylab="Idade", xlab="Comprimento da asa")
abline(mod_exempl,lty=1)

predict(mod_exempl, data.frame(Idade=c(3,10,17)))


plot(pardais$Idade,pardais$`Comprimento da asa`,  ylab="Idade", xlab="Comprimento da asa")
abline(mod_exempl,lty=1)
abline(v = 13,lty=2)
abline(h = 4.226072,lty=2)



# Modelo de regressão linear multipla (exemplo dss crianças)



criancas<-structure(list(peso = c(40.3, 42, 43.6, 44.2, 45.5, 47.9, 46.2, 55.9, 57.7, 58.2, 60.9, 60.3, 65.4, 67.3, 58.2),
altura = c(157, 158.5, 152, 152.1, 158.7, 153.7, 154.8, 165.2, 159, 159.5, 167.3, 159.3, 167.3, 169.6, 160.8),
idade = c(11, 13, 12, 12, 12, 11, 11, 14, 14, 15, 15, 16, 15, 15, 16)
), .Names = c("Peso", "Altura", "Idade"), class = "data.frame", row.names = c(NA,-15L))


par(mfrow=c(1,2))
plot(criancas$Altura,criancas$Peso,  ylab="Peso (Kg)", xlab="Altura (cm)")
plot(criancas$Idade,criancas$Peso,  ylab="Peso (Kg)", xlab="Idade (anos)")

par(mfrow=c(1,1))

mod_exempl2 <- lm(Peso ~Altura + Idade, data=criancas)

summary(mod_exempl2)

predict(mod_exempl2, data.frame(Idade=c(3)),data.frame(Altura=c(157)))
predict(mod_exempl2, Idade=15, Altura=157)





#Gráfico em 3 dimensões

# Fazendo com os gráficos base:
# Primeiro, rode o modelo:
model <- glm(Peso ~Altura + Idade, data=criancas)

# Crie sequencias de x e y:
x <-range(criancas$Altura)
x <- seq(x[1], x[2], length.out=50)    
y <- range(criancas$Idade)
y <- seq(y[1], y[2], length.out=50)

# Crie os valores de z com todas as combinações de x e y:
z <- outer(x,y, 
           function(Altura,Idade)
                     predict(model, data.frame(Altura,Idade)))

# Faça o gráfico do modelo com persp:
p <- persp(x,y,z, theta=30, phi=30, 
           col="lightblue",expand = 0.5,shade = 0.2,
           xlab="Altura", ylab="Idade", zlab="Peso")

# Faça uma projeção dos pontos em 3d para 2d com trans3d e adicione os pontos (com points) e os segmentos (com segment):
obs <- trans3d(criancas$Altura, criancas$Idade,criancas$Peso,p)
pred <- trans3d(criancas$Altura, criancas$Idade,fitted(model),p)
points(obs, col="red",pch=16)
segments(obs$x, obs$y, pred$x, pred$y)



# opção 2
library(rockchalk)
require(rockchalk)
model <- glm(Peso ~ Altura + Idade, data = criancas)
plotPlane(model, plotx1 = "Altura", plotx2 = "Idade",
                 drawArrow = TRUE, pch = 20, pcol = 2,
                 npp = 100, theta = 20, phi = 30)
                 
                 
                 
                 
 # Modelo de regressão logística (exemplo doença coronária)

dados_CHD<-structure(list(idade = c(20,23,24,25,25,26,26,28,28,29,30,30,30,30,30,30,32,32,33,33,34,34,34,34,34,35,35,36,
36,36,37,37,37,38,38,39,39,40,40,41,41,42,42,42,42,43,43,43,44,44,44,44,45,45,46,46,47,47,47,48,48,48,49,49,49,50,50,51,
52,52,53,53,54,55,55,55,56,56,56,57,57,57,57,57,57,58,58,58,59,59,60,60,61,62,62,63,64,64,65,69), CHD = c(0,0,0,0,1,0,0,
0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,1,0,0,1,1,0,1,0,1,0,0,1,0,1,1,0,0,1,0,1,
0,0,1,1,1,1,0,1,1,1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,1)), .Names = c("Idade", "CHD"), class = "data.frame", row.names = c(NA,-100L))


plot(dados_CHD$Idade,dados_CHD$CHD,  ylab="Idade", xlab="CHD")


#Trasnformando a variável em qualitativa e atribuindo rótulos para as categorias

dados_CHD$FAIXA_ETARIA[dados_CHD$Idade <= 29]<-1
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 30 & dados_CHD$Idade < 35]<-2
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 35 & dados_CHD$Idade < 40]<-3
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 40 & dados_CHD$Idade < 45]<-4
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 45 & dados_CHD$Idade < 50]<-5
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 50 & dados_CHD$Idade < 55]<-6
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 55 & dados_CHD$Idade < 60]<-7
dados_CHD$FAIXA_ETARIA[dados_CHD$Idade >= 60]<-8



dados_CHD$FAIXA_ETARIA<-factor(dados_CHD$FAIXA_ETARIA)
levels(dados_CHD$FAIXA_ETARIA)[1]<-"20-29 anos"
levels(dados_CHD$FAIXA_ETARIA)[2]<-"30-34 anos"  
levels(dados_CHD$FAIXA_ETARIA)[3]<-"35-39 anos"  
levels(dados_CHD$FAIXA_ETARIA)[4]<-"40-44 anos"  
levels(dados_CHD$FAIXA_ETARIA)[5]<-"45-49 anos"  
levels(dados_CHD$FAIXA_ETARIA)[6]<-"50-54 anos"  
levels(dados_CHD$FAIXA_ETARIA)[7]<-"55-59 anos"       
levels(dados_CHD$FAIXA_ETARIA)[8]<-"60 anos e mais"   


tab1 <- table(dados_CHD$FAIXA_ETARIA,dados_CHD$CHD) 
prop_tab1<-prop.table(tab1, 1) # percentual nas linhas 




plot(prop_tab1[,2],  ylab="Faixa etária", xlab="CHD")


dados_CHD$fxet_55[dados_CHD$Idade < 55]<-0
dados_CHD$fxet_55[dados_CHD$Idade >= 55]<-1


mod_CHD=glm(CHD~fxet_55,data=dados_CHD, family=binomial(link="logit"))
summary(mod_CHD)

OR1=exp(mod_CHD$coefficients)
OR1

ICbeta1=confint.default(mod_CHD,level=0.95)
ICbeta1

ICOR1=exp(ICbeta1)
ICOR1

p1<-prob<-predict(mod_CHD, type = "response")
table(p1)


mod_CHD2=glm(CHD~FAIXA_ETARIA,data=dados_CHD, family=binomial(link="logit"))
summary(mod_CHD2)
dados_CHD$p2<-prob<-predict(mod_CHD2, type = "response")

par(mfrow=c(1,2))
plot(dados_CHD$FAIXA_ETARIA,dados_CHD$p2,  ylab="Faixa etária", xlab="Probabilidade predita (CHD)")
plot(prop_tab1[,2],  ylab="Faixa etária", xlab="CHD")


    
     
     
     
                 


#EXEMPLO COM DADOS DO SINASC

# Dados do Sistema de Informações sobre Nascidos vivos (SINASC) para o ano de 2014

setwd("C:/Users/expansao/Desktop/Regressao") #Define o diretório de trabalho

install.packages("read.dbc") #Pacote necessário para ler os dados do SINASC disponíveis no sitio do Datasus (http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1&acao=28&pad=31655)
library(read.dbc)

#Os dados do SINASC são disponibilizados por UF e ano de nascimento


DNRJ14 <- read.dbc("DNRJ2014.dbc")




#Índice de Apgar no 1º minuto

DNRJ14$APGAR1_2<-as.numeric(as.character(DNRJ14$APGAR1))
DNRJ14$APGAR5_2<-as.numeric(as.character(DNRJ14$APGAR5))






#Substituindo o 99 por NA
DNRJ14$APGAR1_2 [DNRJ14$APGAR1 == 99]<-NA
DNRJ14$APGAR5_2 [DNRJ14$APGAR5 == 99]<-NA



DNRJ14$PESO2<-as.numeric(as.character(DNRJ14$PESO))


#Substituindo o 99 por NA
DNRJ14$PESO2 [DNRJ14$PESO == 99]<-NA



# Idade gestacional

DNRJ14$SEMAGESTAC2<-as.numeric(as.character(DNRJ14$SEMAGESTAC))

#Parto prematuro (<37 semanas)
DNRJ14$PREMATUR [DNRJ14$SEMAGESTAC2 < 37]<-1
DNRJ14$PREMATUR [DNRJ14$SEMAGESTAC2 >= 37]<-2



a_sinasc_1000 <- DNRJ14[sample(seq_len(nrow(DNRJ14)),size=1000,replace=F),]



#Modelo de regressão linear 1 (Peso ao nascer X Idade gestacional)

plot(a_sinasc_1000$SEMAGESTAC2,a_sinasc_1000$PESO2,  ylab="Peso ao nascer (g)", xlab="Idade Gestacional (semanas)")


mod1<-lm(PESO2 ~ SEMAGESTAC2, a_sinasc_1000)
summary(mod1)


plot(a_sinasc_1000$SEMAGESTAC2,a_sinasc_1000$PESO2,  ylab="Peso ao nascer (g)", xlab="Idade Gestacional (semanas)")
abline(mod1,lty=1)

predict(mod1, data.frame(SEMAGESTAC2=c(22,37,42)))



#Modelo Multivariado 

mod2<-lm(PESO2 ~ SEMAGESTAC2 + APGAR5_2, a_sinasc_1000)
summary(mod2)






