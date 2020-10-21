install.packages("ggplot2")
library(ggplot2)
attach(Luis.TCC)


#Boxplots simples
qplot(Mobilidade.Direito, Angulo.Tub.Dir, geom="boxplot", 
      xlab="Mobilidade Direito", ylab="Ângulo Tubérculo Direito")
qplot(Pos.Disco.Dir, Angulo.Tub.Dir, geom="boxplot",
      xlab="Posição do Disco Direito", ylab="Ângulo Tubérculo Direito")
qplot(Fun.Disco.Dir, Angulo.Tub.Dir, geom="boxplot",
      xlab="Função do Disco Direito", ylab="Ângulo Tubérculo Direito")

qplot(Mobilidade.Esquerdo, Angulo.Tub.Esq, geom="boxplot", 
      xlab="Mobilidade Esquerdo", ylab="Ângulo Tubérculo Esquerdo")
qplot(Pos.Disco.Esq, Angulo.Tub.Esq, geom="boxplot",
      xlab="Posição do Disco Esquerdo", ylab="Ângulo Tubérculo Esquerdo")
qplot(Func.Disco.Esq, Angulo.Tub.Esq, geom="boxplot",
      xlab="Função do Disco Esquerdo", ylab="Ângulo Tubérculo Esquerdo")

#Boxplots complexos
qplot(Mobilidade.Direito, Angulo.Tub.Dir, geom="boxplot", colour=Pos.Disco.Dir,
      xlab="Mobilidade Direito", ylab="Ângulo Tubérculo Direito")
qplot(Pos.Disco.Dir, Angulo.Tub.Dir, geom="boxplot", colour=Mobilidade.Direito,
      xlab="Posição do Disco Direito", ylab="Ângulo Tubérculo Direito")

qplot(Mobilidade.Esquerdo, Angulo.Tub.Esq, geom="boxplot", colour=Pos.Disco.Esq,
      xlab="Mobilidade Esquerdo", ylab="Ângulo Tubérculo Esquerdo")
qplot(Pos.Disco.Esq, Angulo.Tub.Esq, geom="boxplot", colour=Mobilidade.Esquerdo,
      xlab="Posição do Disco Esquerdo", ylab="Ângulo Tubérculo Esquerdo")



qplot(Angulo.Tub.Dir, geom="histogram",
      xlab="Ângulo do Tubérculo Direito", ylab="Número de amostras")
qplot(Angulo.Tub.Dir, geom="density",
      xlab="Ângulo do Tubérculo Direito", ylab="Número de amostras")

qplot(Angulo.Tub.Esq, geom="histogram",
      xlab="Ângulo do Tubérculo Esquerdo", ylab="Número de amostras")
qplot(Angulo.Tub.Esq, geom="density",
      xlab="Ângulo do Tubérculo Esquerdo", ylab="Número de amostras")

qplot(Volumetria.Direito, geom="histogram",
      xlab="Volumetria Direito", ylab="Número de amostras")
qplot(Volumetria.Direito, geom="density",
      xlab="Volumetria Direito", ylab="Número de amostras")

qplot(Volumetria.Esquerdo, geom="histogram",
      xlab="Volumetria Esquerdoo", ylab="Número de amostras")
qplot(Volumetria.Esquerdo, geom="density",
      xlab="Volumetria Esquerdo", ylab="Número de amostras")

pairs(~Pos.Disco.Dir + Angulo.Tub.Dir + Volumetria.Direito)
qplot(Angulo.Tub.Dir,Volumetria.Direito)
qplot(IDADE,Volumetria.Direito)

summary(Luis.TCC)


#Método de Spearman
cor.test(Angulo.Tub.Dir,Volumetria.Direito,method="spearman")

#Regressão Linear
library(MASS)
install.packages("ISLR")
library(ISLR)
names(Luis.TCC)
attach(Luis.TCC)
lm.fit =lm(Angulo.Tub.Dir~Volumetria.Direito, data=Luis.TCC)
summary(lm.fit)
summary(lm.fit)$r.sq #gives the R²
summary(lm.fit)$sigma #Residual Squared Errors (RSE)
names(lm.fit)
coef(lm.fit)
confint (lm.fit) #confidence intervals
plot(Angulo.Tub.Dir,Volumetria.Direito)
abline(lm.fit)


#Removendo outliers
outliers <- boxplot(Luis.TCC$Volumetria.Direito)$out # You can get the actual values of the outliers with this
print(outliers)
out <- Luis.TCC[-which(Luis.TCC$Volumetria.Direito %in% outliers),]
attach(out)
plot(Angulo.Tub.Dir,Volumetria.Direito)


#Again...
lm.fit =lm(Angulo.Tub.Dir~Volumetria.Direito, data=out)
summary(lm.fit)
summary(lm.fit)$r.sq #gives the R²
summary(lm.fit)$sigma #Residual Squared Errors (RSE)
names(lm.fit)
coef(lm.fit)
confint (lm.fit) #confidence intervals
plot(Angulo.Tub.Dir,Volumetria.Direito)
abline(lm.fit)