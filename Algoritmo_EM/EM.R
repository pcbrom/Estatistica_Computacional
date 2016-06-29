##################################################################################
# ESTATISTICA COMPUTACIONAL
# TP5 - ALGORITMO EM
# RESOLUCAO: PEDRO CARVALHO BROM
# MATRICULA: 140077567
##################################################################################

##################################################################################
# ARRUMANDO O ESPACO DE TRABALHO
##################################################################################

# --------------------------------------------------------------------------------

# LIMPAR MEMORIA

rm(list = ls(all = T))

# DEFINIR A SEMENTE

set.seed(140077456)

# DEFINIR AREA DE TRABALHO E LOCAL DO ARQUIVO

setwd("/home/pcbrom/Dropbox/Trabalho e Estudo/Estatistica/Estatística Computacional/TP5-Resolucao")
loc = "mistura.txt"

# CARREGAR O ARQUIVO

db = read.table(loc, sep = "" , header = F, na.strings = "", stringsAsFactors = F)

# VERIFICANDO A INTEGRIDADE DOS DADOS

# estrutura dos dados 
str(db)

# verificando se existe linhas incompletas
dim(db)[1] == sum(complete.cases(db))

# --------------------------------------------------------------------------------

# avaliação grafica preliminar
hist(db$V1, freq = F, xlim = c(0,30), ylim = c(0,0.13), xlab = "",
     main = "Histograma de densidade e curva de núcleo")

# pacote para curva de nucleo
library(KernSmooth)

# suavizacao da curva de nucleo
fhat = bkde(x = db$V1)

# avaliacao visual da curva de nucleo
points(fhat$x, fhat$y, xlab = "", ylab = "", main = "", col = "blue"
     , cex = 0.5, las = 2, xlim = c(0,30), ylim = c(0,0.13))

# valor inicial medias e sds

# distribuicao 1
mu1 = fhat$x[which(max(fhat$y) == fhat$y)]
sd1 = sd(fhat$x[272:401])/2

# distribuicao 2
fhat1 = fhat$y[1:200]
mu2 = fhat$x[which(max(fhat1) == fhat$y)]
sd2 = sd(fhat$x[1:200])

mu1; sd1; mu2; sd2

# --------------------------------------------------------------------------------

# valores iniciais

ghat = function(x, mu1h, sd1h, mu2h, sd2h, p) {
  p*dnorm(x, mu2h, sd2h)/((1 - p)*dnorm(x, mu1h, sd1h) + p*dnorm(x, mu2h, sd2h))
}

ep = 10^(-10)
mu1h = mu1; mu2h = mu2; sd1h = sd1; sd2h = sd2; p = phat = 0.5
mu1ha = mu2ha = sd1ha = sd2ha = phata = 0

while (abs(mu1ha - mu1h) > ep && abs(mu2ha - mu2h) > ep &&
       abs(sd1ha - sd1h) > ep && abs(sd2ha - sd2h) > ep &&
       abs(phata - phat) > ep) {
  
  valor = ghat(x = db$V1, mu1h = mu1h, sd1h = sd1h, mu2h = mu2h, sd2h = sd2h, p = phat)
  mu1h = sum((1 - valor)*db$V1)/sum(1 - valor); mu1ha = mu1h
  mu2h = sum(valor*db$V1)/sum(valor); mu2ha = mu2h
  sd1h = sqrt(sum((1 - valor)*(db$V1 - mu1h)^2)/sum(1 - valor)); sd1ha = sd1h
  sd2h = sqrt(sum(valor*(db$V1 - mu2h)^2)/sum(valor)); sd2ha = sd2h
  phat = sum(valor)/length(db$V1); phata = phat
  
}

Calc_d1 = c(mu1h, sd1h, (1 - phat))
Calc_d2 = c(mu2h, sd2h, phat)

# --------------------------------------------------------------------------------

# comparando os resultados com funcao do pacote mixtools

require(mixtools)
myEM = normalmixEM(db$V1, mu = c(mu1, mu2), sigma = c(sd1, sd2))
resplib = cbind(rbind(myEM$mu, myEM$sigma, myEM$lambda), Calc_d1, Calc_d2)
rownames(resplib) = c("mu", "sd", "phat"); colnames(resplib) = c("Lib_d1", "Lib_d2", "Calc_d1", "Calc_d2")
resplib

# --------------------------------------------------------------------------------

# verificando as distribuições simuladas com a original

# calculada

n1 = round(Calc_d1[3]*10000)
n2 = 10000 - n1

d1c = rnorm(n1, Calc_d1[1], Calc_d1[2])
d2c = rnorm(n2, Calc_d2[1], Calc_d2[2])
d = append(d1c, d2c)

# pelo pacote

n3 = round(myEM$lambda[1]*10000)
n4 = 10000 - n3

d1m = rnorm(n3, myEM$mu[1], myEM$sigma[1])
d2m = rnorm(n4, myEM$mu[2], myEM$sigma[2])
dm = append(d1m, d2m)

# grafico

plot.ecdf(db$V1, xlim = c(0,30), main = "Comparação entre distribuições"
          , xlab = "Preta: original; Vermelha: algoritmo; Verde: mixtools")
par(new = T)
plot.ecdf(d, col = "red", xlim = c(0,30), xlab = "", ylab = "", main = "")
par(new = T)
plot.ecdf(dm, col = "green", xlim = c(0,30), xlab = "", ylab = "", main = "")

# kstest

dhat = bkde(d); dmhat = bkde(dm)
ks.test(fhat$y, dhat$y); ks.test(fhat$y, dmhat$y)
