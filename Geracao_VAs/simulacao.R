##################################################################################
# ESTATISTICA COMPUTACIONAL
# TP2 - GERACAO DE VARIVEIS ALEATORIAS
# RESOLUCAO: PEDRO CARVALHO BROM
# MATRICULA: 140077567
##################################################################################

setwd("/home/pcbrom/Dropbox/Trabalho e Estudo/Estatistica/Estatística Computacional/TP2-Resolucao")

# LIMPAR MEMÓRIA
rm(list = ls(all = T))
set.seed(140077456)

##################################################################################
# Gerador de variaveis aleatorias uniformes
##################################################################################

# m, c, a argumentos de aritmetica modular
# n = tamanho da amostra

rng.unif = function(a, c, m, n) {
  v = NULL
  for (i in 1:n) {
    rng.seed = rng.seed
    rng.seed = (a*rng.seed + c) %% m
    rng.unif = rng.seed/m
    v = append(v, rng.unif)  
  }
  return(v)
}

# Defina a semente
# rng.seed = (as.numeric(Sys.time()) - floor(as.numeric(Sys.time()))) * 1e8
rng.seed = 140077456

# Invocar a funcao
m = 2^32
a = 1103515245
c = 12345
n = 10^5
# testando o gerador de v.a. uniforme
u = rng.unif(m = m, a = a, c = c, n = n)

# metricas
media.u = mean(u); media.u.real = 0.5
vies.u = media.u - media.u.real
variancia.u = var(u); variancia.u.real = 1/12
eqm.u = variancia.u + vies.u^2
se.u = sd(u)
list(medias = c("media.u" = media.u, "media.u.real" = media.u.real, "vies.u" = vies.u),
     variancias = c("variancia.u" = variancia.u, "variancia.u.real" = variancia.u.real),
     eqm.u_e_se.u = c("eqm.u" = eqm.u, "se.u" = se.u))

# confirmacao visual
hist(u, col = "darkslategray3")

plot(ecdf(u), xlim = c(0, 1), main = "", lty = 1, lwd = 2)
par(new = T)
plot(punif, col = "red", xlab = "", ylab = "", xlim = c(0, 1),
     lty = 2, lwd = 2, main = "FDA teórica e empírica - Uniforme")
legend("topleft", legend = c("teórica", "empírica"), lty = 1:2, lwd = 2, inset = 0.02)

##################################################################################
# Gerador de variaveis aleatorias de normal paadrao pelo metodo polar
##################################################################################

# n = tamanho da amostra
# mu = media
# sigma = desvio padrao

rng.norm = function(n, mu, sigma) {
  
  k = z0 = numeric(1)
  z1 = NULL
  
  while (k < n) {
    
    u1 = runif(1)
    u2 = runif(1)
    
    z0 = sqrt(-2.0 * log(u1)) * cos(2*pi*u2)
    z1 = append(z1, z0)
    k = k + 1
    
  }

  return(z1)
  
}

# testando o gerador de v.a. normal
# Invocar a funcao

N = rng.norm(n = 5000, mu = 0, sigma = 1)

# metricas
media.N = mean(N); media.N.real = mu
vies.N = media.N - media.N.real
variancia.N = var(N); variancia.N.real = sigma^2
eqm.N = variancia.N + vies.N^2
se.N = sd(N)
list(medias = c("media.N" = media.N, "media.N.real" = media.N.real, "vies.N" = vies.N),
     variancias = c("variancia.N" = variancia.N, "variancia.N.real" = variancia.N.real),
     eqm.N_e_se.N = c("eqm.N" = eqm.N, "se.N" = se.N),
     teste_de_normalidade = shapiro.test(N))

# confirmacao visual
hist(N, col = "darkslategray3", xlim = c(-4, 4))
par(new = T)
curve(dnorm, col = "red", xlab = "", ylab = "", axes = F, xlim = c(-4, 4),
      lty = 2, lwd = 2)
legend("topright", col = "red", legend = c("normal padrão"), lty = 2, lwd = 2, inset = 0.02)

plot(ecdf(N), xlim = c(-4, 4), main = "", lty = 1, lwd = 2)
par(new = T)
plot(pnorm, col = "red", xlab = "", ylab = "", xlim = c(-4, 4),
     lty = 2, lwd = 2, main = "FDA teórica e empírica - Normal padrão")
legend("topleft", legend = c("teórica", "empírica"), lty = 1:2, lwd = 2, inset = 0.02)


##################################################################################
# tabela de probabilidades acumuladas da variável normal padrão utilizando 
# (1) método de integração de Monte Carlo e 
# (2) através da probabilidade acumulada obtida pela 
#     amostra gerada pelo método polar
##################################################################################

# (1) método de integração de Monte Carlo

g = function(x) (1/(sqrt(2*pi)*1))*exp(-1/2*((x-0)/1)^2) * (x < k)
f = function(x) dt(x, 120)

# Importance Sampling
m = 10^6
theta.hat = numeric(1)
g = function(x) (1/(sqrt(2*pi)*1))*exp(-1/2*((x-0)/1)^2) * (x < k)

# Usando f: t com v = 120

z.MC = function(k) {
  x = rt(m, 120) 
  fg = g(x)/(f(x))
  theta.hat = mean(fg)
  return(theta.hat)
}

z.row = seq(from = 0, to = 3.9, by = 0.1)
z.col = seq(from = 0, to = 9, by = 1)/100

tabela.z.MC.emp = matrix(0, nrow = length(z.row), ncol = length(z.col))
rownames(tabela.z.MC.emp) = z.row; colnames(tabela.z.MC.emp) = z.col

for (i in 1:length(z.row)) {
  
  for (j in 1:length(z.col)) {
    
    k = z.row[i] + z.col[j]
    tabela.z.MC.emp[i,j] = z.MC(k)
    
  }

}


# (2) através da probabilidade acumulada obtida pela 
#     amostra gerada pelo método polar

N = rng.norm(n = 5000, mu = 0, sigma = 1)
z.row = seq(from = 0, to = 3.9, by = 0.1)
z.col = seq(from = 0, to = 9, by = 1)/100

z.N = ecdf(N)

# funcao da normal empirica
z.N.emp = function(a, b) {
  
  k = z.row[a] + z.col[b]
  l = z.N(k)
  return(l)
  
}

# funcao da normal padrao
z = function(a, b) {
  
  k = z.row[a] + z.col[b]
  l = pnorm(k)
  return(l)
  
}

tabela.N = matrix(0, nrow = length(z.row), ncol = length(z.col))
rownames(tabela.N) = z.row; colnames(tabela.N) = z.col
for (i in 1:length(z.row)) {for (j in 1:length(z.col)) {tabela.N[i,j] = z(i,j)}}

tabela.z.N.emp = matrix(0, nrow = length(z.row), ncol = length(z.col))
rownames(tabela.z.N.emp) = z.row; colnames(tabela.z.N.emp) = z.col
for (i in 1:length(z.row)) {for (j in 1:length(z.col)) {tabela.z.N.emp[i,j] = z(i,j)}}

write.table(tabela.N, "tabela.z.N.emp.txt", sep = ";", dec = ".")
write.table((tabela.N - tabela.z.N.emp)/tabela.N, "tabela.erro.rel.z.N.emp.txt", sep = ";", dec = ".")
write.table(tabela.z.MC.emp, "tabela.z.MC.emp.txt", sep = ";", dec = ".")
write.table((tabela.N - tabela.z.MC.emp)/tabela.N, "tabela.erro.rel.z.MC.emp.txt", sep = ";", dec = ".")
