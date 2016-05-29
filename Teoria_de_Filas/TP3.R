##################################################################################
# ESTATISTICA COMPUTACIONAL
# TP3 - SIMULACAO DE SISTEMA M/M/1
# RESOLUCAO: PEDRO CARVALHO BROM
# MATRICULA: 140077567
##################################################################################

setwd("/home/pcbrom/Dropbox/Trabalho e Estudo/Estatistica/Estatística Computacional/TP3-Resolucao")

rm(list = ls(all = T))      # LIMPAR MEMORIA
set.seed(140077456)

##################################################################################
# QUANTIDADE DE SIMULACOES
##################################################################################

# defina o valor do erro tipo I
alpha = 0.05

# total de simulacoes
E = 0.1; S = 1
qtdSimul = ceiling(qnorm(1-alpha/2)^2*S^2/E^2)
if (qtdSimul < 100) {qtdSimul = 100}
empirico = NULL; k = 1
for (k in 1:qtdSimul) {
  
  ##################################################################################
  # VALORES DE ENTRADA
  ##################################################################################
  
  # numero de clientes que entram no sistema a cada unidade de tempo
  lambda = 3/4
  # tempo entre chegadas
  Ta = 1/lambda
  # clientes atendidos em media para cada simulacao
  cms = 1000
  # duracao da simulacao
  t.final = cms/lambda
  # numero de atendimentos a cada unidade de tempo
  mu = 4/3
  # tempo de cada atendimento
  Ts = 1/mu
  # tempo total de simulacao. inicial = 0
  t.relogio = 0
  # tempo para a proxima chegada
  t1 = 0
  # tempo para a proxima partida
  t2 = t.final
  # variacao de tempo para o ultimo evento
  tn = t.relogio
  # variacao de tempo para o ultimo tempo ocioso do sistema
  tb = 0
  # numeracao do sistema
  n = 0
  # quantidade de clientes por tempo acumulado
  s = 0
  # tempo total ocioso
  b = 0
  # quantidade de conclusoes
  c = 0
  # medida instantanea de q
  qc = 0
  # variacao de tempo instantaneo
  tc = 0
  
  ##################################################################################
  # SIMULACAO
  ##################################################################################
  
  while (t.relogio < t.final) { # evento de chegada
    if (t1 < t2) {
      t.relogio = t1 # variacao do tempo ponderado na fila
      s = s + n * (t.relogio - tn)
      n = n + 1
      if (t.relogio < qtdSimul) { 
        qc = append(qc, n)
        tc = append(tc, t.relogio)
        
      }
      tn = t.relogio
      t1 = t.relogio + rexp(1, 1/Ta)
      if(n == 1) { 
        tb = t.relogio
        t2 = t.relogio + rexp(1, 1/Ts) # Período de serviço exponencial
      }
    } else { # evento de saida
      t.relogio = t2
      s = s + n * (t.relogio - tn) # variacao do tempo ponderado na fila
      n = n - 1
      if (t.relogio < qtdSimul) { 
        qc = append(qc,n)
        tc = append(tc,t.relogio)
      }
      tn = t.relogio
      c = c + 1
      if (n > 0) { 
        t2 = t.relogio + rexp(1, 1/Ts) # Período de serviço exponencial
      }
      else { 
        t2 = t.final
        b = b + t.relogio - tb
      }
    }   
  }
  
  ##################################################################################
  # RESULTADOS
  ##################################################################################
  
  # EMPIRICO
  emp = c(
    "TO" = b/t.relogio,            # taxa de ocupacao do sistema
    "TMF" = (s/t.relogio)*(1/mu),  # tempo medio na fila
    "NMCS" = s/t.relogio,          # numero medio de clientes no sistema
    "NMCF" = c/t.relogio,          # numero medio de clientes na fila
    "TMS" = s/c                    # tempo medio no sistema
  )
  
  empirico = rbind(empirico, emp)
  k = k + 1
  
}

# TEORICO
teo = c(
  "TO" = lambda/mu,                       # taxa de ocupacao do sistema
  "TMF" = (lambda/mu)/(mu*(1-lambda/mu)), # tempo medio na fila
  "NMCS" = (lambda/mu)/(1-lambda/mu),     # numero medio de clientes no sistema
  "NMCF" = (lambda/mu)^2/(1-lambda/mu),   # numero medio de clientes na fila
  "TMS" = 1/(mu-lambda)                   # tempo medio no sistema
)

# verificando a qualidade das estimativas
emp.m = apply(empirico, 2, mean)
emp.sd = apply(empirico, 2, sd)
emp.ic.i = emp.m - qnorm(1-alpha/2)*emp.sd
emp.ic.s = emp.m + qnorm(1-alpha/2)*emp.sd
erro.rel = (teo - emp.m)/teo
erro.pad = emp.sd/sqrt(dim(empirico)[1])

# imprimir na tela a tabela de resultados
final = cbind(teo, emp.ic.i, emp.m, emp.ic.s, erro.rel, erro.pad)
print(final)
write.table(final, "final.csv", dec = ",", sep = "\t")
