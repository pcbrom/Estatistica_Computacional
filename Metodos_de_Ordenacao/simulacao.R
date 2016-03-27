##################################################################################
# ESTATISTICA COMPUTACIONAL
# TP1 - METODOS DE ORDENACAO
# RESOLUCAO: PEDRO CARVALHO BROM
# MATRICULA: 140077567
##################################################################################

# LIMPAR MEMÓRIA
rm(list = ls(all = T))

# CONDICOES DAS MATRIZES DE RETORNO

s.inser = NULL
s.selec = NULL
s.qsort = NULL

a = c(500, 1000, 3000, 7000, 15000) # tamanho dos vetores que serao praticados
n = 50 # quantidade de simulacoes

##################################################################################
# MATERIAIS
##################################################################################

# Definindo a semente e o vetor base para amostragem

set.seed(140077456); vector.db = seq(1:100000)

# Definindo os vetores ordenados do experimento

vetor.ord.cres = function(tamanho) {
  
  v.point = round(runif(1, min = vector.db[1], max = vector.db[50001]))
  v.ord.cres = vector.db[v.point:(v.point + tamanho - 1)]
  
  return(v.ord.cres)
  
}

vetor.ord.decres = function(tamanho) {
  
  v.point = round(runif(1, min = vector.db[1], max = vector.db[50001]))
  v.ord.decres = vector.db[(v.point + tamanho - 1):v.point]
  
  return(v.ord.decres)
  
}

# INVOCAR FUNCOES COM: vetor.ord.cres(tamanho) e vetor.ord.decres(tamanho)


# Definindo os vetores aleatorizados do experimento

vetor.aleat = function(tamanho) {
  
  return(sample(vector.db, tamanho))
  
}

# INVOCAR FUNCAO COM: vetor.aleat(tamanho)


# Funcao para simulacao

average.simula = function(a, n, tempo, metodo, ordenamento) {
  
  contador = 1
  
  for (i in 1:n) {
    
    time.i = Sys.time()
    resp = metodo(ordenamento(a))
    time.f = Sys.time() - time.i
    tempo = append(tempo, time.f)
    resp = rbind(resp, resp)
    
    txt = paste0("cotador -> ", i)
    
    contador = contador + 1
    
  }
  
  tempo.m = mean(tempo)
  resp = colMeans(resp)
  
  names(resp) = c("N. medio de comp.", "N. medio de mov.")
  return(c("Tempo medio (seg)" = tempo.m, resp))
  
}

# INVOCAR FUNCAO COM: average.simula(a = 500, n = 100, tempo = NULL, metodo, ordenamento)
# a := tamanho do vetor amostra
# n := Quantidade de simulacoes
# tempo := Tempo medio das simulacoes
# metodo := Metodo de ordenacao
# ordenamento := Tipo de ordenamento do vetor

##################################################################################
# ORDENACAO POR INSERCAO
##################################################################################

o.insercao = function(x) {
  
  n.mov = 0
  n.comp = 0
  
  for(i in 2:(length(x))) {
    
    value = x[i]
    j = i - 1
    
    while(j > 0 && x[j] > value) {
      
      x[j + 1] = x[j]
      j = j - 1
      n.comp = n.comp + 1
      
      print(paste0("insercao -> ", i))
      
    }
    
    x[j + 1] = value
    n.comp = n.comp + 1
    n.mov = n.comp - 2
    
  }
  
  n.mov.comp = c("n.mov" = n.mov, "n.comp" = n.comp)
  return(n.mov.comp)
  
}

# movimentos OK
# tempo OK
# comparacoes OK

# INVOCAR FUNCAO COM: o.insercao(vetor)

# Simulacao para vetor ordenado crescente

time.simula = Sys.time() # Inicio da contagem de tempo da simulacao

sim.inser.cres = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.insercao, ordenamento = vetor.ord.cres)))
    rownames(res) = paste("s.inser.cres.", a[i], sep = "")
    s.inser = rbind(s.inser, res)
    
  }
  
  return(s.inser)
  
}
#sim.inser.cres.resp = sim.inser.cres()

# Simulacao para vetor ordenado decrescente

sim.inser.decres = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.insercao, ordenamento = vetor.ord.decres)))
    rownames(res) = paste("s.inser.decres.", a[i], sep = "")
    s.inser = rbind(s.inser, res)
    
  }
  
  return(s.inser)
  
}
#sim.inser.decres.resp = sim.inser.decres()

# Simulacao para vetor aleatorizado

sim.inser.aleat = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.insercao, ordenamento = vetor.aleat)))
    rownames(res) = paste("s.inser.aleat.", a[i], sep = "")
    s.inser = rbind(s.inser, res)
    
  }
  
  return(s.inser)
  
}
sim.inser.aleat.resp = sim.inser.aleat()

# Matriz de retorno para o metodo de ordenacao por insercao

#s.inser = rbind(sim.inser.cres.resp, sim.inser.decres.resp, sim.inser.aleat.resp)
s.inser = sim.inser.aleat.resp

time.inser = Sys.time() - time.simula # Fim da contagem de tempo da simulacao


##################################################################################
# ORDENACAO POR SELECAO
##################################################################################

o.selecao = function(x) {
  
  n.mov = 0
  n.comp = 0
  
  for(i in 1:length(x)) {
    
    min.i = (i - 1) + which.min(x[i:length(x)])
    n.comp = n.comp + length(i:length(x)) - 1
    
    start_ = seq_len(i - 1)
    
    x = c(x[start_], x[min.i], x[-c(start_, min.i)])
    
    print(paste0("selecao -> ", i))
      
  }
  
  n.mov = 3*(length(x) - 1)
  n.mov.comp = c("n.mov" = n.mov, "n.comp" = n.comp)
  return(n.mov.comp)
  
}

# movimentos OK
# tempo OK
# comparacoes OK

# Simulacao para vetor ordenado crescente

time.simula = Sys.time() # Inicio da contagem de tempo da simulacao

sim.selec.cres = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.selecao, ordenamento = vetor.ord.cres)))
    rownames(res) = paste("s.selec.cres.", a[i], sep = "")
    s.selec = rbind(s.selec, res)
    
  }
  
  return(s.selec)
  
}
#sim.selec.cres.resp = sim.selec.cres()

# Simulacao para vetor ordenado decrescente

sim.selec.decres = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.selecao, ordenamento = vetor.ord.decres)))
    rownames(res) = paste("s.selec.decres.", a[i], sep = "")
    s.selec = rbind(s.selec, res)
    
  }
  
  return(s.selec)
  
}
#sim.selec.decres.resp = sim.selec.decres()

# Simulacao para vetor aleatorizado

sim.selec.aleat = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.selecao, ordenamento = vetor.aleat)))
    rownames(res) = paste("s.selec.aleat.", a[i], sep = "")
    s.selec = rbind(s.selec, res)
    
  }
  
  return(s.selec)
  
}
sim.selec.aleat.resp = sim.selec.aleat()

# Matriz de retorno para o metodo de ordenacao por selecao

#s.selec = rbind(sim.selec.cres.resp, sim.selec.decres.resp, sim.selec.aleat.resp)
s.selec = sim.selec.aleat.resp

time.selec = Sys.time() - time.simula # Fim da contagem de tempo da simulacao


##################################################################################
# ORDENACAO POR QUICKSORT
##################################################################################

o.qsort = function(v) {
  
  n.mov = 0
  n.comp = 0
  
  if (length(v) <= 1) {
    
    return(v)
    
  } else {
    
    less = NULL
    pivotList = NULL
    more = NULL
    pivot = median(v)
    
    for (i in 1:length(v)) {
      
      if (v[i] < pivot) {
        
        less = append(less, v[i])
        n.comp = n.comp + 1
        n.mov = n.mov + 1
        
        print(paste0("qsort -> ", i))
        
      } else if (v[i] > pivot) {
        
        more = append(more, v[i])
        n.comp = n.comp + 2
        n.mov = n.mov + 1
        
        print(paste0("qsort -> ", i))
        
      } else {
        
        n.comp = n.comp + 2
        pivotList = append(pivotList, v[i])
        n.mov = n.mov + 1
        
        print(paste0("qsort -> ", i))
        
      }
      
    }
      
    less = o.qsort(less)
    more = o.qsort(more)
    
  }
  
  resp = c(less, pivotList, more)
  n.mov.comp = c("n.mov" = n.mov, "n.comp" = n.comp)
  return(n.mov.comp)
  
}

# movimentos OK
# tempo OK
# comparacoes OK

# 1.386*100*log(100) - 0.846*100

# Simulacao para vetor ordenado crescente

time.simula = Sys.time() # Inicio da contagem de tempo da simulacao

sim.qsort.cres = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.qsort, ordenamento = vetor.ord.cres)))
    rownames(res) = paste("s.qsort.cres.", a[i], sep = "")
    s.qsort = rbind(s.qsort, res)
    
  }
  
  return(s.qsort)
  
}
#sim.qsort.cres.resp = sim.qsort.cres()

# Simulacao para vetor ordenado decrescente

sim.qsort.decres = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.qsort, ordenamento = vetor.ord.decres)))
    rownames(res) = paste("s.qsort.decres.", a[i], sep = "")
    s.qsort = rbind(s.qsort, res)
    
  }
  
  return(s.qsort)
  
}
#sim.qsort.decres.resp = sim.qsort.decres()

# Simulacao para vetor aleatorizado

sim.qsort.aleat = function(){
  
  for(i in 1:length(a)) { 
    
    res = t(as.matrix(average.simula(a = a[i], n = n, tempo = NULL, metodo = o.qsort, ordenamento = vetor.aleat)))
    rownames(res) = paste("s.qsort.aleat.", a[i], sep = "")
    s.qsort = rbind(s.qsort, res)
    
  }
  
  return(s.qsort)
  
}
sim.qsort.aleat.resp = sim.qsort.aleat()

# Matriz de retorno para o metodo de ordenacao por selecao

#s.qsort = rbind(sim.qsort.cres.resp, sim.qsort.decres.resp, sim.qsort.aleat.resp)
s.qsort = sim.qsort.aleat.resp

time.qsort = Sys.time() - time.simula # Fim da contagem de tempo da simulacao


##################################################################################
# RESULTADOS
##################################################################################

time.inser; time.selec; time.qsort

metodo = c(rep("Insercao", 5), rep("Selecao", 5), rep("QuickSort", 5))
#ordenamento = c(rep("Crescente", 5), rep("Decrescente", 5), rep("Aleatorizado", 5))
#ordenamento = rep(ordenamento, 3)
ordenamento = rep("Aleatorizado", 3)
comp.vetor = rep(a, 3)
#comp.vetor = rep(comp.vetor, 3)
resultados = cbind(rbind(s.inser, s.selec, s.qsort), metodo, ordenamento, comp.vetor)
resultados = as.data.frame(resultados)
resultados[, 1] = as.numeric(as.character(resultados[, 1]))
resultados[, 2] = as.numeric(as.character(resultados[, 2]))
resultados[, 3] = as.numeric(as.character(resultados[, 3]))
resultados[, 4] = as.character(resultados[, 4])
resultados[, 5] = as.character(resultados[, 5])
resultados[, 6] = as.character(resultados[, 6])

# Resultados tabelados

setwd("/home/pcbrom/Dropbox/Trabalho e Estudo/Estatistica/Estatística Computacional/TP1-Resolucao")
write.csv(resultados, "TP1_resultados.csv")

resultados = read.csv("TP1_resultados.csv")

# Resultados gráficos

require(lattice)

# Resultados gráficos
# POR tempo

png(filename = "tempo_metodo.png", width = 1200, height = 800, res = 120)
p = xyplot(resultados[, 2] ~ 1:nrow(resultados), 
       group = resultados[, 5], 
       data = resultados,
       main = list('Quadro comparativo de tempo médio de simulação', cex = 1.5),
       ylab = 'Tempo de execução em segundos', 
       xlab = paste0('Tempo de execução ao lado direito de cada ponto, em cinza', '\nTamanho dos vetores ao lado esquerdo de cada ponto, em vermelho', '\nNúmero de movimentos acima de cada ponto, em azul', '\nNúmero de comparações abaixo de cada ponto, em verde'),
       auto.key = list(title = 'Método de\nordenação', corner = c(.95, 1), cex = .8), 
       pch = 22,
       par.settings = list(superpose.symbol = list(pch = 22)))
p
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(a)[[1]], 
           pos = 2, 
           cex = 0.7, 
           col = "red")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[, 4])[[1]], 
           pos = 3, 
           cex = 0.7, 
           col = "blue")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[,3])[[1]], 
           pos = 1, 
           cex = 0.7, 
           col = "green4")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(round(resultados[, 2], 2))[[1]], 
           pos = 4, 
           cex = 0.7, 
           col = "grey1")
trellis.unfocus()
dev.off()

# POR quantidade de comparações

png(filename = "comparacoes_metodo.png", width = 1200, height = 800, res = 120)
p = xyplot(resultados[, 3] ~ 1:nrow(resultados), 
           group = resultados[, 5], 
           data = resultados,
           main = list('Quadro comparativo de quantidade média de comparações\nentre métodos da simulação', cex = 1.5),
           ylab = 'Total de comparações', 
           xlab = paste0('Tempo de execução ao lado direito de cada ponto, em cinza', '\nTamanho dos vetores ao lado esquerdo de cada ponto, em vermelho', '\nNúmero de movimentos acima de cada ponto, em azul', '\nNúmero de comparações abaixo de cada ponto, em verde'),
           #ylim = c(0, 20000),
           auto.key = list(title = 'Método de\nordenação', corner = c(.95, 1), cex = .8), 
           pch = 22,
           par.settings = list(superpose.symbol = list(pch = 22)))
p
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(a)[[1]], 
           pos = 2, 
           cex = 0.7, 
           col = "red")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[, 4])[[1]], 
           pos = 3, 
           cex = 0.7, 
           col = "blue")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[,3])[[1]], 
           pos = 1, 
           cex = 0.7, 
           col = "green4")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(round(resultados[, 2], 2))[[1]], 
           pos = 4, 
           cex = 0.7, 
           col = "grey1")
trellis.unfocus()
dev.off()

# POR quantidade de comparações ESCALA AJUSTADA

png(filename = "comparacoes_metodo_escala.png", width = 1200, height = 800, res = 120)
p = xyplot(resultados[, 3] ~ 1:nrow(resultados), 
           group = resultados[, 5], 
           data = resultados,
           main = list('Quadro comparativo de quantidade média de comparações\nentre métodos da simulação (escala de y de 0 a 70000)', cex = 1.5),
           ylab = 'Total de comparações', 
           xlab = paste0('Tempo de execução ao lado direito de cada ponto, em cinza', '\nTamanho dos vetores ao lado esquerdo de cada ponto, em vermelho', '\nNúmero de movimentos acima de cada ponto, em azul', '\nNúmero de comparações abaixo de cada ponto, em verde'),
           ylim = c(-5000, 70000),
           auto.key = list(title = 'Método de\nordenação', corner = c(.95, 1), cex = .8), 
           pch = 22,
           par.settings = list(superpose.symbol = list(pch = 22)))
p
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(a)[[1]], 
           pos = 2, 
           cex = 0.7, 
           col = "red")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[, 4])[[1]], 
           pos = 3, 
           cex = 0.7, 
           col = "blue")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[,3])[[1]], 
           pos = 1, 
           cex = 0.7, 
           col = "green4")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(round(resultados[, 2], 2))[[1]], 
           pos = 4, 
           cex = 0.7, 
           col = "grey1")
trellis.unfocus()
dev.off()

# POR quantidade de movimentos

png(filename = "movimentos_metodo.png", width = 1200, height = 800, res = 120)
p = xyplot(resultados[, 4] ~ 1:nrow(resultados), 
           group = resultados[, 5], 
           data = resultados,
           main = list('Quadro comparativo de quantidade média de movimentos\nentre métodos da simulação', cex = 1.5),
           ylab = 'Total de movimentos', 
           xlab = paste0('Tempo de execução ao lado direito de cada ponto, em cinza', '\nTamanho dos vetores ao lado esquerdo de cada ponto, em vermelho', '\nNúmero de movimentos acima de cada ponto, em azul', '\nNúmero de comparações abaixo de cada ponto, em verde'),
           #ylim = c(0, 20000),
           auto.key = list(title = 'Método de\nordenação', corner = c(.95, 1), cex = .8), 
           pch = 22,
           par.settings = list(superpose.symbol = list(pch = 22)))
p
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(a)[[1]], 
           pos = 2, 
           cex = 0.7, 
           col = "red")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[, 4])[[1]], 
           pos = 3, 
           cex = 0.7, 
           col = "blue")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[,3])[[1]], 
           pos = 1, 
           cex = 0.7, 
           col = "green4")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(round(resultados[, 2], 2))[[1]], 
           pos = 4, 
           cex = 0.7, 
           col = "grey1")
trellis.unfocus()
dev.off()

# POR quantidade de movimentos ESCALA AJUSTADA

png(filename = "movimentos_metodo_escala.png", width = 1200, height = 800, res = 120)
p = xyplot(resultados[, 4] ~ 1:nrow(resultados), 
           group = resultados[, 5], 
           data = resultados,
           main = list('Quadro comparativo de quantidade média de movimentos\nentre métodos da simulação (escala de y de 0 a 70000)', cex = 1.5),
           ylab = 'Total de movimentos', 
           xlab = paste0('Tempo de execução ao lado direito de cada ponto, em cinza', '\nTamanho dos vetores ao lado esquerdo de cada ponto, em vermelho', '\nNúmero de movimentos acima de cada ponto, em azul', '\nNúmero de comparações abaixo de cada ponto, em verde'),
           ylim = c(-5000, 70000),
           auto.key = list(title = 'Método de\nordenação', corner = c(.95, 1), cex = .8), 
           pch = 22,
           par.settings = list(superpose.symbol = list(pch = 22)))
p
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(a)[[1]], 
           pos = 2, 
           cex = 0.7, 
           col = "red")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[, 4])[[1]], 
           pos = 3, 
           cex = 0.7, 
           col = "blue")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(resultados[,3])[[1]], 
           pos = 1, 
           cex = 0.7, 
           col = "green4")
trellis.unfocus()
trellis.focus("panel",1,1)
panel.text(x = p$panel.args[[1]]$x, 
           y = p$panel.args[[1]]$y, 
           labels = list(round(resultados[, 2], 2))[[1]], 
           pos = 4, 
           cex = 0.7, 
           col = "grey1")
trellis.unfocus()
dev.off()
