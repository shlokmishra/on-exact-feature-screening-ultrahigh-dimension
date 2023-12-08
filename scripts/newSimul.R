rm(list = ls())
#------------------------------------------------------------------------------
set.seed(123)
REP = 100 # number of iterations to run.
trn.sz.perClass = 100 # size of training sample from each class in all simulated examples Ex 1- 16 except Ex 8
tst.sz.perClass = 250 # size of test sample from each class in all simulated examples
# OS.var = readline(prompt = 'If you are on Windows, please type "W". Otherwise type "O": ')
# cluster.type = ifelse(OS.var == 'W', 'PSOCK', 'FORK')
cluster.type = 'PSOCK'
#------------------------------------------------------------------------------
EXNAMES = paste('ex', c(1,2,4,5,6,7,8), sep = '')
#------------------------------------------------------------------------------
PCKG = c(
  'energy',
  'proxy',
  'parallelDist',
  'nbpMatching',
  'MASS',
  'doParallel',
  'plot.matrix',
  'RcppArmadillo',
  'RcppXPtrUtils',
  'inline',
  'VariableScreening',
  'utils',
  'e1071',
  'Peacock.test',
  'ks'
)

install.packages(setdiff(PCKG, rownames(installed.packages())))
lapply(PCKG, library, character.only = T)

#------------------------------------------------------------------------------
source('relevant_functions.R')

#main script starts here.

no.cores = round(detectCores() - 1)
cl = makeCluster(no.cores, type = cluster.type)
registerDoParallel(cl)
clusterExport(cl, as.character(lsf.str())) # exporting all user defined functions
i = 1
flg = 1
RESULT = list(0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0)
names(RESULT) = EXNAMES
out = NULL
pb = ERR = NULL
gamfuns = c('gexp', 'gsqrt', 'glog') #choice of gamma functions
d = 10 #dimension
revmu = 0.75 # the difference in mean for the marginal signal in examples 9-16

# exID = EXNAMES[1]

for (exID in EXNAMES) {
  switch (
    exID,
    ex1 = { #example 1 from the manuscript. A location problem with marginal features.
      block.size = 2
      s = 2
      no.s = 4 #4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex2 = { #example 2 from the manuscript. A scale problem with paired features
      block.size = 2
      s = 2
      no.s = 4 #2 pairs
      sig.pos = c(1, 3) 
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex3 = { #example 3 from the manuscript. A location-scale problem with marginal and paired features
      block.size = 2
      s = 2
      no.s = 3 # 1 pair 1 marginal
      sig.pos = c(1, 3) 
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex4 = { #example 4 from the manuscript. A scale problem with marginal features
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex5 = { #example 5 from the manuscript. A heavy-tailed location problem with marginal features
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) 
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex6 = { #example 6 from the manuscript. A heavy-tailed scale problem with marginal features
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex7 = { #example 7 from the manuscript. A location-scale problem with mixture distributions having marginal features 
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex8 = { #example 8 from the manuscript. A scale problem with marginal features 
      block.size = 2
      s = 2
      no.s = 2 # 2 pairs
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = 20
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex9 = { #example 9 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 2
      s = 2
      no.s = 3 #1 marginal 1 pair
      sig.pos = 1:2 #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaI.1.1 = constcor.mat(d = 2, rho = 0.9)
      SigmaI.1.2 = constcor.mat(d = 2, rho = -0.9)
    },
    
    ex10 = { #example 10 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 2
      s = 2
      no.s = 3 #1 marginal 1 pair
      sig.pos = 1:2 #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaI.2.1 = constcor.mat(d = 2, rho = 0.9)
      SigmaI.2.2 = constcor.mat(d = 2, rho = -0.9)
      SigmaI.2.N = constcor.mat(d = d - 3, rho = 0.9)
    },
    
    ex11 = { #example 11 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 3
      s = 1
      no.s = s * block.size
      sig.pos = seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaI.2.5.1 = constcor.mat(d = 3, rho = 0.9)
      SigmaI.2.5.1[1, -1] = SigmaI.2.5.1[-1, 1] = 0.5
      
      SigmaI.2.5.2 = constcor.mat(d = 3, rho = -0.4)
      SigmaI.2.5.2[1, -1] = SigmaI.2.5.2[-1, 1] = 0.5
      
      SigmaI.2.5.N = diag(d - 3)
      
      # w1 = eigen(SigmaI.2.5.1); range(w1$values)
      # w2 = eigen(SigmaI.2.5.2); range(w2$values)
    },
    
    ex12 = {  #example 12 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 3
      s = 1
      no.s = s * block.size
      sig.pos = seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaI.3.1 = constcor.mat(d = 3, rho = 0.9)
      SigmaI.3.1[1, -1] = SigmaI.3.1[-1, 1] = 0.5
      
      SigmaI.3.2 = constcor.mat(d = 3, rho = -0.4)
      SigmaI.3.2[1, -1] = SigmaI.3.2[-1, 1] = 0.5
      
      SigmaI.3.N = constcor.mat(d = d - 3, rho = 0.9)
      
      # w1 = eigen(SigmaI.3.1); range(w1$values)
      # w2 = eigen(SigmaI.3.2); range(w2$values)
    },
    
    ex13 = { #example 13 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 2
      s = 2
      no.s = 3 # 1 mar 1 pair #no.s = s * block.size
      sig.pos = 1:2 #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaII.2.1 = SigmaII.2.2 = constcor.mat(d = d, rho = 0.25)
      
      SigmaII.2.1[1, 2] = SigmaII.2.1[2, 1] = SigmaII.2.1[1, 3] = SigmaII.2.1[3, 1] = 0
      SigmaII.2.2[1, 2] = SigmaII.2.2[2, 1] = SigmaII.2.2[1, 3] = SigmaII.2.2[3, 1] = 0
      SigmaII.2.1[2, 3] = SigmaII.2.1[3, 2] = 0.9
      SigmaII.2.2[2, 3] = SigmaII.2.2[3, 2] = -0.25
      # w1 = eigen(SigmaII.2.1); range(w1$values)
      # w2 = eigen(SigmaII.2.2); range(w2$values)
    },
    
    ex14 = { #example 14 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 3
      s = 1
      no.s = s * block.size
      sig.pos = seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaII.5.1 = SigmaII.5.2 = autocor.mat(d = d, rho = 0.25)
      
      SigmaII.5.1[2:3, 2:3] = constcor.mat(d = 2, rho = 0.9)
      
      SigmaII.5.2[2:3, 2:3] = constcor.mat(d = 2, rho = -0.9)
      
      
      # w1 = eigen(SigmaII.5.1); range(w1$values)
      # w2 = eigen(SigmaII.5.2); range(w2$values)
    },
    
    ex15 = { #example 15 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 3
      s = 1
      no.s = s * block.size
      sig.pos = seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaII.4.1 = SigmaII.4.2 = autocor.mat(d = d, rho = 0.25)
      
      SigmaII.4.1[2:3, 2:3] = constcor.mat(d = 2, rho = 0.9)
      SigmaII.4.1[-(1:3),-(1:3)] = constcor.mat(d = d - 3, rho = 0.25)
      
      SigmaII.4.2[2:3, 2:3] = constcor.mat(d = 2, rho = -0.8)
      SigmaII.4.2[-(1:3),-(1:3)] = constcor.mat(d = d - 3, rho = 0.25)
      
      # w1 = eigen(SigmaII.4.1); range(w1$values)
      # w2 = eigen(SigmaII.4.2); range(w2$values)
    },
    
    ex16 = { #example 16 from the supplementary. A location-scale problem marginal and paired features 
      block.size = 3
      s = 1
      no.s = s * block.size
      sig.pos = seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
      
      SigmaII.3.1 = SigmaII.3.2 = constcor.mat(d = d, rho = 0.25)
      
      SigmaII.3.1[2:3, 2:3] = constcor.mat(d = 2, rho = 0.9)
      SigmaII.3.2[2:3, 2:3] = constcor.mat(d = 2, rho = 0.1)
      # w1 = eigen(SigmaII.3.1); range(w1$values)
      # w2 = eigen(SigmaII.3.2); range(w2$values)
    }
  )
  clusterExport(
    cl,
    c(
      'revmu',
      'n1train',
      'n2train',
      'n',
      'ntrain',
      'n1test',
      'n2test',
      'n1',
      'n2',
      'd',
      'block.size'
    )
  )
  ptm = proc.time()
  # pb <- progress_bar$new(total = REP)
  pb <-
    txtProgressBar(
      initial = 1,
      # title = paste("Time Elapsed ", round((
      #   proc.time()[3] - ptm[3]
      # ) / 60), " Minutes"),
      min = 1,
      max = REP ,
      style = 3,
      width = REP,
      char = '='
      # label = paste(iter * 100 / REP, '% complete', sep = '')
      # paste("Time Elapsed ", round((
      #   proc.time()[3] - ptm[3]
      # ) / 60), " Minutes")
    )
  # out2 <- data.frame(listS = character(REP), listShat = character(REP), stringsAsFactors = FALSE)
  # out2$marg<- vector("list", length = REP) # Initialize a list column
  
  
  for (iter in 1:REP) {
    # marg_sig <- vector("list", REP)
    # marg_sig <- numeric(length = REP)
    # margpair_sig <- vector("list", REP)
    # correct_sig  <- vector("list", REP)
    
    switch (
      exID,
      ex1 = {
        pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop2 = cbind(matrix(rnorm(n2 * 4, mean = 1, sd = sqrt(1)), ncol = 4), matrix(rnorm(n2 * (d-4), mean = 0, sd = sqrt(1)), ncol = d-4))
        
      },
      
      ex2 = {
        pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        for (k in 1:n1) {
          for (len in 1:length(sig.pos)) {
            pop1[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
              n = 1,
              mu = c(rep(0, block.size - 1), 0) ,
              Sigma = constcor.mat(d = block.size, rho = 0.9)
            )
          }
        }
        
        for (k in 1:n2) {
          for (len in 1:length(sig.pos)) {
            pop2[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
              n = 1,
              mu = c(rep(0, block.size - 1), 0) ,
              Sigma = constcor.mat(d = block.size, rho = -0.9)
            )
          }
        }
      },
      
      ex3 = {
        pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop1[, 1:3] = foreach(k = 1:n1, .combine = rbind) %dopar% {
          c(mvrnorm(
            n = 1,
            mu = rep(0, block.size) ,
            Sigma = constcor.mat(d = block.size, rho = 0.9)
          ),
          rnorm(
            n = 1,
            mean = 1,
            sd = 1
          ))
        }
        
        pop2[, 1:2] = foreach(k = 1:n2, .combine = rbind) %dopar% {
          mvrnorm(
            n = 1,
            mu = rep(0, block.size) ,
            Sigma = constcor.mat(d = block.size, rho = -0.9)
          )
        }
      },
      
      ex4 = {
        pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop2 = cbind(matrix(rnorm(n2 * 4, mean = 0, sd = sqrt(1/3)), ncol = 4), matrix(rnorm(n2 * (d-4), mean = 0, sd = sqrt(1)), ncol = d-4))
      },
      
      ex5 = {
        pop1 = matrix(rcauchy(
          n = n1 * d,
          location = 0,
          scale = sqrt(1)
        ), ncol = d)
        
        pop2 = cbind(matrix(rcauchy(
          n = n2 * 4,
          location = 2,
          scale = sqrt(1)
        ), ncol = 4),
        matrix(rcauchy(
          n = n2 * (d-4),
          location = 0,
          scale = sqrt(1)
        ), ncol = d-4)
        )
      },
      
      ex6 = {
        pop1 = matrix(rcauchy(
          n = n1 * d,
          location = 0,
          scale = sqrt(1)
        ), ncol = d)
        
        pop2 = cbind(matrix(rcauchy(
          n = n2 * 4,
          location = 0,
          scale = 5
        ), ncol = 4),
        matrix(rcauchy(
          n = n2 * (d-4),
          location = 0,
          scale = 1
        ), ncol = d-4)
        )
      },
      
      ex7 = {
        pop1 = cbind(matrix(rnorm(
          n1 * 4, mean = 0, sd = sqrt(4)
        ), ncol = 4), matrix(rnorm(
          n1 * (d - 4), mean = 0, sd = sqrt(1)
        ), ncol = d - 4))
        
        pop2  = cbind(matrix(
          mixdist(n = n2 * 4, mu = 1.95),
          nrow = n2,
          ncol = 4
        ),
        matrix(rnorm(
          n2 * (d-4), mean = 0, sd = sqrt(1)
        ), ncol = d-4))
      },
      
      ex8 = {
        pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        tmp = foreach (k = 1:n2, .combine = rbind) %dopar% {
          c(cycl(pop2[k, 1:2]), cycl(pop2[k, 3:4]))
        }
        pop2[, 1:4] = tmp
        rm(tmp)
      },
      
      ex9 = {
        pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
        
        pop1[, 1:3] = foreach (k = 1:n1,
                               .combine = rbind,
                               .packages = 'MASS') %dopar% {
                                 c(rnorm(
                                   n = 1,
                                   mean = revmu,
                                   sd = 1
                                 ),
                                 mvrnorm(
                                   n = 1,
                                   mu = c(0, 0) ,
                                   Sigma = SigmaI.1.1
                                 ))
                               }
        
        
        pop2[, 2:3] = mvrnorm(n = n2,
                              mu = c(0, 0) ,
                              Sigma = SigmaI.1.2)
      },
      
      ex10 = {
        pop1 = cbind(
          rnorm(n = n1,
                mean = revmu,
                sd = 1),
          mvrnorm(
            n = n2,
            mu = c(0, 0) ,
            Sigma = SigmaI.2.1
          ),
          mvrnorm(
            n = n1,
            mu = rep(0, d - 3) ,
            Sigma = SigmaI.2.N
          )
        )
        
        pop2 = cbind(
          rnorm(n = n2,
                mean = 0,
                sd = 1),
          mvrnorm(
            n = n2,
            mu = c(0, 0) ,
            Sigma = SigmaI.2.2
          ),
          mvrnorm(
            n = n2,
            mu = rep(0, d - 3),
            Sigma = SigmaI.2.N
          )
        )
      },
      
      ex11 = {
        pop1 = cbind(
          mvrnorm(
            n = n1,
            mu = c(revmu, rep(0, 2)) ,
            Sigma = SigmaI.2.5.1
          ),
          mvrnorm(
            n = n1,
            mu = rep(0, d - 3) ,
            Sigma = SigmaI.2.5.N
          )
        )
        
        pop2 = cbind(
          mvrnorm(
            n = n2,
            mu = rep(0, 3) ,
            Sigma = SigmaI.2.5.2
          ),
          mvrnorm(
            n = n2,
            mu = rep(0, d - 3),
            Sigma = SigmaI.2.5.N
          )
        )
      },
      
      ex12 = {
        pop1 = cbind(
          mvrnorm(
            n = n1,
            mu = c(revmu, rep(0, 2)) ,
            Sigma = SigmaI.3.1
          ),
          mvrnorm(
            n = n1,
            mu = rep(0, d - 3) ,
            Sigma = SigmaI.3.N
          )
        )
        
        pop2 = cbind(
          mvrnorm(
            n = n2,
            mu = rep(0, 3) ,
            Sigma = SigmaI.3.2
          ),
          mvrnorm(
            n = n2,
            mu = rep(0, d - 3),
            Sigma = SigmaI.3.N
          )
        )
      },
      
      ex13 = {
        pop1 = mvrnorm(n = n1,
                       mu = c(revmu, rep(0, d - 1)) ,
                       Sigma = SigmaII.2.1)
        
        pop2 = mvrnorm(n = n2,
                       mu = rep(0, d) ,
                       Sigma = SigmaII.2.2)
      },
      
      ex14 = {
        pop1 = mvrnorm(n = n1,
                       mu = c(revmu, rep(0, d - 1)) ,
                       Sigma = SigmaII.5.1)
        
        pop2 = mvrnorm(n = n2,
                       mu = rep(0, d) ,
                       Sigma = SigmaII.5.2)
      },
      
      ex15 = {
        pop1 = mvrnorm(n = n1,
                       mu = c(revmu, rep(0, d - 1)) ,
                       Sigma = SigmaII.4.1)
        
        pop2 = mvrnorm(n = n2,
                       mu = rep(0, d) ,
                       Sigma = SigmaII.4.2)
      },
      
      ex16 = {
        pop1 = mvrnorm(n = n1,
                       mu = c(revmu, rep(0, d - 1)) ,
                       Sigma = SigmaII.3.1)
        
        pop2 = mvrnorm(n = n2,
                       mu = rep(0, d) ,
                       Sigma = SigmaII.3.2)
      }
    )
    
    ##########################################################################
    ########################### DATA GENERATION IS COMPLETE ##################
    train1 = pop1[1:n1train,]
    train2 = pop2[1:n2train,]
    
    train.set  = rbind(train1, train2)
    
    test.set = rbind(pop1[n1train + (1:n1test),], pop2[n2train + (1:n2test),])
    rm(pop1)
    rm(pop2)
    train.lab = rep(1:2, c(n1train, n2train))
    test.lab = rep(1:2, c(n1test, n2test))
    tmp = NULL
    
    clusterExport(cl, c('train.set', 'test.set'))
    
    # gf = gamfuns[1]
    
    for (gf in gamfuns) {
      suffix <- switch(gf,
                       'gexp' = "_g1",
                       'gsqrt' = "_g2",
                       'glog' = "_g3",
                       NA)  # default value if none of the options match
      clusterExport(cl, c('gf'))
      
      tmar = system.time({
        # time required to identify the marginal signals
        margE = ks_test_1d(train.set, n1train, n2train)
        
        sort.margE = sort(margE)
        
        b.rat = abs(sort.margE[-1] / sort.margE[-length(sort.margE)])
        
        Ecut = (floor(length(b.rat) / 2)) + which.max(b.rat[-(1:(floor(length(b.rat) /
                                                                         2)))])
        sig.pos.est = which(margE %in% sort.margE[1:(length(sort.margE)) > Ecut]) # selected components
        marg.var = sig.pos.est
      }) #identification of marginal signals ends here
      
    
      #classification based on marginal signals begins here.
      marclass = system.time({
        # Step 1: Extract Marginal Data
        train_class1 <- as.matrix(train.set[train.lab == 1, sig.pos.est])
        train_class2 <- as.matrix(train.set[train.lab == 2, sig.pos.est])
        
        n <- nrow(train_class1)
        m <- nrow(train_class2)
        
        # Step 2: Compute KDEs
        f_hats <- lapply(1:ncol(train_class1), function(i) {
          kde(train_class1[, i])
        })
        g_hats <- lapply(1:ncol(train_class2), function(i) {
          kde(train_class2[, i])
        })
        
        # Step 3: Compute Aggregate Densities (using log-scale for numerical stability)
        compute_density <- function(x, kde_list) {
          density_logs <- sapply(1:length(kde_list), function(i) {
            pred_val <- predict(kde_list[[i]], x = x[i])
            if(is.atomic(pred_val)) {
              log_val <- log(pred_val[1])  # Assuming the first value is the estimate
            } else {
              log_val <- log(pred_val$estimate)
            }
            return(log_val)
          })
          return(sum(density_logs))
        }
        
        
        
        # Step 4: Classify Test Data
        classify_test_obs <- function(x) {
          log_F_hat <- compute_density(x, f_hats) + log(n / (n + m))
          log_G_hat <- compute_density(x, g_hats) + log(m / (n + m))
          
          DM <- log_F_hat - log_G_hat
          ifelse(DM >= 0, 1, 2)
        }
        
        test_data_subset <- test.set[, sig.pos.est, drop = FALSE]
        test_predictions <- apply(test_data_subset, 1, classify_test_obs)
        
        EMarclass = mean(test_predictions != test.lab) # 
        out2[iter, paste("EMarClass", suffix, sep = "")] <- EMarclass
      })
      
      IM.tmp = match(1:no.s, sig.pos.est)
      IM.tmp[!is.na(IM.tmp)] = 1
      IM.tmp[is.na(IM.tmp)] = 0
      IM = sum(IM.tmp) # how many components are correctly identified
      
      #screening by MixS begins here.
      tpair = system.time({
        pairE = ks_test_2d(train.set, n1train, n2train)
      })
      
      tnbp = system.time({
        #time required for the non-bipartite matching algorithm to find the appropriate pairs of components
        pairE = as.matrix(pairE)
        dissimMAT =  max(pairE) - pairE #measure of dissimilarity
        diag(dissimMAT) = 0 # diagonals of the dissimilarity matrix are set to zero
        obj3 = distancematrix(x = as.matrix(dissimMAT))
        obj4 = nonbimatch(obj3)
        
        pairs.tmp = cbind(obj4$matches$Group1.Row, obj4$matches$Group2.Row) #- obj4$matches$Distance + max(as.matrix(pairE))
        pairs.tmp1 = unique(apply(pairs.tmp, 1, function(vec)
          paste(sort(vec), collapse = '-'))) #removing repeated entries
        Dpairs = do.call('rbind.data.frame',
                         strsplit(pairs.tmp1, split = '-', fixed = T))
        colnames(Dpairs) = c('p1', 'p2')
        rm(obj3)
        rm(obj4)
        rm(pairs.tmp)
        rm(pairs.tmp1)
        
        Dpairs = (apply(Dpairs, 2, as.numeric))
      }) # pairing complete
      
      tdecoupling = system.time({
        clusterExport(cl, 'pairE')
        
        blockE = parApply(cl, Dpairs, 1, function(vec) {
          pairE[vec[1], vec[2]]
        })
        
        sort.blockE = sort(blockE)
        
        b.rat = abs(sort.blockE[-1] / sort.blockE[-length(sort.blockE)])
        
        Ecut = (floor(length(b.rat) / 2)) + which.max(b.rat[-(1:(floor(length(b.rat) /
                                                                         2)))])

        sig.pos.est = which(blockE %in% sort.blockE[1:(length(sort.blockE)) > Ecut]) # selected pairs
        # Convert matrix rows into strings of pairs
        selected_pairs <- Dpairs[sig.pos.est, , drop=FALSE]
        pair_strings <- apply(selected_pairs, 1, function(x) paste(x, collapse = "-"))
        # Add it to the out2 dataframe
        out2[iter, paste("pair", suffix, sep = "")] <- paste(pair_strings, collapse = ", ")
        clusterExport(cl, c('Dpairs', 'margE'))
        pair.key = foreach(
          val = 1:length(sig.pos.est),
          .combine = rbind,
          .packages = 'energy'
        ) %do% {
          comps = as.numeric(Dpairs[sig.pos.est[val],])
          e1 = margE[comps[1]]
          e2 = margE[comps[2]]
          e12M = (e1 + e2) / 2
          e12J = pairE[comps[1], comps[2]]
          
          T.val = c(e1, e2, e12M, e12J)
          
          comp1.dist = proxy::dist(train.set[, comps[1]], method = get(paste(
            'custom.dist', substr(gf, start = 2, stop = 5), sep = '.'
          )))
          comp1.dist = as.matrix(comp1.dist)
          
          comp2.dist = proxy::dist(train.set[, comps[2]], method = get(paste(
            'custom.dist', substr(gf, start = 2, stop = 5), sep = '.'
          )))
          comp2.dist = as.matrix(comp2.dist)
          
          comp12.dist = proxy::dist(train.set[, comps], method = get(paste(
            'custom.dist', substr(gf, start = 2, stop = 5), sep = '.'
          )))
          comp12.dist = as.matrix(comp12.dist)
          
          
          clusterExport(cl, c('comp1.dist', 'comp2.dist', 'comp12.dist'))
          permEn = parApply(cl, prms, 1 , function(vec) {
            tmp1 = energy::eqdist.e(
              x = comp1.dist[vec, vec],
              sizes = c(n1train, n2train),
              distance = T
            )
            
            tmp2 = energy::eqdist.e(
              x = comp2.dist[vec, vec],
              sizes = c(n1train, n2train),
              distance = T
            )
            
            tmp12M = (tmp1 + tmp2) / 2
            tmp12J = energy::eqdist.e(
              x = comp12.dist[vec, vec],
              sizes = c(n1train, n2train),
              distance = T
            )
            
            return(c(tmp1, tmp2, tmp12M, tmp12J))
          })
          
          p.vals = sapply(1:4, function(val) {
            mean(permEn[val, ] >= T.val[val])
          })
          
          if (which.min(p.vals) == 3) {
            paste(c('MA', 'MB'), val, sep = '')
          } else{
            if (which.min(p.vals) == 1) {
              paste(c('M', 'D'), val, sep = '')
            } else{
              if (which.min(p.vals) == 2) {
                paste(c('D', 'M'), val, sep = '')
              } else{
                paste(c('J', 'J'), val, sep = '')
              }
            }
          }
        }
        
        components = as.numeric(t(as.matrix(Dpairs[sig.pos.est,])))
        keys = c(t(pair.key))
        
        drops = grep(pattern = 'D', x = keys)
        if (length(drops) > 0) {
          final_select = components[-drops]
          block.methods = keys[-drops]
        } else{
          final_select = components
          block.methods = keys
        }
        
        Shat = cbind.data.frame(block.methods, final_select)
        listShat = split(Shat$final_select, f = block.methods)
      })
      
      # pairClass = system.time({
      #   train_data_X <- train.set[train.lab == 1, ]
      #   train_data_Y <- train.set[train.lab == 2, ]
      #   
      #   # Step 1: Extract paired data for training
      #   extract_paired_data <- function(data, Dpairs, sig.pos.est) {
      #     paired_data <- lapply(sig.pos.est, function(pos) {
      #       pair <- Dpairs[pos, ]
      #       # Extract the paired columns based on the pair indices
      #       return(data[, c(pair[1], pair[2])])
      #     })
      #     return(paired_data)
      #   }
      #   
      #   # Step 2: Train KDEs
      #   train_kde_pairs <- function(data_X, data_Y, Dpairs, sig.pos.est) {
      #     paired_data_X <- extract_paired_data(data_X, Dpairs, sig.pos.est)
      #     paired_data_Y <- extract_paired_data(data_Y, Dpairs, sig.pos.est)
      #     
      #     kde_list_f <- lapply(paired_data_X, function(pair_data) {
      #       # Ensure that the KDE function receives 2D data for each pair
      #       kde(pair_data)
      #     })
      #     
      #     kde_list_g <- lapply(paired_data_Y, function(pair_data) {
      #       # Ensure that the KDE function receives 2D data for each pair
      #       kde(pair_data)
      #     })
      #     
      #     return(list(f = kde_list_f, g = kde_list_g))
      #   }
      #   compute_density_pair <- function(x, kde_list) {
      #     density_logs <- sapply(1:length(kde_list), function(i) {
      #       pred_val <- predict(kde_list[[i]], x = x)
      #       if (is.atomic(pred_val)) {
      #         log_val <- log(pred_val)  # Use the atomic value directly
      #       } else {
      #         log_val <- log(pred_val$estimate)
      #       }
      #       return(log_val)
      #     })
      #     return(sum(density_logs))
      #   }
      #   
      #   
      #   # Train the KDE pairs using the training data
      #   kde_pairs <- train_kde_pairs(train_data_X, train_data_Y, Dpairs, sig.pos.est)
      #   
      #   # Extract the relevant paired columns from test.set
      #   test_data_paired <- extract_paired_data(test.set, Dpairs, sig.pos.est)
      #   
      #   DP_list <- lapply(test_data_paired, function(test_data) {
      #     apply(test_data, 1, function(x) {
      #       # Convert the vector x back to a 2-column matrix
      #       x_matrix <- matrix(x, ncol = 2)
      #       
      #       log_Fhat <- compute_density_pair(x_matrix, kde_pairs$f) + log(n1train / (n1train + n2train))
      #       log_Ghat <- compute_density_pair(x_matrix, kde_pairs$g) + log(n2train / (n1train + n2train))
      #       log_Fhat - log_Ghat
      #     })
      #   })
      #   
      #   DP_average <- apply(simplify2array(DP_list), 1, mean)
      #   
      #   test_labels <- ifelse(DP_average >= 0, 1, 2)
      #   
      #   EPairclass = mean(test_labels != test.lab) # 
      #   
      #   # Use the classifier on paired test data
      #   # test_labels <- paired_classifier(test_data_paired, kde_pairs, n1train, n2train)
      #   
      # })
      
      pairclass = system.time({
        
        # Step 1: Extract Paired Data for training
        extract_paired_data <- function(data, Dpairs, sig.pos.est) {
          selected_pairs <- Dpairs[sig.pos.est, ]
          
          # Now, extract these pairs from the data
          paired_data <- do.call(cbind, lapply(1:nrow(selected_pairs), function(i) {
            pair <- selected_pairs[i, ]
            data[, c(pair[1], pair[2])]
          }))
          
          return(as.matrix(paired_data))
        }
        # train_data_X <- extract_paired_data(train.set[train.lab == 1, ], Dpairs, sig.pos.est)
        # train_data_Y <- extract_paired_data(train.set[train.lab == 2, ], Dpairs, sig.pos.est)
        
        train_data_X <-train.set[train.lab == 1, ]
        train_data_Y <-train.set[train.lab == 2, ]
        
        n <- nrow(train_data_X)
        m <- nrow(train_data_Y)
        
        # Step 2: Compute KDEs for 2D data
        compute_kdes_2d <- function(data, Dpairs, sig.pos.est) {
          selected_pairs <- Dpairs[sig.pos.est, , drop = FALSE]
          kde_list <- lapply(1:nrow(selected_pairs), function(i) {
            pair <- selected_pairs[i, ]
            if(any(pair > ncol(data))) {
              print(paste("Problematic pair indices:", pair[1], pair[2]))
            }
            kde(data[, c(pair[1], pair[2])])
          })
          return(kde_list)
        }
        
        f_hats <- compute_kdes_2d(train_data_X, Dpairs, sig.pos.est)
        g_hats <- compute_kdes_2d(train_data_Y, Dpairs, sig.pos.est)
        
        # Compute the dp(x)
        compute_dp <- function(x_2d, f_hats, g_hats) {
          log_Fhat = 0
          log_Ghat = 0
          for(i in 1:length(f_hats)) {
            pred_val_f = predict(f_hats[[i]], x = matrix(x_2d[c(2*i-1, 2*i)], ncol=2))
            pred_val_g = predict(g_hats[[i]], x = matrix(x_2d[c(2*i-1, 2*i)], ncol=2))
            
            # Ensure that we handle atomic predictions correctly
            if (is.atomic(pred_val_f)) {
              log_Fhat = log_Fhat + log(pred_val_f)
            } else {
              log_Fhat = log_Fhat + log(pred_val_f$estimate)
            }
            
            if (is.atomic(pred_val_g)) {
              log_Ghat = log_Ghat + log(pred_val_g)
            } else {
              log_Ghat = log_Ghat + log(pred_val_g$estimate)
            }
          }
          
          return(log_Fhat - log_Ghat)
        }
        
        # Extract the relevant paired columns from the test set based on selected pairs
        test_data_paired = as.matrix(test.set[, unlist(selected_pairs)])
        
        # Calculate DP values for each row of the test set
        DP_values = apply(test_data_paired, 1, function(x) compute_dp(x, f_hats, g_hats))
        
        # Assign class labels based on the sign of DP(x)
        class_labels = ifelse(DP_values >= 0, 1, 2)
        
        EPairclass = mean(class_labels != test.lab) # 
        out2[iter, paste("EPairClass", suffix, sep = "")] <- EPairclass
        
      })
      
      switch(
        exID,
        ex1 = {
          listS = list(1,2,3,4)
        },
        ex2 = {
          listS = list(1:2, 3:4)
        },
        ex3 = {
          listS = list(1:2, 3)
        },
        ex4 = {
          listS = list(1,2,3,4)
        },
        ex5 = {
          listS = list(1,2,3,4)
        },
        ex6 = {
          listS = list(1,2,3,4)
        },
        ex7 = {
          listS = list(1,2,3,4)
        },
        ex8 = {
          listS = list(1:2, 3:4)
        },
        ex9 = {
          listS = list(1, c(2, 3))
        },
        ex10 = {
          listS = list(1, c(2, 3))
        },
        ex11 = {
          listS = list(1, c(2, 3))
        },
        ex12 = {
          listS = list(1, c(2, 3))
        },
        ex13 = {
          listS = list(1, c(2, 3))
        },
        ex14 = {
          listS = list(1, c(2, 3))
        },
        ex15 = {
          listS = list(1, c(2, 3))
        },
        ex16 = {
          listS = list(1, c(2, 3))
        }
      )
      
      IB.tmp = match(listS, listShat)
      IB.tmp[!is.na(IB.tmp)] = 1
      IB.tmp[is.na(IB.tmp)] = 0
      IB = sum(IB.tmp) # how many blocks are correctly identified
      
      IC.tmp = match(1:no.s, final_select)
      IC.tmp[!is.na(IC.tmp)] = 1
      IC.tmp[is.na(IC.tmp)] = 0
      IC = sum(IC.tmp) # how many components are correctly identified

      out2$listS[iter] <- paste(format_list(listS), collapse = ", ")
      out2[iter, paste("listShat", suffix, sep = "")] <- paste(format_list(listShat), collapse = ", ")
      marg_string <- paste(marg.var, collapse = ", ")
      out2[iter, paste("marg", suffix, sep = "")] <- marg_string
      
      tMixSclassctn <- system.time({
        newtrain = as.matrix(train.set[, final_select])
        newtrain1 = as.matrix(train1[, final_select])
        newtrain2 = as.matrix(train2[, final_select])
        newtest = as.matrix(test.set[, final_select])
        
        split.train.SW <-   matrix(0,
                                   nrow = nrow(newtrain),
                                   ncol = ncol(newtrain))
        for (i in 1:ncol(split.train.SW)) {
          split.train.SW[, i] <- rep(block.methods[i], nrow(newtrain))
        }
        
        tmp.blocked.train.SW <-
          split(as.matrix(newtrain), split.train.SW)
        
        rm(split.train.SW)
        
        blocked.train.SW <-
          lapply(tmp.blocked.train.SW, function(x) {
            a1 <- matrix(x, nrow = nrow(newtrain), byrow = F)
            return(a1)
          })
        
        rm(tmp.blocked.train.SW)
        
        clusterExport(cl,
                      c(
                        'newtrain',
                        'block.methods',
                        'blocked.train.SW',
                        'fun2.block'
                      ))
        distmats.temp.SW <-
          lapply(1:nrow(newtrain), function(y) {
            fun2.block(
              x = split(as.numeric(newtrain[y, ]), block.methods),
              blocked.train.SW,
              gamfun = gf
            )
          })
        
        SW.distmat <- do.call('cbind', distmats.temp.SW)
        
        rm(distmats.temp.SW)
        
        
        clusterExport(cl,
                      c("SW.distmat"))
        
        bgSAVG.lbl <- t(parApply(cl, newtest, 1, function(Z) {
          bgSAVG.real(
            z = split(as.numeric(Z), block.methods),
            blocked.train.SW = blocked.train.SW,
            n1train = n1train,
            n2train = n2train,
            SW.distmat = SW.distmat,
            gamfun  = gf
          )
          
        }))   #performance of the proposed method
        E0 = mean(bgSAVG.lbl != test.lab) # error of proposed method
      })
      
      
      s.hat = floor(ntrain / log(ntrain)) #n/logn
      
      # MV-SIS
      tMVscreen1 = system.time({
        MV.order = screenIID(X = train.set,
                             Y = train.lab,
                             method = 'MV-SIS')
        
        MV.var = which(MV.order$rank %in% 1:s.hat)
      })
      
      #RRS
      tRRSscreen1 = system.time({
        RRS.var = screenRRS(train.set, n1train, n2train)
      })
      
      #FKF
      tKGscreen1 = system.time({
        KG = k.filter(
          x = train.set,
          y = train.lab,
          response.type = 'categorical',
          method = 'single'
        )
        # KG.var = which(KG$k.rank %in% 1:s.hat)
        KG.var = which(order(KG$k.stat, decreasing = T) %in% 1:s.hat)
      })
      
      sig.pos.est = list(marg.var,
                         final_select,
                         MV.var,
                         RRS.var,
                         KG.var) #
      
      tru.sig = sapply(sig.pos.est, function(vec)
        sum(1:no.s %in% vec))
      
      tru.nois = sapply(sig.pos.est, function(vec) {
        est.nois = setdiff(1:d, vec)
        true.noise = setdiff(1:d, sig.pos)
        cap.nois = intersect(est.nois, true.noise)
        length(cap.nois)
      })
      
      selected = sapply(sig.pos.est, length)
      
      Indices = cbind.data.frame(
        'ModelSize' = selected,
        'Cap.Sig' = tru.sig,
        'Cap.Noise' = tru.nois
      ) #screening performance of all methods
      
      
      sig.pos.est = list('EMV' = MV.var,
                         'ERRS' =  RRS.var,
                         'EKG' = KG.var)
      
      LIST =  lapply(sig.pos.est, function(vec) {
        tmvclass = system.time({
          newtrain = as.matrix(train.set[, vec])
          newtrain1 = as.matrix(train.set[which(train.lab == 1), vec])
          newtrain2 = as.matrix(train.set[which(train.lab == 2), vec])
          newtest = as.matrix(test.set[, vec])
          
          split.train.SW <-   matrix(0,
                                     nrow = nrow(newtrain),
                                     ncol = ncol(newtrain))
          
          block.methods = rep(1:length(vec))
          
          for (i in 1:ncol(split.train.SW)) {
            split.train.SW[, i] <- rep(block.methods[i], nrow(newtrain))
          }
          
          tmp.blocked.train.SW <-
            split(as.matrix(newtrain), split.train.SW)
          
          rm(split.train.SW)
          
          blocked.train.SW <-
            lapply(tmp.blocked.train.SW, function(x) {
              a1 <- matrix(x, nrow = nrow(newtrain), byrow = F)
              return(a1)
            })
          
          rm(tmp.blocked.train.SW)
          
          clusterExport(cl,
                        c('newtrain', 'block.methods', 'blocked.train.SW'),
                        envir = environment())
          distmats.temp.SW <-
            parLapply(cl, 1:nrow(newtrain), function(y) {
              fun2.block(
                x = split(as.numeric(newtrain[y, ]), block.methods),
                blocked.train.SW,
                gamfun = gf
              )
            })
          
          SW.distmat <- do.call('cbind', distmats.temp.SW)
          
          rm(distmats.temp.SW)
          
          
          clusterExport(cl,
                        c("SW.distmat"), envir = environment())
          gSAVG.lbl <- t(parApply(cl, newtest, 1, function(Z) { #predicting class labels with bgSAVG after screening variables using the existing methods.
            bgSAVG.real(
              z = split(as.numeric(Z), block.methods),
              blocked.train.SW = blocked.train.SW,
              n1train = n1train,
              n2train = n2train,
              SW.distmat = SW.distmat,
              gamfun = gf
            )
            
          }))
        })
        return(c(tmvclass[3], mean(gSAVG.lbl != test.lab)))
      })
      
      tmp = c(
        tmp,
        c(
          tmar[3],
          #MarS screening time
          marclass[3],
          #MarS classification time
          tpair[3],
          # time for pXp distance matrix computation
          tnbp[3],
          # nbpmatching time
          tdecoupling[3],
          # time for decoupling of pairs using permutation tests
          tMixSclassctn[3],
          # MixS classifition time
          EMarclass,
          # MarS classsificaiton error
          EPairclass,
          # MixS classification  error
          tMVscreen1[3],
          #MVSIS screening time
          LIST$EMV[1],
          #MVSIS classification time
          LIST$EMV[2],
          # MVSIS classification error
          tRRSscreen1[3],
          #RRS screening time
          LIST$ERRS[1],
          #RRS classification time
          LIST$ERRS[2],
          # RRS classification error
          tKGscreen1[3],
          # KF screening time
          LIST$EKG[1],
          # KF classification time
          LIST$EKG[2],
          # KF classification error
          IB,
          # number of correctly identified blocks
          unlist(c(Indices)) # number of correctly identified components
        )
      )
      #Commented out gamfun loop is here
    }
    
    #MCS-SVM
    tMCSscreen = system.time({
      tmp1 = MCS.SVM(train.set, train.lab)
      MCS.svmlin.var = tmp1[[1]]
      MCS.svmrbf.var = tmp1[[2]]
    })
    
    tMCSSVMLINclassfctn = system.time({
      newtrain = as.matrix(train.set[, MCS.svmlin.var])
      newtest = as.matrix(test.set[, MCS.svmlin.var])
      k.den1 = svm(x = newtrain,
                   y = factor(train.lab),
                   kernel = 'linear')
      svm.lin.lbl = predict(k.den1, newdata = newtest)
      ESvmLin = mean(svm.lin.lbl != test.lab)
      
    })
    
    tMCSSVMRBFclassfctn = system.time({
      k.den2 = svm(x = train.set[, MCS.svmrbf.var],
                   y = factor(train.lab),
                   kernel = 'radial')
      svm.rbf.lbl = predict(k.den2, newdata = test.set[, MCS.svmrbf.var])
      
      ESvmRbf = mean(svm.rbf.lbl != test.lab)
    })
    
    sig.pos.est = list(MCS.svmlin.var,
                       MCS.svmrbf.var) #
    
    tru.sig = sapply(sig.pos.est, function(vec)
      sum(1:no.s %in% vec))
    
    tru.nois = sapply(sig.pos.est, function(vec) {
      est.nois = setdiff(1:d, vec)
      true.noise = setdiff(1:d, sig.pos)
      cap.nois = intersect(est.nois, true.noise)
      length(cap.nois)
    })
    
    selected = sapply(sig.pos.est, length)
    
    Indices.MCS = cbind.data.frame(
      'ModelSize' = selected,
      'Cap.Sig' = tru.sig,
      'Cap.Noise' = tru.nois
    ) #screening performance of all methods
    
     ERR[[iter]] = c(
      tmp,
      tMCSscreen[3] / 2,
      #time required for MCSSVMLIN screening
      tMCSSVMLINclassfctn[3],
      # time for MCSSVMLIN classification
      ESvmLin,
      # MCSSVMLIN classification error
      tMCSscreen[3] / 2,
      #time required for MCSSVMRBF screening
      tMCSSVMRBFclassfctn[3],
      # time for MCSSVMRBF classification
      ESvmRbf,
      # MCSSVMRBF classification error
      unlist(c(Indices.MCS))
    )
    
    
    setTxtProgressBar(pb, iter)
    cat(
      paste('//', iter * 100 / REP, '% complete', sep = ''),
      paste("Time Elapsed ", round((
        proc.time()[3] - ptm[3]
      ) / 60), " Minutes"),
      " "
    )
    
  }
  
  gc()
  
  out = do.call('rbind', ERR)
  
  t.stamps = c(
    'MarS screening time (sec)',
    'MarS classification time (sec)',
    'pairwise energy comp time (sec)',
    'NBP matching time (sec)',
    'time for decoupling of pairs (sec)',
    'MixS classification time (sec)'
  )
  prop.methods = c('MarS', 'MixS')
  ex.methods = c('MV', 'RRS', 'KG')
  all.methods = c('MarS', 'MixS', 'MV', 'RRS', 'KG')
  
  nm1 = sapply(gamfuns, function(val) {
    tmp0 = c(sapply(ex.methods, function(flg) {
      paste(c('screen.time', 'class.time', 'ERR'), flg, val, sep = '_')
    }))
    
    
    mdlsz = paste('modelsize', all.methods, val, sep = '_')
    capsig = paste('capSig', all.methods, val, sep = '_')
    capnois = paste('capNoise', all.methods, val, sep = '_')
    
    c(
      t.stamps,
      paste(prop.methods, val, sep = '_'),
      tmp0,
      'captured#Blocks',
      mdlsz,
      capsig,
      capnois
    )
  })
  
  MCSLINnms = paste(c('screen.time', 'class.time', 'ERR'), 'MCSSVMLIN', sep =
                      '_')
  MCSRBFnms = paste(c('screen.time', 'class.time', 'ERR'), 'MCSSVMRBF', sep =
                      '_')
  
  mdlsz = paste('modelsize', c('MCSSVMLIN', 'MCSSVMRBF'), sep = '_')
  capsig = paste('capSig', c('MCSSVMLIN', 'MCSSVMRBF'), sep = '_')
  capnois = paste('capNoise', c('MCSSVMLIN', 'MCSSVMRBF'), sep = '_')
  
  
  colnames(out) = c(nm1,  c(MCSLINnms, MCSRBFnms, mdlsz, capsig, capnois))
  out.name = switch(
    EXPR = exID,
    ex1 = 'ErrorRates_1.csv',
    ex2 = 'ErrorRates_2.csv',
    ex3 = 'ErrorRates_3.csv',
    ex4 = 'ErrorRates_4.csv',
    ex5 = 'ErrorRates_5.csv',
    ex6 = 'ErrorRates_6.csv',
    ex7 = 'ErrorRates_7.csv',
    ex8 = 'ErrorRates_8.csv',
    ex9 = 'ErrorRates_9.csv',
    ex10 = 'ErrorRates_10.csv',
    ex11 = 'ErrorRates_11.csv',
    ex12 = 'ErrorRates_12.csv',
    ex13 = 'ErrorRates_13.csv',
    ex14 = 'ErrorRates_14.csv',
    ex15 = 'ErrorRates_15.csv',
    ex16 = 'ErrorRates_16.csv'
  )
  out2.name = switch(
    EXPR = exID,
    ex1 = 'signals_1.csv',
    ex2 = 'signals_2.csv',
    ex3 = 'signals_3.csv',
    ex4 = 'signals_4.csv',
    ex5 = 'signals_5.csv',
    ex6 = 'signals_6.csv',
    ex7 = 'signals_7.csv',
    ex8 = 'signals_8.csv',
    ex9 = 'signals_9.csv',
    ex10 = 'signals_10.csv',
    ex11 = 'signals_11.csv',
    ex12 = 'signals_12.csv',
    ex13 = 'signals_13.csv',
    ex14 = 'signals_14.csv',
    ex15 = 'signals_15.csv',
    ex16 = 'signals_16.csv'
  )
  RESULT[[flg]] = out
  flg = flg+1
  
  # Convert each numeric vector in the list to a single character string
  # marg_sig_str <- sapply(marg_sig, function(x) paste(x, collapse = ", "))
  # marg_df <- data.frame(marginal_signals = marg_sig_str, stringsAsFactors = FALSE)
  # out$'captured marginal signals' <- marg_df
  # out$'listS' <- sapply(listS_results, paste, collapse=", ")
  # out$'listShat' <-  sapply(listShat_results, paste, collapse=", ")
  # out$'listS' <- vapply(listS_results, function(x) paste(x, collapse = ", "), character(1))
  # out$'listShat' <- vapply(listShat_results, function(x) paste(x, collapse = ", "), character(1))
  

  # correct_df <- data.frame(correct_sig = rep(correct_sigs, each = REP), stringsAsFactors = FALSE)
  # out$'correct signals' <- correct_df
  
  # out$'captured paired signals' <- pair_sig
  # out$'sig.pos.est[[1]]' <- 
  # out$'sig.pos.est[[2]]' <- 
  write.csv(out, out.name, row.names = F)
  write.csv(out2, out2.name, row.names = F)
  
  close(pb)
  print(paste(exID, 'is done.', sep = ' '))
  
}

stopCluster(cl)
gc()
