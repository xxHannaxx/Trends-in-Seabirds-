

#  DFA analysis

# remove rows that only contain 0s or NAs
CB<- CB[rowSums(CB[], na.rm = T)>0,]
CN<- CN[rowSums(CN[], na.rm = T)>0,]
BB<- BB[rowSums(BB[], na.rm = T)>0,]
BN<- BN[rowSums(BN[], na.rm = T)>0,]
PB<- PB[rowSums(PB[], na.rm = T)>0,]
PN<- PN[rowSums(PN[], na.rm = T)>0,]


# standardize the data using scale2
library(multicon)

# transpose the matrices
BBt<- t(BB)
BNt<- t(BN)
CBt<- t(CB)
CNt<- t(CN)
PBt<- t(PB)
PNt<- t(PN)


BBsc <- scale2(BBt)
BNsc <- scale2(BNt)
CBsc <- scale2(CBt)
CNsc <- scale2(CNt)
PBsc <- scale2(PBt)
PNsc <- scale2(PNt)

# transpose so that time series goes across columns
BB<- t(BBsc)
BN<- t(BNsc)
CB<- t(CBsc)
CN<- t(CNsc)
PB<- t(PBsc)
PN<- t(PNsc)

# get number of time series

nBB<- dim(BB)[1]
nBN<- dim(BN)[1]
nCB<- dim(CB)[1]
nCN<- dim(CN)[1]
nPB<- dim(PB)[1]
nPN<- dim(PN)[1]

# get length of times series

lBB<- dim(BB)[2]
lBN<- dim(BN)[2]
lCB<- dim(CB)[2]
lCN<- dim(CN)[2]
lPB<- dim(PB)[2]
lPN<- dim(PN)[2]





## PB new with 10 trends



levels.R<-c("equalvarcov")

A <-matrix(0, nrow=nPB, ncol=1) # when I named it A_CB, it gave an error

# set the min & max number of latent variables estimated
M.min <- 1
M.max <- 10 


# total num of models to test
n.set <-length(levels.R)*(M.max - M.min + 1)


# set counter for model results
cnt_PB <- 1

mod.fit_PB_eq <-vector("list", n.set)

for(R in levels.R)  {             # looping over R structures
  # store name of R for later
  r.name <- R
  for(m in M.min:M.max){          # looping over number of latent variables, or trends
    Z_PB <-matrix(list(), nPB, m)
    for(i in seq(nPB)) {Z_PB[i,] <-paste(i,seq(m), sep="")}
    if(m > 1) {for(i in 1:(m-1)){Z_PB[i, (i+1):m] <- 0}}
    x0_PB <- U_PB <-matrix(0, m, 1)
    Q_PB <- B_PB <-diag(1, m)
    V0_PB <-diag(26, m)
    dfa.model_PB_eq <-list(A="zero", R=R, m=m)
    # fit DFA & store results
    mod.fit_PB_eq[[cnt_PB]] <-MARSS(PB,
                                    model=dfa.model_PB_eq, z.score=TRUE, form="dfa",
                                    control=list(maxit=40000,allow.degen=FALSE,abstol=0.0001,
                                                 conv.test.slope.tol=0.05,
                                                 safe=TRUE,trace=0),silent=FALSE)
    mod.fit_PB_eq[[cnt_PB]]$R <- r.name
    mod.fit_PB_eq[[cnt_PB]]$M <- m
    cnt_PB <- cnt_PB + 1}                 #ending m loop
}# ending R loop
