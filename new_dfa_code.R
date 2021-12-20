

levels.R<-c("equalvarcov")

A <-matrix(0, nrow=nPB, ncol=1) # when I named it A_CB, it gave an error

# set the min & max number of latent variables estimated
M.min <- 1
M.max <- 10 

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
    aic_P[[cnt_PB]]<- AIC(MARSS(PB,
                                model=dfa.model_PB_eq, z.score=TRUE, form="dfa",
                                control=list(maxit=40000,allow.degen=FALSE,abstol=0.0001,
                                             conv.test.slope.tol=0.05,
                                             safe=TRUE,trace=0),silent=FALSE))
    #mod.fit_PB_eq[[cnt_PB]]$R <- r.name
    m#od.fit_PB_eq[[cnt_PB]]$M <- m
    cnt_PB <- cnt_PB + 1}                 #ending m loop
}# ending R loop
