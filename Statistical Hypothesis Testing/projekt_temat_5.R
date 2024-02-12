set.seed(123)
X <- rnorm(100, mean = 4, sd = 1)
Y_list = list() 
for (sigma in seq(1, 10, by = 0.5)) {
  epsilon <- rnorm(100, mean = 0, sd = sigma)
  Y <- X / 2 + epsilon
  Y_list[[as.character(sigma)]] <- Y
}
n <- length(X)
empirical_powers <- numeric(length(Y_list))
names(empirical_powers) <- names(Y_list)

empirical_powers_cor <- numeric(length(Y_list))
names(empirical_powers_cor) <- names(Y_list)

empirical_powers_kop <- numeric(length(Y_list))
names(empirical_powers_kop) <- names(Y_list)

for (q in 1:length(Y_list)){
  Y <- Y_list[[q]]
  
  #PEARSON
  observed_cor <- cor(X, Y)
  exceedance_count_cor <- 0
  #SPEARMAN
  R <- rank(X, ties.method = "random")
  S <- rank(Y, ties.method = "random")
  observed_r <- 1 - 6 * sum((R - S)^2) / (n * (n^2 - 1))
  exceedance_count <- 0
  #KOPULA
  exceedance_count_kop <- 0
  Z<-cbind(R,S) 
  Z=Z[order(Z[,1],decreasing=FALSE),] #posortowane pary rang
  u<-c() #u i v to punkty siatki
  v<-c()
  R<-Z[,1]
  S<-Z[,2] #wektor rang wiazanych 
  for (i in 1:(n+1)) {
    u[i]=(i-1/2)/(n+1)
    v[i]=(i-1/2)/(n+1) 
  }
  C<- matrix(0,nrow=n+1,ncol=n+1) #definiujemy kopule
  #algorytm wyliczania komorek macierzy
  for (j in 1:(n+1)) {
    C[j,1]=0 
  }
  for (i in 1:n) {
    for (j in 1:(n+1)) {
      if(j-1<S[i]){
        C[j,i+1]=C[j,i]
      }
      else{
        C[j,i+1]=C[j,i]+1/n
      } 
    }
  }
  for (i in 1:(n+1)) {
    for (j in 1:(n+1)) {
      C[j,i]=C[j,i]-u[i]*v[j]
    } 
  }
  CKS <- max(abs(C))
  CKSMC<-c()
  
  #MONTE CARLO
  mc <- 100
  for (k in 1:mc) {
    
    XMC <- runif(n, 0, 1)
    YMC <- runif(n, 0, 1)
    
    #PEARSON
    corMC <- cor(XMC, YMC)  
    if (abs(corMC) >= abs(observed_cor)) {
      exceedance_count_cor <- exceedance_count_cor + 1
    }
    
    #SPEARMAN
    RMC <- rank(XMC, ties.method = "random")
    SMC <- rank(YMC, ties.method = "random")
    rMC <- 1 - 6 * sum((RMC - SMC)^2) / (n * (n^2 - 1))
    if (abs(rMC) >= abs(observed_r)) {
      exceedance_count <- exceedance_count + 1
    }
    
    #KOPULA
    ZMC<-cbind(RMC,SMC)
    ZMC=ZMC[order(ZMC[,1],decreasing=FALSE),]
    SMC<-ZMC[,2]
    CMC<- matrix(0,nrow=n+1,ncol=n+1)
    for (j in 1:(n+1))
    {
      CMC[j,1]=0 }
    for (i in 1:n) {
      for (j in 1:(n+1)) {
        if(j-1<SMC[i]){
          CMC[j,i+1]=CMC[j,i]}
        else{
          CMC[j,i+1]=CMC[j,i]+1/n} }
    }
    for (i in 1:(n+1)) {
      for (j in 1:(n+1)) {
        CMC[j,i]=CMC[j,i]-u[i]*v[j] }
    }
    CKSMC[k]<-max(abs(CMC))
  }
  #PEARSON
  empirical_powers_cor[q] <- exceedance_count_cor / mc
  #SPEARMAN
  empirical_powers[q] <- exceedance_count / mc
  #KOPULA
  pCKS<-0
  for (j in 1:mc) {
    if(abs(CKSMC[j])>abs(CKS)){ 
      pCKS = pCKS +1} 
  }
  empirical_powers_kop[q] <- pCKS / mc
}

plot(seq(1, 10, by = 0.5), empirical_powers, type = "b", xlab = "Sigma", ylab = "Moc empiryczna", 
     col = "blue", main = "Moc empiryczna w zależności od parametru sigma", ylim = c(0,1))

lines(seq(1, 10, by = 0.5), empirical_powers_cor, type = "b", col = "red")

lines(seq(1, 10, by = 0.5), empirical_powers_kop, type = "b", col = "black")

legend("topright", legend=c("Pearson", "Spearman", "Kołmogorow-Smirnow"), 
       col=c("red", "blue", "black"), lty=1, cex=0.8)
