require(mvtnorm)
S <- matrix(c(1,.8,.8,1),2,2) #Correlation matrix
G <- rmvnorm(mean=c(0,0),sig=S,n=1000) #Our gaussian variables

pmvnorm(upper = G[1,], corr = cov2cor(S))
p_vec = c()
for(i in 1:nrow(G)){
  p_vec = c(p_vec, pmvnorm(upper = G[i,], corr = cov2cor(S)))
}

plot(p_vec)
hist(U_1)
hist(U_2)
U_1 = pnorm(G[,1],sd = sqrt(S[1,1]))
U_2 = pnorm(G[,2],sd = sqrt(S[2,2]))

plot(U_1,U_2)

U <- pnorm(AB) #Now U is uniform - check using hist(U[,1]) or hist(U[,2])
x <- qgamma(U[,1],2) #x is gamma distributed
y <- qbeta(U[,2],1,2) #y is beta distributed
plot(x,y) #They correlate!