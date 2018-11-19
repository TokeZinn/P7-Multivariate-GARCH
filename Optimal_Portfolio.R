#Calculate Beta of Efficient Frontier: 


E_Oil = mean(Return_DF$Returns_Oil); s_oil = sd(Return_DF$Returns_Oil)
E_Gold = mean(Return_DF$Returns_Gold); s_gold = sd(Return_DF$Returns_Gold)
E_SP500 = mean(Return_DF$Returns_SP500); s_SP500 = sd(Return_DF$Returns_SP500)
c_Gold_SP500 = cov(Return_DF$Returns_Gold, Return_DF$SP500)
M = cbind(Return_DF$Returns_SP500 - E_SP500 ,Return_DF$Returns_Gold - E_Gold)


cov = ((length(Return_DF$Returns_SP500)-1)^(-1))*( t(M) %*% M)
sig = t(c(w_1[1], w_2[1])) %*% cov %*% c(w_1[1], w_2[1])
w_1 = seq(from = -1, to = 3, by = 0.01)
w_2 = 1- w_1
sig = rep(0, length(w_1))
E_R = rep(0, length(w_1))
for (i in 1:length(w_1)){
  E_R[i] = t(c(w_1[i], w_2[i])) %*% c(E_SP500, E_Gold)
  sig[i] = sqrt(t(c(w_1[i], w_2[i])) %*% cov %*% c(w_1[i], w_2[i]))
}


plot(x = sig, y = E_R, type = "l")


w_1 = seq(from = -1, to = 3, by = 0.01)
w_2 = 1- w_1
w = c(w_1,w_2)

