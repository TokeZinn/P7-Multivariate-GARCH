# Getting data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./Data_Load.R")

SP500_returns = returns(SP500$Close)[-1]
AMD_returns = returns(AMD$Close)[-1]
NVIDIA_returns = returns(NVIDIA$Close)[-1]

#Fitting garch models using rugarch.

Spec = ugarchspec(variance.model = list( model = "eGARCH", garchOrder = c(1,1)),
                        mean.model = list( armaOrder = c(0,0)) )
 

SP_egarch = ugarchfit(Spec, SP500_returns , solver = "hybrid")
SP_egarch

AMD_egarch = ugarchfit(Spec, AMD_returns , solver = "hybrid")
AMD_egarch


NVIDIA_egarch = ugarchfit(Spec, NVIDIA_returns , solver = "hybrid")
NVIDIA_egarch

#Residul analysis
Sdr_SP = SP_garch@fit$residuals / SP_garch@fit$sigma

Sdr_SP %>% jarque.bera.test()

KS = Sdr_SP %>% e1071::kurtosis() + 3

(sqrt(KA)*sum(coef(SP_garch)[c(3,4)]) - (1-coef(SP_garch)[c(5)])) %>% as.numeric()


Sdr_AMD = AMD_garch@fit$residuals / AMD_garch@fit$sigma

Sdr_AMD %>% jarque.bera.test()

KA = Sdr_AMD %>% e1071::kurtosis() + 3

(sqrt(KA)*coef(SP_garch)[c(3)] - (1-sum(coef(SP_garch)[c(4,5)]))) %>% as.numeric()


Sdr_NVIDIA = NVIDIA_garch@fit$residuals / NVIDIA_garch@fit$sigma

Sdr_NVIDIA %>% jarque.bera.test()

KN = Sdr_NVIDIA %>% e1071::kurtosis() + 3

(sqrt(KN)*coef(SP_garch)[c(3)] - (1-sum(coef(SP_garch)[c(4,5)]))) %>% as.numeric()
