setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,tictoc,parallel,mgarchBEKK,tidyverse,rugarch,
               matrixcalc,rmgarch,mvtnorm)
source("./DATA/DataAndReturnFct.R")
source("./montecarlo functions/DCCvUgarch_power.R")
source("./montecarlo functions/BEKKvCOV_power.R")
source("./montecarlo functions/BEKKvUgarch_power.R")
source("./montecarlo functions/UGARCHvBEKK_power.R")
source("../Rolling_BEKK.R")
source("./montecarlo functions/DCCvCOV_power.R")
DF <-  Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS <- Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()
df <- rbind(DF,OS)
end <- length(DF[,1]); end2 = length(OS[,1]) ; end3 <- length(df[,1])
colnames(Return_DF_OOS) <- colnames(Return_DF)
df_total <- rbind(Return_DF,Return_DF_OOS)

set.seed(100)
tic() ; Powers_BvC = BEKKvCOV_power(in.sample = df[(end3-750):(end3-500),],
                              out.sample = df[(end3-499):end3,],B = 1000,refit = 5,
                              optim = "Nelder-Mead"); toc()
save(Powers_BvC,file = "BEKKvCOV_result.Rdata")


set.seed(400)
tic() ; Powers_DCCvUgarch = DCCvUgarch_power(in.sample = df[(end3-750):(end3-500),],
                                    out.sample = df[(end3-499):end3,],B = 1000); toc()
stopCluster(cl)
save(Powers_DCCvUgarch,file = "DCCvUgarch_result.Rdata")


set.seed(200)
tic() ; Powers_BvU = BEKKvUgarch_power(in.sample = df[(end3-750):(end3-500),],
                                    out.sample = df[(end3-499):end3,],B = 1000,refit = 10,
                                    optim = "Nelder-Mead"); toc()
save(Powers_BvU,file = "BEKKvUgarch_result.Rdata")

set.seed(700)
tic() ; Powers_DCCvCOV = DCCvCOV_power(in.sample = df[(end3-750):(end3-500),],
                                       out.sample = df[(end3-499):end3,],B = 1000); toc()
stopCluster(cl)
save(Powers_DCCvCOV,file = "DCCvCOV_result.Rdata")

set.seed(700)
tic() ; Powers_UGARCHvBEKK = UGARCHvBEKK_power(in.sample = df[(end3-750):(end3-500),],
                                               out.sample = df[(end3-499):end3,],B = 1000,
                                               refit = 10,optim = "Nelder-Mead"); toc()
save(Powers_UGARCHvBEKK,file = "UGARCHvBEKK.Rdata")


