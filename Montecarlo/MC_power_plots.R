setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(ggplot2,tidyverse)
source("../Multiplot.R")

load("Powerdf5.Rdata")


CL <- h2[[1]]
CSL <- h2[[2]]
Power <- cbind(CL,CSL) %>% as.data.frame(); colnames(Power) <- c("CL_Power","CL_Spur","CSL_Power","CSL_Spur")

rr <- 2.5 ; rs <- seq(from = -rr, to = rr,by = 0.1)
#plot(x = rs, y = Power$CL_Power,type = "l",col = "blue",ylim = c(0,1))
#lines(x = rs,y = Power$CSL_Power,col = "red")


p1 <- ggplot(data = Power[,c(1,3)]) + 
  geom_line(aes(x = rs,y=Power$CL_Power,colour = "CL"),size = 1) + 
  geom_line(aes(x=rs,y=Power$CSL_Power,colour = "CSL"),linetype="dashed",size = 1) +
  scale_y_continuous(limits = c(0.8,1)) + xlab("r") + ylab("Rejection rate") +
  scale_color_manual(values = c("#FF0000", "#282088")) +
  theme(legend.title=element_blank()) + ggtitle("True Power") +
  theme(legend.position="bottom")

p2 <- ggplot(data = Power[,c(2,4)]) + 
  geom_line(aes(x = rs,y=Power$CL_Spur,colour = "CL"),size = 1) + 
  geom_line(aes(x=rs,y=Power$CSL_Spur,colour = "CSL"),linetype="dashed",size = 1) +
  scale_y_continuous(limits = c(0,1)) + xlab("r") + ylab("Rejection rate") +
  scale_color_manual(values = c("#FF0000", "#282088")) +
  theme(legend.title=element_blank()) + ggtitle("Spurious Power") + 
  theme(legend.position="bottom")

multiplot(p1,p2,cols = 2)



load("MC_power_t.Rdata")


CL_t <- h[[1]]
CSL_t <- h[[2]]
Power_t <- cbind(CL_t,CSL_t) %>% as.data.frame(); colnames(Power_t) <- c("CL_Power","CL_Spur","CSL_Power","CSL_Spur")


p3 <- ggplot(data = Power_t[,c(1,3)]) + 
  geom_line(aes(x = rs,y=CL_Power,colour = "CL"),size = 1) + 
  geom_line(aes(x=rs,y=CSL_Power,colour = "CSL"),linetype="dashed",size = 1) +
  scale_y_continuous(limits = c(0.5,1)) + xlab("r") + ylab("Rejection rate") +
  scale_color_manual(values = c("#FF0000", "#282088")) +
  theme(legend.title=element_blank()) + ggtitle("True Power") +
  theme(legend.position="bottom")

p4 <- ggplot(data = Power_t[,c(2,4)]) + 
  geom_line(aes(x = rs,y=CL_Spur,colour = "CL"),size = 1) + 
  geom_line(aes(x=rs,y=CSL_Spur,colour = "CSL"),linetype="dashed",size = 1) +
  scale_y_continuous(limits = c(0,1)) + xlab("r") + ylab("Rejection rate") +
  scale_color_manual(values = c("#FF0000", "#282088")) +
  theme(legend.title=element_blank()) + ggtitle("Spurious Power") + 
  theme(legend.position="bottom")


multiplot(p3,p4,cols = 2)


p1x <- ggplot(data = Power[,c(1,3)]) + 
  geom_line(aes(x = rs,y=Power$CL_Power,colour = "CL"),size = 1) + 
  geom_line(aes(x=rs,y=Power$CSL_Power,colour = "CSL"),linetype="dashed",size = 1) +
  scale_y_continuous(limits = c(0.5,1)) + xlab("r") + ylab("Rejection rate") +
  scale_color_manual(values = c("#FF0000", "#282088")) +
  theme(legend.title=element_blank()) + ggtitle("True Power") +
  theme(legend.position="bottom")


multiplot(p3,p1x,cols=2)


