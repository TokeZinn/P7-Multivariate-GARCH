pacman::p_load(rmgarch,parallel)

#https://www.r-bloggers.com/the-garch-dcc-model-and-2-stage-dccmvt-estimation/

data(dji30retw)

Dat = dji30retw[, 1:10, drop = FALSE]

# define a DCCspec object: 2 stage estimation should usually always use
# Normal for 1-stage (see below for
xspec = ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                   variance.model = 
                     list(garchOrder = c(1,1), 
                          model = 'eGARCH'), 
                   distribution.model = 'norm')

uspec = multispec(replicate(10, xspec))
spec1 = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')

cl = makePSOCKcluster(10)
multf = multifit(uspec, Dat, cluster = cl)

fit1 = dccfit(spec1, data = Dat, fit.control = list(eval.se = TRUE), fit = multf, cluster = cl)


stopCluster(cl)
