rm(list=ls())

setwd('C:/Users/ricar/OneDrive - ufmg.br/Mort Materna/ipc21/R/regiao')




### packages ######################################################################
library(dplyr)
library(matrixStats)
library(readr)

library(brms)
# methods(class = "brmsfit")




### data ##########################################################################
dat.macro <- read_delim('dados_modelo_2021.csv')
dim(dat.macro)
names(dat.macro)
# summary(dat.macro)
# dat.macro %>% mutate(rmm_ric=om_tot/nvtot*1e5) %>% select(nvtot,om_tot,rmm,rmm_ric,rmmest)



### fit ##########################################################################
ctrl_mt = 30
ctrl_ad = 0.95


# family = zero_inflated_poisson()


### Poisson
fit.pois <- brm(bf(om_tot ~ tbn + log(mpib20142018) + med_por1000_20142018 + mpsaude20142018 + offset(log(nvtot))),
                data = dat.macro, family = poisson(),
                control = list(max_treedepth = ctrl_mt, adapt_delta=ctrl_ad),
                chains = 1, iter = 2e4, warmup = 1e4, thin = 1)
summary(fit.pois)



# ### Negative Binomial
# fit.nb <- brm(bf(om_tot ~ tbn + log(mpib20142018) + med_por1000_20142018 + mpsaude20142018 + offset(log(nvtot))),
#               data = dat.macro, family = negbinomial(),
#               control = list(max_treedepth = ctrl_mt, adapt_delta=ctrl_ad),
#               chains = 1, iter = 2e4, warmup = 1e4, thin = 1)
# summary(fit.nb)



### Error
plot(dat.macro$rmmest-dat.macro$rmm, cex=0.7, ylim=c(-80,80), ylab='', xlab='macro')
points(predict(fit.pois,type='response')[,1]/dat.macro$nvtot*1e5 - dat.macro$rmm, pch=20, col='blue')
# points(predict(fit.nb,type='response')[,1]/dat.macro$nvtot*1e5 - dat.macro$rmm, pch=4, col='red', cex=0.5, lwd=2)
abline(0,0)




### fit (random effects) ###########################################################
ctrl_mt_re = 30
ctrl_ad_re = 0.95



### Poisson
fit.pois.re <- brm(bf(om_tot ~ tbn + log(mpib20142018) + med_por1000_20142018 + mpsaude20142018 + offset(log(nvtot)) + (1|macrocode)),
                   data = dat.macro, family = poisson(),
                   control = list(max_treedepth = ctrl_mt_re, adapt_delta=ctrl_ad_re),
                   chains = 1, iter = 5e4, warmup = 4e4, thin = 5)
summary(fit.pois.re)



# ### Negative Binomial
# fit.nb.re <- brm(bf(om_tot ~ tbn + log(mpib20142018) + med_por1000_20142018 + mpsaude20142018 + offset(log(nvtot)) + (1|macrocode)),
#                  data = dat.macro, family = negbinomial(),
#                  control = list(max_treedepth = ctrl_mt_re, adapt_delta=ctrl_ad_re),
#                  chains = 1, iter = 5e4, warmup = 4e4, thin = 5)
# summary(fit.nb.re)



### Error
plot(dat.macro$rmmest-dat.macro$rmm, cex=0.7, ylim=c(-80,80), ylab='', xlab='macro')
points(predict(fit.pois.re,type='response')[,1]/dat.macro$nvtot*1e5 - dat.macro$rmm, pch=20, col='blue')
# points(predict(fit.nb.re,type='response')[,1]/dat.macro$nvtot*1e5 - dat.macro$rmm, pch=4, col='red', cex=0.5, lwd=2)
abline(0,0)




### plot mmr
plot(dat.macro$rmmest-dat.macro$rmm, pch=1, cex=.7, lwd=1.5, ylim=c(-60,60),
     ylab='mmr_est - mmr_obs', xlab='macro', main='')
points(predict(fit.pois.re,type='response')[,1]/dat.macro$nvtot*1e5 - dat.macro$rmm, pch=20, col='blue')
abline(0,0, col='gray30')
legend(0,-40, bty='n', cex=1, pt.cex=c(0.7,1),
       c("resultado no arquivo dados_modelo_2021.csv",
         "modelo Poisson com efeito aleatorio na macroregiao"),
       lty=c(NA,NA),
       pch=c(1,20),
       lwd=c(1.5,1),
       col=c("black","blue"),
       text.col=c("black","darkblue"),)




### plot obitos
obt_est = predict(fit.pois.re,type='response')[,1]
obt_q0 = predict(fit.pois.re,type='response')[,3]
obt_q1 = predict(fit.pois.re,type='response')[,4]
x = 1:nrow(dat.macro)
plot(y=log(dat.macro$om_tot), x=x, ylim=c(0,8), lwd=2, col="darkred", pch=4, cex=0.8,
     ylab='log(deaths)', xlab='macro')
segments(x0=x, y0=log(obt_q0), y1=log(obt_q1), col='blue', lty='solid', lwd=0.5)
points(y=log(obt_est), x=x, pch=20, lwd=1.2, col='darkblue', cex=1)
legend(0, 8, bty='n', c('observados','estimados'), pt.cex=c(0.8,1),
       lty=c(NA,NA), pch=c(4,20), lwd=c(2,0.5),
       col=c('darkred','darkblue'),
       text.col=c('darkred','darkblue'))


write.table(x=cbind(dat.macro$macrocode, predict(fit.pois.re,type='response')[,c(1,3,4)]),
  file='C:/Users/ricar/OneDrive - ufmg.br/Mort Materna/ipc21/R/regiao/obitos_poisson.csv',
  col.names=c('macrocode','estimate','q25','q975'), row.names=F, sep=',')

