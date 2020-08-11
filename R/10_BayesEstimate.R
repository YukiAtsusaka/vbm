################################################################################################
# 10_BayesEstimate.R
# Created by Yuki Atsusaka
# Since 6/18/2020
# Last updated 6/18/2020
# Aim: to compute the posterior belief on the VBM effect given previous findings and ours
################################################################################################

################################################################################################


rm(list=ls())
library("ggsci")
library("bayestestR")
library("RColorBrewer")


pdf("Bayes.pdf",width=10, height=7)
par(mar=c(3,3,1.2,1.2), mfrow=c(2,1))

################################################################################################
# (NC: 1) COMPUTE POSTERIOR GIVEN PRIOR AND LIKELIHOOD
################################################################################################

prior_mu = 0.041  # (0.018 + 0.064)/2 mid point of Lower and Upper estimates (except Bonica)
prior_sigma = 0.03

n_data = 1    
data_mu = 0.057
data_sigma = 0.003

post_mu = ((prior_mu/prior_sigma^2) + ((n_data * data_mu)/data_sigma^2))/
         ((1/prior_sigma^2) + (n_data/data_sigma^2))
  
post_sigma = sqrt(1/((1/prior_sigma^2) + (n_data/data_sigma^2)))


################################################################################################
# (NC: 2) VISUALIZE PRIOR, LIKELIHOOD, AND POSTERIOR
################################################################################################
set.seed(618)
y = seq(from=-0.02, to=0.15, by=0.001)  # to center plot on posterior

y_prior = dnorm(y, prior_mu, prior_sigma)
y_lik   = dnorm(y, data_mu,  data_sigma)
y_post  = dnorm(y, post_mu,  post_sigma)

# 89% credible interval
qnorm(post_mu,  post_sigma, p=0.055) # Lower 
qnorm(post_mu,  post_sigma, p=0.945) # Upper

y_max = max(c(y_prior, y_lik, y_post)) # For Y-axis
library(wesanderson)
pal = c("gray30", "gray30", "firebrick4")


plot(y, y_prior, type = "n", xlim = c(0, 0.105), ylim = c(0, y_max),
         ylab = "density", xlab="Total VBM Effect", main="North Carolina as Control", cex.mai=1.2)

# PRIOR, LIKELIHOOD, POSTERIOR
lines(y, y_prior, col = pal[1], lty = 2, lwd = 2)
lines(y, y_lik, col = pal[2], lwd = 4)
lines(y, y_post, col = pal[3], lwd = 5, lty=2)
text(x=0.078, y=80, labels="0.057 (sd=0.003)", col=pal[2], cex=1.1)
arrows(x0=0.068, x1=0.063, y0=74,y1=60, length=0.1,lwd=1.5,col=pal[2])
text(x=0.025, y=40, labels="0.041 (sd=0.03)", col=pal[1], cex=1.1)
arrows(x0=0.03, x1=0.036, y0=35,y1=16, length=0.1,lwd=1.5,col=pal[1])
text(x=0.03, y=75, labels="0.057\n89% CI (0.052, 0.062)", col=pal[3], cex=1.1)
arrows(x0=0.043, x1=0.05, y0=66,y1=60, length=0.1,lwd=1.5,col=pal[3])
#arrows(x0=0.041, x1=0.041, y0=0,y1=12, length=0,lty=2,col="gray50")
#arrows(x0=0.057, x1=0.057, y0=0,y1=125, length=0,lty=2,col=pal[3])

# PRIOR STUDIES
arrows(x0=0.018,x1=0.018, y0=20, y1=0, lwd=1, length=0.1, col="black")
text(x=0.018, y=25, labels="Barber&Holbein ('20)", cex=0.8, col="black")
arrows(x0=0.06,x1=0.06, y0=20, y1=0, lwd=1, length=0.1, col="black")
text(x=0.068, y=25, labels="Barber&Holbein ('20)", cex=0.8, col="black")
arrows(x0=0.094,x1=0.094, y0=10, y1=0, lwd=1, length=0.1, col="black")
text(x=0.094, y=15, labels="Bonica et al. ('20)", cex=0.8, col="black")
arrows(x0=0.02,x1=0.02, y0=10, y1=0, lwd=1, length=0.1, col="black")
text(x=0.02, y=15, labels="Thompson et al. ('20)", cex=0.8, col="black")
arrows(x0=0.064,x1=0.064, y0=10, y1=0, lwd=1, length=0.1, col="black")
text(x=0.07, y=15, labels="Gerber et al. ('13)", cex=0.8, col="black")


legend("topleft", col = pal, lty = c(2, 1, 2), lwd=c(1.2,2,3), 
       cex = 1, bty = "n",
       legend = c("Prior (previous studies)", 
                  "Likelihood (our finding)",
                  "Posterior (updated belief)"))


################################################################################################
# (NM: 1) COMPUTE POSTERIOR GIVEN PRIOR AND LIKELIHOOD
################################################################################################

prior_mu = 0.041  # (0.018 + 0.064)/2 mid point of Lower and Upper estimates (except Bonica)
prior_sigma = 0.03

n_data = 1    
data_mu = 0.05
data_sigma = 0.005

post_mu = ((prior_mu/prior_sigma^2) + ((n_data * data_mu)/data_sigma^2))/
         ((1/prior_sigma^2) + (n_data/data_sigma^2))
  
post_sigma = sqrt(1/((1/prior_sigma^2) + (n_data/data_sigma^2)))


################################################################################################
# (NM: 2) VISUALIZE PRIOR, LIKELIHOOD, AND POSTERIOR
################################################################################################
set.seed(618)
y = seq(from=-0.02, to=0.15, by=0.001)  # to center plot on posterior

y_prior = dnorm(y, prior_mu, prior_sigma)
y_lik   = dnorm(y, data_mu,  data_sigma)
y_post  = dnorm(y, post_mu,  post_sigma)

# 89% credible interval
qnorm(post_mu,  post_sigma, p=0.055) # Lower 
qnorm(post_mu,  post_sigma, p=0.945) # Upper


y_max = max(c(y_prior, y_lik, y_post)) # For Y-axis
library(wesanderson)
pal = c("gray30", "gray30", "firebrick4")


plot(y, y_prior, type = "n", xlim = c(0, 0.105), ylim = c(0, y_max),
         ylab = "density", xlab="Total VBM Effect", main="New Mexico as Control", cex.main=1.2)

# PRIOR, LIKELIHOOD, POSTERIOR
lines(y, y_prior, col = pal[1], lty = 2, lwd = 2)
lines(y, y_lik, col = pal[2], lwd = 4)
lines(y, y_post, col = pal[3], lwd = 5, lty=2)
text(x=0.07, y=45, labels="0.057 (sd=0.003)", col=pal[2], cex=1.1)
arrows(x0=0.065, x1=0.06, y0=40,y1=30, length=0.1,lwd=1.5,col=pal[2])
#text(x=0.025, y=40, labels="0.041 (sd=0.03)", col=pal[1], cex=1.1)
#arrows(x0=0.03, x1=0.036, y0=35,y1=16, length=0.1,lwd=1.5,col=pal[1])
text(x=0.03, y=45, labels="0.050\n89% CI (0.042, 0.058)", col=pal[3], cex=1.1)
arrows(x0=0.033, x1=0.04, y0=35,y1=30, length=0.1,lwd=1.5,col=pal[3])
#arrows(x0=0.041, x1=0.041, y0=0,y1=12, length=0,lty=2,col="gray50")
#arrows(x0=0.05, x1=0.05, y0=0,y1=125, length=0,lty=2,col=pal[3])

# PRIOR STUDIES
arrows(x0=0.018,x1=0.018, y0=20, y1=0, lwd=1, length=0.1, col="black")
text(x=0.018, y=25, labels="Barber&Holbein ('20)", cex=0.8, col="black")
arrows(x0=0.06,x1=0.06, y0=20, y1=0, lwd=1, length=0.1, col="black")
text(x=0.068, y=25, labels="Barber&Holbein ('20)", cex=0.8, col="black")
arrows(x0=0.094,x1=0.094, y0=10, y1=0, lwd=1, length=0.1, col="black")
text(x=0.094, y=15, labels="Bonica et al. ('20)", cex=0.8, col="black")
arrows(x0=0.02,x1=0.02, y0=10, y1=0, lwd=1, length=0.1, col="black")
text(x=0.02, y=15, labels="Thompson et al. ('20)", cex=0.8, col="black")
arrows(x0=0.064,x1=0.064, y0=10, y1=0, lwd=1, length=0.1, col="black")
text(x=0.07, y=15, labels="Gerber et al. ('13)", cex=0.8, col="black")


legend("topleft", col = pal, lty = c(2, 1, 2), lwd=c(1.2,2,3), 
       cex = 1, bty = "n",
       legend = c("Prior (previous studies)", 
                  "Likelihood (our finding)",
                  "Posterior (updated belief)"))


dev.off()


################################################################################################
# END OF THIS R SOURCE CODE
################################################################################################
  
