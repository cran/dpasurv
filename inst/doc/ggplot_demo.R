## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dpasurv)

## -----------------------------------------------------------------------------
data(simdata)

set.seed(1)

# Perform dynamic path analysis
s <- dpa(Surv(start,stop,event)~M+x, list(M~x), id="subject", data=simdata, boot.n=500)

# Calculate direct, indirect and total effects
direct <- effect(x ~ outcome, s)
indirect <- effect(x ~ M ~ outcome, s)
total <- sum(direct, indirect)

# Perform dynamic path analysis under multiple treatment arms:
s2 <- dpa(Surv(start,stop,event)~M+dose, list(M~dose), id="subject", data=simdata, boot.n=500)

# Calculate corresponding direct, indirect and total effects
direct2 <- effect(dose ~ outcome, s2)
indirect2 <- effect(dose ~ M ~ outcome, s2)
total2 <- sum(direct2, indirect2)

## -----------------------------------------------------------------------------
layout1x3 <- par(mfrow=c(1,3))
plot(direct); abline(h=0, lty=2, col=2)
plot(indirect); abline(h=0, lty=2, col=2)
plot(total); abline(h=0, lty=2, col=2)

# restore user's graphical parameters:
par(layout1x3)

## ---- eval=FALSE--------------------------------------------------------------
#  layout2x3 <- par(mfrow=c(2,3))
#  plot(direct2); abline(h=0, lty=2, col=2)
#  plot(indirect2); abline(h=0, lty=2, col=2)
#  plot(total2); abline(h=0, lty=2, col=2)
#  
#  # restore user's graphical parameters:
#  par(layout2x3)

## -----------------------------------------------------------------------------
ggplot.effect(indirect)

## -----------------------------------------------------------------------------
ggplot.effect(list(direct, indirect, total))

## -----------------------------------------------------------------------------
ggplot.effect(direct2)

## -----------------------------------------------------------------------------
ggplot.effect(list(direct2, indirect2, total2))

## -----------------------------------------------------------------------------
ggplot.effect(list(direct, indirect, total), 
              titles = c("Direct","Indirect","Total"),
              x_label = "Time (in years)", 
              y_label = "Custom y-label")

