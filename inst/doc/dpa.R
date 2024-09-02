## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dpasurv)
library(ggplot2)

## ---- out.width = "630px", include=TRUE, fig.align="center", echo=FALSE, fig.cap="Figure 1. (a) A dynamic path diagram with treatment $Z$, a single mediator $M(t)$, and baseline confounders $X$; (b) Corresponding dynamic path diagram without the mediator process."----
knitr::include_graphics("../man/figures/tutorial_fig.png")

## -----------------------------------------------------------------------------
simdata

## ---- eval=FALSE--------------------------------------------------------------
#  # Perform dynamic path analysis
#  s <- dpa(Surv(start, stop, event) ~ M + x, list(M ~ x), id = "subject", data = simdata, boot.n = 500)
#  
#  # Extract cumulative direct and indirect effects with 95% pointwise bootstrap confidence bands:
#  direct <- effect(x ~ outcome, s, alpha=0.05)
#  indirect <- effect(x ~ M ~ outcome, s, alpha=0.05)
#  
#  # Use sum() method to obtain total effect
#  total <- sum(direct, indirect)
#  
#  # Plot the results
#  layout1x3 <- par(mfrow=c(1,3))
#  plot(direct); abline(h=0, lty=2, col=2)
#  plot(indirect); abline(h=0, lty=2, col=2)
#  plot(total); abline(h=0, lty=2, col=2)
#  
#  # restore user's graphical parameters:
#  par(layout1x3)
#  
#  # Plot the results with ggplot2 graphics:
#  ggplot.effect(list(direct, indirect, total))

## ---- message=FALSE, warning=FALSE, fig.width=5-------------------------------
ggplot(mapping=aes(x = start, y = M, group=dose), data=simdata) + geom_smooth(aes(colour=dose))

## ---- fig.width=5-------------------------------------------------------------
fit <- survival::survfit(Surv(start, stop, event) ~ dose, data=simdata)
plot(fit, col=2:4, xlab="Time", ylab="Survival", main="KM-curves by dose level")
legend(150, 1, c("control", "low dose", "high dose"), col=2:4, lwd=2, bty='n')

## -----------------------------------------------------------------------------
summary(survival::coxph(Surv(start, stop, event) ~ dose + M, data=simdata))

## -----------------------------------------------------------------------------
set.seed(1)
s <- dpa(Surv(start, stop, event) ~ M + dose, list(M ~ dose), id = "subject", data = simdata, boot.n = 500)

## -----------------------------------------------------------------------------
# The dose effect on mediator response:
dose.on.mediator <- effect(dose ~ M, s)

ggplot.effect(dose.on.mediator)

## -----------------------------------------------------------------------------
# direct effect of dose on outcome
direct <- effect(dose ~ outcome, s)

ggplot.effect(direct)

## -----------------------------------------------------------------------------
# effect of mediator on outcome
mediator.on.outcome <- effect(M ~ outcome, s)

ggplot.effect(mediator.on.outcome)

## -----------------------------------------------------------------------------
# Output from Aalen's additive model from the timereg::aalen() implementation:
summary(s$aalen)

## -----------------------------------------------------------------------------
# indirect effect of dose on outcome, mediated through M
indirect <- effect(dose ~ M ~ outcome, s)

# total effect
total <- sum(direct, indirect)

## ---- fig.width=7-------------------------------------------------------------
ggplot.effect(list(direct, indirect, total))

