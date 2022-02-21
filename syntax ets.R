
library(readxl) # packages untuk membaca data pada excel dengan format .xlsx
datakuisl <- read_excel("E:/KULIAH/ANALISIS SURVIVAL/datakuisl.xlsx") # nama data : datakuisl.xlsx
View(datakuisl) # menampilkan data yang digunakan

library(survival) #packages untuk fungsi fungsi survival
Y=Surv(datakuisl$data,datakuisl$stat==1) 
kmfit1=survfit(Y~datakuisl$ket)
summary(kmfit1)
plot(kmfit1, lty = c("solid", "dashed"), col=c("black","red"),
     xlab="time",ylab="s^(t)")
legend("topright", c("Negative","positive"),lty = c("solid", "dashed"), col=c("black","red"))

survdiff(Surv(datakuisl$data,datakuisl$stat)~datakuisl$ket)


med <- survfit(Surv(datakuisl$data, datakuisl$stat) ~ datakuisl$ket) ; med

# Regression
# Exponential Dist
exp <- survreg(formula = Surv(data, stat) ~ ket, data = datakuisl, dist = "exponential" )
summary(exp)
aic.exp <- -2*exp$loglik ; aic.exp

# Weibull Dist
weib <- survreg(formula = Surv(data, stat) ~ ket, data = datakuisl, dist = "weibull" )
summary(weib)
aic.weib <- -2*weib$loglik ; aic.weib

# Log-Logistic Dist
logi <- survreg(formula = Surv(data, stat) ~ ket, data = datakuisl, dist = "loglogistic" )
summary(logi)
aic.logi <- -2*logi$loglik ; aic.logi

# Log-Normal Dist
lognorm <- survreg(formula = Surv(data, stat) ~ ket, data = datakuisl, dist = "lognormal" )
summary(lognorm)
aic.lognorm <- -2*lognorm$loglik ; aic.lognorm
