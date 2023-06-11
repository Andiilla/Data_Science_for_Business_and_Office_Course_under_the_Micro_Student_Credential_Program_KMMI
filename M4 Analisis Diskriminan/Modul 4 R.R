library(Hmisc)
library(mvShapiroTest)
library(MVTests)
library(MASS)
library(rrcov)
dt.disk<-read.csv(file.choose(),header=T,sep=",",dec=".") 
dt.disk1<-dt.disk[1:40,] 
dt.disk2<-dt.disk[41:50,]  
dt.disk1 
uji.multi<-rcorr(as.matrix(dt.disk1[,-5],type="pearson")) 
uji.multi
x <- dt.disk1[,-5] 
cm <- colMeans(x)
S <- cov(x) 
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm)) 
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 10), + 
         sd <- sort(d), + 
         xlab = expression(paste(chi[10]^2, " Quantile")), +
         ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1)) 
??Ordered distances
?Ordered distances
plot(qc<-qchisq((1:nrow(x)-1/2)/nrow(x),df=10), +
 sd<-sort(d), + 
   xlab=expression(paste(chi[10]^2,"Quantile")), + 
   ylab=("Ordered Distances"), xlim=range(qc)*c(1,1.1)

library(MVN)
install.packages(MVN)
x <- dt.disk1[,-5] 
>cm <- colMeans(x) 
> S <- cov(x) 
> d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm)) 
> plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 10), 
       + sd <- sort(d), + xlab = expression(paste(chi[10]^2, " Quantile")), 
       + ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 10),
     + sd <- sort(d), 
     + xlab = (expression(paste(chi[10]^2), "Quantile")),
     + ylab = "Ordereb distances", xlim = range(qc)*c(1,1.1))
> plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 10),
       + sd <- sort(d),
       + xlab = (expression(paste(chi[10]^2), "Quantile")),
       + ylab = "Ordereb distances", xlim = range(qc)*c(1,1.1))
uji.mvn<-mvShapiro.Test(as.matrix(dt.disk1[,-5])) 
uji.mvn
Generalized Shapiro-Wilk test for Multivariate Normality by Villasenor-Alva and
Gonzalez-Estrada

data:  as.matrix(dt.disk1[, -5])
MVW = 0.93973, p-value = 6.07e-07
uji.KMV<-BoxM(dt.disk1[,-11],dt.disk1[,11]) 
uji.KMV
uji.KVM<-manova(cbind(Bersih,Harga,Kasir,Lengkap,AC,Diskon,Parkir,Staf,Citra)~Belanja,data=dt.disk1) 
summary(uji.KVM) 
model.disk<-lda(Belanja~Bersih+Harga+Kasir+Lengkap+AC+Diskon+Parkir+Staf+Citra,data=dt.disk1) 
model.disk 

model.disk<-lda(Belanja~Bersih+Harga+Kasir+Lengkap+AC+Diskon+Parkir+Staf+Citra,data = dt.disk1)
model.disk
plot(model.disk)
lda.klasik<-LdaClassic(dt.disk1[,-5],dt.disk1[,5]) 
pred.klas<-predict(lda.klasik) 
pred.klas
dt.baru<-dt.disk2[,-5] 
pred.class<-predict(model.disk,dt.baru)$class 
cbind(dt.baru,pred.class
dt.baru<-dt.disk1[,-5]      
pred.class<-predict(model.disk,dt.baru)$class
pred.class<-predict(model.disk,dt.baru)$class
dt.baru<-dt.disk1[-5,]
pred.class<-predict(model.disk,dt.baru)$class
cbind(dt.baru,pred.class)

> 