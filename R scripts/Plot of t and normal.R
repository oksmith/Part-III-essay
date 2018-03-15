# Plot the pdf of multivariate t distribution as a function of Delta2 (Mahalabonis 
# distance), for various. Plot also that of a multivariate normal for comparison.
# Use Sigma=I, nu=3
Delta2<-seq(0,5,0.01);
p=39;
f_1<-(gamma((3+p)/2)/(gamma(1.5)*(3^(p/2))*(pi^(p/2))))*(1+(1/3)*Delta2)^(-(3+p)/2)
p=45;
f_2<-(gamma((3+p)/2)/(gamma(1.5)*(3^(p/2))*(pi^(p/2))))*(1+(1/3)*Delta2)^(-(3+p)/2)
p=60;
f_3<-(gamma((3+p)/2)/(gamma(1.5)*(3^(p/2))*(pi^(p/2))))*(1+(1/3)*Delta2)^(-(3+p)/2)

f<-(1/sqrt(2*pi))*exp(-0.5*Delta2)

plot(Delta2,f,type='l',col=1,xlim=c(0,5),ylim=c(0,0.6),xlab="Delta^2",ylab="f(Delta^2)",main="A comparison of PDFs")

points(Delta2,f_1,type='l',col=2)
points(Delta2,f_2,type='l',col=3)
points(Delta2,f_3,type='l',col=4)
legend(3.7,0.6,c("Normal","p=40","p=45","p=60"),col=c(1,2,3,4),lty=1)
