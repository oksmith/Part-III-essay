# We want to get the ROC information, then compute the AUC (adjusting for the
# maximum FPR amongst the paths). Then, take an average over 100 runs so that 
# we get an accurate idea of how the estimator performs.
nruns<-100
auc=matrix(0,nrow=nruns,ncol=1)
for (t in 1:nruns){
  # Generate MVN data and true structure
  source('~/Tripos/III/[Essay] Graphical Modelling for High-Dimensional Data/R scripts/Generate.R')
  out.NS<-huge(Gaussdata,nlambda=30)   #perform NS
  h<-huge.roc(out.NS$path,a)
  auc[t]<-h$AUC + h$tp[30]*(1-h$fp[30]) + 0.5*(1-h$tp[30])*(1-h$fp[30])  
    # this adjusts for the maximum FPR on the curve
  print(t)
}

mean_area<-mean(auc)
median_area<-median(auc)
auc2<-(auc-rep(mean_area,100))^2
sigma<-(1/100)*sum(auc2)
print(mean_area)
print(median_area)
print(sigma)