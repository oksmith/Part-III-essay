# We want to get the ROC information, then compute the AUC (adjusting for the
# maximum FPR amongst the paths). Then, take an average over 100 runs so that 
# we get an accurate idea of how the estimator performs.
nruns<-100
auc=matrix(0,nrow=nruns,ncol=1)
for (t in 1:nruns){
  # Generate MVN data and true structure
  source('~/Tripos/III/[Essay] Graphical Modelling for High-Dimensional Data/R scripts/Generate.R')
  out.tiger = sugm(Gaussdata,method="tiger",nlambda=30) #perform CLIME
  h<-huge.roc(out.tiger$path,a)
  auc[t]<-h$AUC + h$tp[30]*(1-h$fp[30]) + 0.5*(1-h$tp[30])*(1-h$fp[30])
  # this adjusts for the maximum TPR and FPR on the curve, by filling the rest in linearly
  print(t)
  }
mean_area<-mean(auc)
median_area<-median(auc)
print(mean_area)
print(median_area)