p<-20
n<-100;
s<-0.1;
g <- make_empty_graph(n = p)
prob=matrix(0,nrow=p,ncol=p)

y1<-runif(p,0,1);
y2<-runif(p,0,1);
for (i in 1:p){
  for (j in 1:p){
    prob[i,j]<-(1/sqrt(2*pi))*exp(-((y1[i]-y1[j])^2+(y2[i]-y2[j])^2)/(2*s))
      #probability of an edge between node i and node j
    u<-runif(1,0,1)
    if (u<=prob[i,j]){g<-add_edges(g,c(i,j, j,i))} #add edge between i,j with probability prob(i,j)
  }
}
g<-moralize(g)
a<-as_adjacency_matrix(g,type="both")
#restrict degree to at most 4-8 (to control sparsity)
for (i in 1:p){
  while(sum(a[i,1:p])>4) {
    tf<-(a[i,1:p]==1)
    w<-which(tf)#the nonzero entries in this row
    j<-sample(w,1)#pick a random index corresponding to a 1-entry
    a[i,j]=0; a[j,i]=0;#set it to be 0
  }
}
g1<-graph_from_adjacency_matrix(a,mode="undirected")
plot(g1,layout=layout.circle,main="Truth")

#Now turn a into a precision matrix, with non-zero (non-diagonal) entries
b=matrix(0,nrow=p,ncol=p)
for (i in 1:p){
  for (j in 1:p){
    if (a[i,j]==1){
      b[i,j]<-0.245}
    end}
  end
  b[i,i]<-1}
end 

#Invert and scale to obtain the correlation matrix
Sigma<-solve(b)
for (i in 1:p){
  for (j in 1:p){
    Sigma[i,j]<-Sigma[i,j]/sqrt(Sigma[i,i])
  }
  for (j in 1:p){
    Sigma[j,i]<-Sigma[j,i]/sqrt(Sigma[i,i])
  }
}
S<-(Sigma + t(Sigma))/2 #symmetrising
#Now simulate Multivariate Normal data
Gaussdata<-mvrnorm(n,mu=rep(0,p),Sigma=S)
