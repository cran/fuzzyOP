`fuzzyfunction` <-
function(f,anz,A="input",vektor,pl)
{
 u<-length(vektor);
 w=1/anz;                
 d<-c(seq(0,1, by=w));  
 delta<-length(d);
 Z<-matrix(1,2*delta,2*u);
 M<-fuzzynumber(anz,A,vektor,0);
 tt<-Inf;
 ss<-(-Inf);
 tt1<-Inf;
 ss1<-(-Inf);
 for (i in 1:u)
  {
   for(j in 1:delta)
    {
     Z[j,2*i-1]<-min(f(M[j:(2*delta-j+1),2*i-1]));
     Z[2*delta-j+1,2*i-1]<-max(f(M[j:(2*delta-j+1),2*i-1]));
    }
   Z[,2*i]<-M[,2];
   tt<-min(M[,2*i-1],Z[,2*i-1],tt);
   ss<-max(M[,2*i-1],Z[,2*i-1],ss);
   tt1<-min(Z[,2*i-1],tt1);
   ss1<-max(Z[,2*i-1],ss1);
  }
 if (pl == 1)                  
  {
   limy=c(0,1.1);
   limx=c(tt1-1,ss1+1);
   for (i in 1:u)
    {
     if(i==1) plot(c(tt1-10^(5),Z[,2*i-1],ss1+10^(5)),c(0,Z[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="function of fuzzy numbers")
     else lines(c(tt1-10^(5),Z[,2*i-1],ss1+10^(5)),c(0,Z[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,5));
   }
  }
 if (pl == 2)                 
  {
   par(mfrow=c(2,1));
   limx=c(tt-1,ss+1);
   limy=c(0,1.2);
   for(i in 1:u)
    {
     if(i==1) plot(c(tt-10^(5),M[,2*i-1],ss+10^(5)),c(0,M[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="fuzzy numbers")
     else lines(c(tt-10^(5),M[,2*i-1],ss+10^(5)),c(0,M[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
   for (i in 1:u)
    {
     if(i==1) plot(c(tt-10^(5),Z[,2*i-1],ss+10^(5)),c(0,Z[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="function of fuzzy numbers")
     else lines(c(tt-10^(5),Z[,2*i-1],ss+10^(5)),c(0,Z[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
  }
 fuzzyfunction<-Z;
}

