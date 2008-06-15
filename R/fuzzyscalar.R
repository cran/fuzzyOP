`fuzzyscalar` <-
function(anz,A="input",vektor,v,pl)
{
 u<-length(vektor);
 w=1/anz;
 d<-c(seq(0,1, by=w));   
 delta<-length(d);
 M<-fuzzynumber(anz,A,vektor,0);
 scalar<-matrix(1,2*delta,2*u);
 tt<-Inf;
 tt1<-Inf;
 ss<-(-Inf);
 ss1<-(-Inf);
 for (i in 1:u)
  {
   scalar[,2*i-1]=v*M[,2*i-1];
   tt=min(M[,2*i-1],tt);
   ss=max(M[,2*i-1],ss);
   tt1=min(scalar[,2*i-1],tt1);
   ss1=max(scalar[,2*i-1],ss1);
   scalar[,2*i]=M[,2*i];
  }
 if (pl == 1)           
  {
   limx=c(tt1-1,ss1+1);
   limy=c(0,1.1);
   for (i in 1:u)
    {
     if(i==1) plot(c(min(tt1,tt)-10^5,scalar[,2*i-1],max(ss1,ss)+10^5),c(0,scalar[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="product of fuzzy numbers with a scalar")
     else lines(c(min(tt1,tt)-10^5,scalar[,2*i-1],max(ss1,ss)+10^5),c(0,scalar[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,5));
    }
  }
 if (pl == 2)
  {
   par(mfrow=c(2,1));
   limx=c(min(tt1,tt)-1,max(ss1,ss)+1);
   limy=c(0,1.2);
   for (i in 1:u)
    {
     if(i==1) plot(c(min(tt1,tt)-10^5,M[,2*i-1],max(ss1,ss)+10^5),c(0,M[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="fuzzy numbers")
     else lines(c(min(tt1,tt)-10^5,M[,2*i-1],max(ss1,ss)+10^5),c(0,M[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
   for (i in 1:u)
    {
     if(i==1) plot(c(min(tt1,tt)-10^5,scalar[,2*i-1],max(ss1,ss)+10^5),c(0,scalar[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",col=1,xlab="x",ylab="Char.f",main="product of fuzzy numbers with a scalar")
     else lines(c(min(tt1,tt)-10^5,scalar[,2*i-1],max(ss1,ss)+10^5),c(0,scalar[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
  }
 fuzzyscalar<-scalar;
}

