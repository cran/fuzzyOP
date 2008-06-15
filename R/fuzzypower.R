`fuzzypower` <-
function(anz,A="input",vektor,pot,pl)
{
 u<-length(vektor);
 w=1/anz;               
 d<-c(seq(0,1, by=w));  
 delta<-length(d);
 M<-fuzzynumber(anz,A,vektor,0);
 prodkt<-matrix(1,2*delta,2*u);
 tt<-Inf;
 tt1<-Inf;
 ss<-(-Inf);
 ss1<-(-Inf);
 for (k in 1:u)
  {
   for(j in 1:(pot))
    {
     for(i in 1:delta)
      {
       v=min(prodkt[i,2*k-1]*M[2*delta-i+1,2*k-1],prodkt[2*delta-i+1,2*k-1]*M[2*delta-i+1,2*k-1],prodkt[i,2*k-1]*M[i,2*k-1],prodkt[2*delta-i+1,2*k-1]*M[i,2*k-1]);
       z=max(prodkt[i,2*k-1]*M[2*delta-i+1,2*k-1],prodkt[2*delta-i+1,2*k-1]*M[2*delta-i+1,2*k-1],prodkt[i,2*k-1]*M[i,2*k-1],prodkt[2*delta-i+1,2*k-1]*M[i,2*k-1]);
       prodkt[i,2*k-1]=v;
       prodkt[2*delta-i+1,2*k-1]=z;
      }
    }
   prodkt[,2*k]=M[,2*k];
   tt=min(M[,2*k-1],tt);
   tt1=min(prodkt[,2*k-1],tt1);
   ss=max(M[,2*k-1],ss);
   ss1=max(prodkt[,2*k-1],ss1);
  }
 if (pl == 1)
  {
   limx=c(tt1-1,ss1+1);
   limy=c(0,1.1);
   for (i in 1:u)
    {
     if(i==1) plot(c(min(tt1,tt)-10^5,prodkt[,2*i-1],max(ss1,ss)+10^5),c(0,prodkt[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="power of fuzzy numbers")
     else lines(c(min(tt1,tt)-10^5,prodkt[,2*i-1],max(ss1,ss)+10^5),c(0,prodkt[,2*i],0),type="l")
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
     if(i==1) plot(c(min(tt1,tt)-10^(5),M[,2*i-1],max(ss1,ss)+10^(5)),c(0,M[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="fuzzy numbers")
     else lines(c(min(tt1,tt)-10^(5),M[,2*i-1],max(ss1,ss)+10^(5)),c(0,M[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
   for (i in 1:(u))
    {
     if(i==1) plot(c(min(tt1,tt)-10^5,prodkt[,2*i-1],max(ss1,ss)+10^5),c(0,prodkt[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="power of fuzzy numbers")
     else lines(c(min(tt1,tt)-10^5,prodkt[,2*i-1],max(ss1,ss)+10^5),c(0,prodkt[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
  }
 fuzzypower<-prodkt;
}

