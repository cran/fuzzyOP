`fuzzymin` <-
function(anz,A="input",vektor,pl)
{
 u<-length(vektor);
 w=1/anz;                
 d<-c(seq(0,1, by=w));   
 delta<-length(d);
 M<-fuzzynumber(anz,A,vektor,0);
 ix<-matrix(NA,2*delta,2);
 tt<-Inf;
 ss<-(-Inf);
 for (i in 1:u)
  {
   ix[,1]=pmin(ix[,1],M[,2*i-1],na.rm=TRUE);
   tt=min(M[,2*i-1],tt);
   ss=max(M[,2*i-1],ss);
  }
 ix[,2]<-M[,2];
 if (pl == 1)                       
  {
   limx=c(ix[1,1]-1,ix[2*delta,1]+1);
   limy=c(0,1.1);
   plot(c(ix[1,1]-10^5,ix[,1],ix[2*delta,1]+10^5),c(0,ix[,2],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="minimum of fuzzy numbers");
   abline(h=1,lty=2);
   axis(2,yaxp=c(0,1,5));
  }
 if (pl == 2)                   
  {
   par(mfrow=c(2,1));
   limx=c(min(ix[1,1],tt)-1,max(ix[2*delta,1],ss)+1);
   limy=c(0,1.2);
   for(i in 1:u)
    {
     if(i==1) plot(c(min(ix[1,1],tt)-10^5,M[,2*i-1],max(ix[2*delta,1],ss)+10^5),c(0,M[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="fuzzy numbers")
     else lines(c(min(ix[1,1],tt)-10^5,M[,2*i-1],max(ix[2*delta,1],ss)+10^5),c(0,M[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
   plot(c(min(ix[1,1],tt)-10^5,ix[,1],max(ix[2*delta,1],ss)+10^5),c(0,ix[,2],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="minimum of fuzzy numbers");
   abline(h=1,lty=2);
   axis(2,yaxp=c(0,1,2));
  }
 fuzzymin<-ix;
}

