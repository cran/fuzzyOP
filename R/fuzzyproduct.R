`fuzzyproduct` <-
function(anz,A="input",vektor,pl)
{
 u<-length(vektor);
 w=1/anz;               
 d<-c(seq(0,1, by=w));  
 delta<-length(d);
 M<-fuzzynumber(anz,A,vektor,0);
 prodkt<-matrix(1,2*delta,2);
 #PO<-matrix(0,2*delta,2);
 tt<-Inf;
 ss<-(-Inf);
 for(j in 1:(u))
  {
   for(i in 1:delta)   
    {
     v=min(prodkt[i,1]*M[2*delta-i+1,2*j-1],prodkt[2*delta-i+1,1]*M[2*delta-i+1,2*j-1],prodkt[i,1]*M[i,2*j-1],prodkt[2*delta-i+1,1]*M[i,2*j-1]);
     w=max(prodkt[i,1]*M[2*delta-i+1,2*j-1],prodkt[2*delta-i+1,1]*M[2*delta-i+1,2*j-1],prodkt[i,1]*M[i,2*j-1],prodkt[2*delta-i+1,1]*M[i,2*j-1]);
     prodkt[i,1]=v;
     prodkt[2*delta-i+1,1]=w;
    }
   tt=min(M[,2*j-1],tt);
   ss=max(M[,2*j-1],ss);
  }
 prodkt[,2]=M[,2];
 if (pl == 1)                  
  {
   limx=c(prodkt[1,1]-1,prodkt[2*delta,1]+1);
   limy=c(0,1.1);
   plot(c(prodkt[1,1]-10^5,prodkt[,1],prodkt[2*delta,1]+10^5),c(0,prodkt[,2],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="product of fuzzy numbers");
   abline(h=1,lty=2);
   axis(2,yaxp=c(0,1,5));
  }                                                 
 if (pl == 2)                
  {
   par(mfrow=c(2,1));
   limx=c(min(prodkt[1,1],tt)-1,max(prodkt[2*delta,1],ss)+1);
   limy=c(0,1.2);
   for(i in 1:u)
    {
     if(i==1) plot(c(min(prodkt[1,1],tt)-10^5,M[,2*i-1],max(prodkt[2*delta,1],ss)+10^5),c(0,M[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="fuzzy numbers")
     else lines(c(min(prodkt[1,1],tt)-10^5,M[,2*i-1],max(prodkt[2*delta,1],ss)+10^5),c(0,M[,2*i],0),type="l")
     abline(h=1,lty=2);
     axis(2,yaxp=c(0,1,2));
    }
   plot(c(min(prodkt[1,1],tt)-10^5,prodkt[,1],max(prodkt[2*delta,1],ss)+10^5),c(0,prodkt[,2],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="product of fuzzy numbers");
   abline(h=1,lty=2);
   axis(2,yaxp=c(0,1,2));
  }
 fuzzyproduct<-prodkt;
}

