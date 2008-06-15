`fuzzynumber` <-
function(anz,A="input",vektor,pl)
{
 gis<-dim(A);
 u<-length(vektor);
 w=1/anz;              
 d<-c(seq(0,1, by=w));    
 e<-c(seq(1,0,by=-w));
 delta<-length(d);
 tt<-Inf;
 M<-matrix(0,2*delta,2*u);
 ss<-(-Inf);
 a_i<-as.vector(1:delta);    
 b_i<-as.vector(1:delta);     
 f_i<-as.vector(1:2);
 H<-matrix(0,gis[[1]],2*u);
 for (i in 1:u)
  {
   m=vektor[i];
   H[,2*i-1]<-A[,2*m-1];
   H[,2*i]<-A[,2*m];
  }
 g<-fuzzycheck(H);
 if(g==1)
  {
   for (i in 1:u) 
    {
     m=vektor[i];          
     x_i<-A[,2*m-1];
     y_i<-A[,2*m];
     x_i<-na.omit(x_i);
     y_i<-na.omit(y_i);
     n_i<-length(x_i);     
     k<-1;
     for(l in 1:((n_i)-1))               
      {
       for(j in 1:delta)    
        {
         if(d[j]>=y_i[l] & d[j]<y_i[l+1])   
          {
           a_i[j]=(x_i[l+1]-x_i[l])/(y_i[l+1]-y_i[l])*(d[j]-y_i[l])+x_i[l];
           a_i[1]=x_i[1];
          }
         if(d[j]<=y_i[l] & d[j]>y_i[l+1])   
          {
           b_i[delta-j+1]=(x_i[l+1]-x_i[l])/(y_i[l+1]-y_i[l])*(d[j]-y_i[l])+x_i[l];
           b_i[delta]=x_i[n_i];
          }
        }
       if(y_i[l]==1)    
        {
         f_i[k]=x_i[l];
         f_i[k+1]=x_i[l];
         k=k+1;
        }
       a_i[delta]=f_i[1];
       b_i[1]=f_i[2];
      }
     M[,2*i]<-c(d,e);
     M[,2*i-1]<-c(a_i,b_i);
     tt<-min(M[,2*i-1],tt);
     ss<-max(M[,2*i-1],ss);
    }
   if (pl == 1)                       
    {
     limx=c(tt-1,ss+1);
     limy=c(0,1.1);
     for (i in 1:u)
      {
       if(i==1) plot(c(tt-10^(5),M[,2*i-1],ss+10^(5)),c(0,M[,2*i],0),xlim=limx,ylim=limy,yaxt="n",type="l",xlab="x",ylab="Char.f",main="fuzzy numbers")
       else lines(c(tt-10^(5),M[,2*i-1],ss+10^(5)),c(0,M[,2*i],0),type="l")
       abline(h=1,lty=2);
       axis(2,yaxp=c(0,1,5));
      }
    }
   fuzzynumber<-M;
  }
}

