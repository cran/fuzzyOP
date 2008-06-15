`fuzzydeltacut` <-
function(del,A="input",vektor,pl)
{
 zum<-length(del);
 gis<-dim(A);
 u<-length(vektor);
 M<-matrix(0,zum*u,3, byrow=TRUE,dimnames=list(NULL,c("del","x1","x2")));
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
     for(r in 1:zum)
      {
       if (del[r]>=0 & del[r]<=1)
        {
         d<-c(seq(del[r],1,length=100));
         e<-c(seq(1,del[r],length=100));
         delta<-length(d);
         a_i<-as.vector(1:zum);
         b_i<-as.vector(1:zum);
         m=vektor[i];
         x_i<-A[,2*m-1];
         y_i<-A[,2*m];
         x_i<-na.omit(x_i);
         y_i<-na.omit(y_i);
         n_i<-length(x_i);
         for(l in 1:((n_i)-1))
          {
           if(del[r]>=y_i[l] & del[r]<y_i[l+1])
            {
             a_i=(x_i[l+1]-x_i[l])/(y_i[l+1]-y_i[l])*(del[r]-y_i[l])+x_i[l];
            }
           if(del[r]<=y_i[l] & del[r]>y_i[l+1])
            {
             b_i=(x_i[l+1]-x_i[l])/(y_i[l+1]-y_i[l])*(del[r]-y_i[l])+x_i[l];
            }
           if(del[r]==0 & y_i[l]==0 & y_i[l+1]>0) 
            {
             a_i=x_i[l];
            }
           if(del[r]==0 & y_i[l]>0 & y_i[l+1]==0) 
            {
             b_i=x_i[l+1];
            }
           if(del[r]==1 & y_i[l]<1 & y_i[l+1]==1) 
            {
             a_i=x_i[l+1];
            }
           if(del[r]==1 & y_i[l]==1 & y_i[l+1]<1) 
            {
             b_i=x_i[l];
            }
          }
         M[zum*(i-1)+r,]<-c(del[r],a_i,b_i);
        }
       else 
        {
         print("Input-Error: Del must have a value between 0 and 1!");
        }
      }
     cat("Delta cuts and the related x-values of Char.f of fuzzy number",i," \n");
     print(M[(zum*(i-1)+1):(zum*i),]);
    }
   if (pl == 1)
    {
     fuzzynumber(100,A,vektor,1);
    }
  }
 fuzzydeltacut<-M;
}

