`fuzzycheck` <-
function(H="input")
{
 u=ncol(H)
 k=1;
 for(i in 1:(u/2))
  {
   n=0;
   h=0;
   m=length(na.omit(H[,(2*i-1)]));
   for(j in 1:(m-1))
    { 
     zeile=j;
     spaltex=(2*i-1); 
     spaltey=(2*i);
     if(is.na(H[j,2*i-1])==TRUE & is.na(H[j,2*i])==TRUE)
      {
       na.exclude(H[j,2*i-1]);
     na.exclude(H[j,2*i]);
     h=1;
      }
     if(h==0)
      {
       if(H[j,(2*i-1)]>H[j+1,(2*i-1)])
        {     
         cat("Input-Error: The columns" , c(spaltex,spaltey), "do not determine a fuzzy number! \n");
         cat("See help(fuzzyOP) \n" );
         k=0;
         break;
        }
       if(H[j,2*i]!=0 && j==1)
        {     
         cat("Input-Error: The columns" , c(spaltex,spaltey), "do not determine a fuzzy number! \n");
         cat("See help(fuzzyOP) \n" );
         k=0;
         break;
        }
       if(j+1==m && H[m,2*i]!=0)
        {   
         cat("Input-Error: The columns" , c(spaltex,spaltey), "do not determine a fuzzy number! \n");
         cat("See help(fuzzyOP) \n" );
         k=0;
         break;
        }
       else
        {
         if(n==0)
          { 
           if(H[j,2*i]<=H[j+1,2*i])
            {
             n=0;
             if(H[j+1,2*i]==1)
              {
               j=j+1;
               n=1;
              }           
            } 
           else
            {     
             cat("Input-Error: The columns" , c(spaltex,spaltey), "do not determine a fuzzy number! \n");
             cat("See help(fuzzyOP) \n" );
             k=0;
             break;
            }
          }  
         if(n==1)
          { 
           if(H[j,2*i]>=H[j+1,2*i])
            {
             n=1;
            }
           else
            {   
             cat("Input-Error: The columns" , c(spaltex,spaltey), "do not determine a fuzzy number! \n");
             cat("See help(fuzzyOP) \n" );
             k=0;
             break;
            }
          }
        }
      }
    }
  }
 fuzzycheck<-k;
}

