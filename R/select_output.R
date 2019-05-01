
select_output<-function(output,model=c(),pv_thold=1){
  
  if (length(model)==0){
    
  first_snp_r<-rownames(output[1][[1]][[1]]) 

  model_def<- ("(Intercept)" %in% first_snp_r)*1
  snp_num<-length(output)
  
  if (model_def==0){
      p_value_ouput<-function(x){
            R<- output[x][[1]][[1]][1,3]
            return(R)
       }
      pv_vec<-unlist(lapply(1:snp_num, p_value_ouput))
  } else {
     p_value_ouput<-function(x){
           R<- output[x][[1]][[1]][2,3]
           return(R)
      }
     pv_vec<-unlist(lapply(1:snp_num, p_value_ouput))
  }
  } else if (length(model)==1){
    
    
    
    
  } else {
    stop("Only one kind of SNP type model to be chosen.")
  }

  return(results)
}



