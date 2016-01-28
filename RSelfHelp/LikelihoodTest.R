#likelihood test
#-----------------------------------------------------------------
#there is no premade likelihood test in R
#make your own likelihood test!!
likelihood.test = function(x){
  nrows = dim(x) [1]  #number rows in contingency table
  ncols = dim(x) [2]  #number columns in contingency table
  chi.out = chisq.test(x,correct=F) #pearson chisq test
  table = chi.out[[6]] #get OF (observed freq)
  ratios = chi.out[[6]]/chi.out[[7]] #calculate OF/EF ratios
  sum = 0 #store test statistic
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sum = sum + table[i,j]*log(ratios[i,j])
    }
  }
  sum = 2*sum #likelihood ratio chi square
  df = chi.out [[2]] #calculate df
  p = 1 - pchisq(sum,df) #p-value
  out = c(sum, df, p, chi.out[[1]]) #output vector
  names(out) = c("LR-chisq", "df", "p-value", "Pears-chisq")
  round(out,4)
}

#test it
likelihood.test(sex.survived)