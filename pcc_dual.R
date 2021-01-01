library(dplyr)
library(Hmisc)
library(tidyr)
library(tibble)
library(corrr)
library(corrplot) #visualisation


fmt_pcc<- read.table("dualrna2coexp.txt", header=T, sep="\t", row.names=1)

flat_corr_mat<- function(cor_r, cor_p){
  library(tidyr)
  library(tibble)
  cor_r<- rownames_to_column(as.data.frame(cor_r), var= "row")
  cor_r<- gather(cor_r, column, cor, -1)
  cor_p<- rownames_to_column(as.data.frame(cor_p), var= "row")
  cor_p<- gather(cor_p, column, p, -1)
  cor_p_matrix<- left_join(cor_r, cor_p, by=c("row", "column"))
  cor_p_matrix
}

corr_max<- rcorr(as.matrix(fmt_pcc[, 1:2156])) 

dual_pcc<- flat_corr_mat(corr_max$r, corr_max$P)

dual_corr0.7<- dual_pcc %>%
  filter(pcc >= 0.6) %>%
  filter(pvalue <= 0.05)

dual_corr0.1<- dual_pcc %>%
  filter(pcc >= 1.0)

***************************************

mydt_pcc<- cor(fmt_pcc, method = c("pearson"))

mydt_corr<- rcorr(as.matrix(mydt_pcc)) #to run the correlation matrix with p-values. Note that the data has to be fed to the rcorr function as a matrix.

mydt_coeff<- mydt_corr$r
mydt_pvalue<- mydt_corr$P

mydt_table<- flat_corr_mat(mydt_corr$r, mydt_corr$P)



