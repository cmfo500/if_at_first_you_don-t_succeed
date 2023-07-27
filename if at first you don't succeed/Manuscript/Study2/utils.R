# Determines whether libraries need to be installed
  ## Source of ipak function: https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Disattenuation formula 
Disat <- function(rxy) {
  rxx = .29
  ryy = .70
  
  Rxy <- rxy /sqrt(rxx*ryy)
  return(Rxy)
}

### Transform fisher z back to r
ztor<- function(z){
  ((exp(1)^(2*z))-1)/((exp(1)^(2*z))+1)
}

# Transform Spearman-Brown correction into Pearson's r
Spearman_Brown_to_Pearson <- function(x) {x/(2-x)} #Horst(1951)


# Holm-Bonferroni method

HB.correction <- function(alpha, p.value){
  n <- length(na.omit(p.value))
  HB.pvalue <- alpha/(n - rank(p.value) + 1)
  
}

#function for eliminating outlier datapoints

remove_outliers <-  function(x) {
  mn <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  (x > mn + sd * 2.5) | (x < mn - sd * 2.5)
}
