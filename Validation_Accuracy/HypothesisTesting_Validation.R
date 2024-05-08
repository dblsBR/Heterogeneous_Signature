N = 20000

#####   Chain Network with m1 = m2 = 8  ##############################

exact = "Exact_Signature_Chain_m1_8.txt"
exact_data <- read.delim(exact, sep = " ", header = FALSE)
exact_data <- exact_data[, -ncol(exact_data)]

estimate = "Chain_BO_M20000_m1_8_signature.txt"
estimate_data <- read.delim(estimate, sep = " ", header = FALSE)
estimate_data <- estimate_data[, -ncol(estimate_data)]

fileName <- "P_values_m1_8.txt"

######################################################################


#####     Chain Network with m1 = m2 = 20  ###########################

# exact = "Exact_Signature_Chain_m1_20.txt"
# exact_data <- read.delim(exact, sep = " ", header = FALSE)
# exact_data <- exact_data[, -ncol(exact_data)]
# 
# estimate = "Chain_BO_M75000_m1_20_signature.txt"
# estimate_data <- read.delim(estimate, sep = " ", header = FALSE)
# estimate_data <- estimate_data[, -ncol(estimate_data)]
# 
# fileName <- "P_values_m1_20.txt"


#####################################################################

p_values <-matrix(0, nrow(exact_data), nrow(exact_data))

num_rejects = 0

num_entries = 0;

for(i in 1:nrow(exact_data)){
  for(j in 1:ncol(exact_data)){
    p<-estimate_data[i,j]
    p0<-exact_data[i,j]
    
    if (p == p0){
      p_values[i,j] <- 1
      if (p0 > 0 & p0 < 1){num_entries = num_entries + 1}
    } else if (p0 > 0 & p0 < 1){
      num_entries = num_entries + 1
      res <- prop.test(p*N, N, p0, alternative = "two.sided", correct = TRUE)
      p_values[i,j] <- res$p.value
      if (p_values[i,j] < 0.05){
        print("reject H0")
        num_rejects = num_rejects+1}
    }
  }
}

print(min(p_values))

write.table(p_values, fileName, append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
print(num_rejects)
print(num_entries)
print(num_rejects/num_entries)
