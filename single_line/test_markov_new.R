############# Petit script de calcul des trajets via une chaîne de Markov

n_montees = c(10, 20, 50, 20, 30, 20, 10, 0) # Nombre de gens qui montent
n_descentes = c(0, 5, 10, 25, 50, 20, 20, 30) # Nombre de gens qui descendent
l = length(n_montees) # Longueur du trajet1

# Calcul du transit (doit correspondre dans les données) 
transit_ij = n_montees[1] 
for (i in 2:(l-1)){
  transit_ij = c(transit_ij, transit_ij[i-1] + n_montees[i] - n_descentes[i])
}

p_in = n_montees / sum(n_montees) # Probabilité de montées 
p_out = n_descentes / c(1, transit_ij) # Probabilité de descente
p_ij = 1 - p_out # Probabilité de continuer

########## Calcul avec chaîne de Markov 

# On construit la matrice W
W_tr = rbind(cbind(rep(0,l-1), diag(p_ij[-l])), rep(0, l))
W = rbind(cbind(rep(0, l), rbind(p_in, cbind(W_tr[-1,-1], p_out[-1]))), rep(0, l+1))

# On trouve la matrice fondamentale
Z = solve(diag(dim(W)[1]) - W)

# On construit la matrice de prob
Ptr = outer(p_in[-l], rep(1,l-1)) * Z[-c(1, l+1), -c(1, l+1)] * outer(rep(1,l-1), p_out[-1])
Ptr = rbind(cbind(rep(0, l-1), Ptr), rep(0, l))

A = matrix(1, dim(Ptr)[1], dim(Ptr)[1])
for(i in 1:dim(A)[1]){
  for(j in 1:dim(A)[2]){
    if(j <= i) A[i, j] = 0
  }
}

sigma_in = n_montees / sum(n_montees)
sigma_out = n_descentes / sum(n_descentes)
converge_if = F
results_mat = A
epsilon = 1e-30
conv_thres_if = 1e-10
while(!converge_if){
  # Saving old results
  results_mat_old = results_mat 
  # Normalizing by row
  results_mat = results_mat * sigma_in / rowSums(results_mat + epsilon)
  # Normalizing by columns 
  results_mat = t(t(results_mat) * sigma_out / colSums(results_mat + epsilon))
  # Checking for convergence
  if(sum(abs(results_mat_old - results_mat)) < conv_thres_if){
    converge_if = T
  }
}
