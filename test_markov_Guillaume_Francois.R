# Petit script de calcul des trajets via une chaîne de Markov (+/-)
# Orginal: Guillaume (+ qqes notations de François)


# génération de montées x et descentes y d'une ligne de longueur (= nombre arrêts) l


#  Exemple 1 : l=8 
x = c(30, 15, 50, 15, 0, 20, 5, 0) # Nombre de gens qui montent
y = c(0, 10, 30, 40, 25, 0, 10, 20) # Nombre de gens qui descendent

#  Exemple 2 : l=10 
#   x = c(30, 15, 50, 15, 0, 20, 5, 30,20,0) # Nombre de gens qui montent
#   y = c(0, 10, 30, 40, 25, 0, 10, 50,0,20) # Nombre de gens qui descendent
# ici, le bus est vide entre les arrêts 8 et 9 


l = length(x) # Longueur du trajet

X=cumsum(x) # Nombre cumulé de gens qui montent
Y=cumsum(y) # Nombre cumulé de gens qui descendent. Il faut que $X_j\ge Y_j$ pour tout $j$, et que $X_l=Y_l$

transit=X-Y  #  ici un vecteur de longueur l, équivalent à transit_ij qui était de longueur l-1
t=c(1,transit)[-(l+1)] # "transit d'avant". La valeur t[1]=1 permet de définir p_out[1]=0/1=0

p_in = x / sum(x) # Probabilité de montée 
p_out = y / t # Probabilité de descente
c = 1 - p_out # Probabilité de continuer


Ptr = matrix(0, l, l)
for(i in 1:(l-1)){
  for(j in (i+1):l){
  	c_trunc_right=c[-(j:l)]
  	c_trunc=c(1,c_trunc_right)[-(1:(i+1))]
  	Ptr[i, j]=p_in[i]*prod(c_trunc)*p_out[j]   
  }
}

# Note: le code marche parce que, pour j=i+1, on a prod(numeric(0))=1, ce qui est assez chanceux... 


# Affichage en probs 
Ptr
# Affichage en effectifs (les marges donnent x et y)
N=Ptr*sum(x); N 

# les marges donnent x et y:
#  rowSums(N)-x == 0
#   colSums(N)-y == 0

###### iterative fitting ######


Nloop=500

beta=c(0,rep(1/(l-1),l-1))
alpha_old=c(rep(1/(l-1),l-1),0)
beta_old=c(0,rep(1/(l-1),l-1))
eps_alpha=c()
eps_beta=c()

for (r in 1:Nloop){
alpha=c((1/(sum(beta*y)-cumsum((beta*y))))[-l],0)  # formule (9) du document MaxEnt_une_ligne_v2.pdf
beta=c(0,(1/cumsum(c(0,alpha*x))[-l])[-1]) # idem
eps_alpha[r]=sum((alpha-alpha_old)^2)  # donne une idée de la convergence
eps_beta[r]=sum((beta-beta_old)^2) # donne une idée de la convergence
alpha_old=alpha
beta_old=beta
}

plot(eps_alpha[-1])
plot(eps_beta[-1])


I_j_greater_i=matrix(0,l,l)
for (i in 1:l){
	for (j in (i+1):l){
		I_j_greater_i[i,j]=1
	}
}
I_j_greater_i # matrice $I(j>i)$

a=alpha*x
b=beta*y
N_approx=diag(a)%*%I_j_greater_i%*%diag(b); N_approx

max(abs(N-N_approx)) # 8.348877e-14
