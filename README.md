# Estimation of flow trajectories in a multi-lines transportation network

### Guillaume Guex, Romain Loup, François Bavaud

This repository contains the data and code in order to produce the results in the (upcoming) article *Estimation of flow trajectories in a multi-lines
transportation network*, with applications to the public transportation network in the city of Lausanne.

## Abstract 

Characterizing a public transportation network, such as an urban network
with multiple lines, requires the origin-destination trip counts during a given
period. Yet, if automatic counting makes the embarkment (boarding) and
disembarkment (alighting) counts in vehicles known, it often happens that
pedestrian transfers between lines are harder to track, and require costly and
invasive devices (e.g., facial recognition system) to be estimated. In this
contribution, we propose a method, based on maximum entropy and
involving an iterative fitting procedure, which estimates the passenger flow
between origins and destinations solely based on embarkment and
disembarkment counts. Moreover, this method is flexible enough to provide
an adaptable framework in case additional data is known, such as attraction
poles between certain nodes in the network, or percentages of transferring
passengers between some lines. This method is tested on toy examples, as
well as with the data of the public transportation network of the city of
Lausanne provided by its Transportation Agency (tl), and gives arguably
convincing estimations of the transportation flow.

## Keywords 

multiline bus network; origin-destination flows; boarding and alighting counts; maximum entropy estimation; iterative
proportional fitting

## Some Results

#### Iteration example on a toy network. The network edges are represented by black edges, transfer flow is represented by green edges
<figure>
    <img src="utilities/readme_img/iterations.png" alt="tutorial example" style="width:50%">
</figure>

#### Mean transportation error (MTE) on different toy examples size (p) vs the minimum embarkement/disembarkment rate $\theta$

<figure>
    <img src="utilities/readme_img/MTE_tours.png" alt="tutorial example" style="width:50%">
</figure>

#### Profiles of stops in the city of Lausanne (first CA dimension)

<figure>
    <img src="utilities/readme_img/afc_in_dim1.png" alt="tutorial example" style="width:50%">
</figure>

#### Largest estimated public transportation hubs in the city of Lausanne

<figure>
    <img src="utilities/readme_img/transfers.png" alt="tutorial example" style="width:50%">
</figure>

## Tutorial 

To present how the algorithm work, we present a small tutorial with a very simple network (the code for this tutorial can be found in `0.0_tutorial.R`). This network have two lines, with 3 stops each, and transfer edges connecting their middle stops.

<figure>
  <img src="utilities/readme_img/tuto_network.png" alt="tutorial example" style="width:20%">
</figure>

All the functions to run the algorithm are located in `local_functions.R`, we first need to load them

```R
source("local_functions.R")
```

We will build the network structure by hand 

```R
# The line memberships of nodes
line_mbrshps = c(1, 1, 1, 2, 2, 2)

# The adjacency matrix within lines
adj_w = matrix(c(0, 1, 0, 0, 0, 0,
                 0, 0, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 1,
                 0, 0, 0, 0, 0, 0), 6, 6, byrow=T)

# The adjacency matrix between lines
adj_b = matrix(c(0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 0, 0,
                 0, 1, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0), 6, 6, byrow=T)
```

The (permitted) shortest-paths in the network are needed to run the algorithm. Information about them can be constructed with `build_sp_data()` function. This function need memberships of each line, memberships of each tour (back and forth trips), the adjacency matrix within lines (`adj_w`) and between lines `adj_b`)

```R
# Prepare the shortest-paths data
sp_data = build_sp_data(line_mbrshps, 
                        tour_mbrshps=line_mbrshps, 
                        adj_w, 
                        adj_b)
```

This function return 3 elements : `sp_data$edge_ref` is a table describing oriented edges, with 3 columns: the starting node, the ending node and a boolean indicator equal to 1 if the edge is a transfer edge. `sp_data$sp_ref` is a table referencing perimitted shortest-paths among the network, with 2 columns: the origin node and the destination node. Finally, `sp_data$sp_edge_link` is the shortest-paths/edges incidence matrix, with order corresponding to the one given in reference matrices. This matrix might be very large, and is given in a sparse format (`"ngCMatrix"`).

Now that the permitted shortest-paths data is constructed, we will draw a random number of passengers on each possible trips. First let us construct the permitted path in a matrix format 

```R
permitted_paths = as.matrix(sparseMatrix(sp_data$sp_ref[, 1], 
                                         sp_data$sp_ref[, 2], 
                                         dims=c(6, 6)))
```

And let us draw the passengers

```R
# A random vector with the size of permitted paths
random_vec = round(runif(sum(permitted_paths), 1, 1000))

# Fill permitted paths
n_real = permitted_paths
n_real[n_real] = random_vec
n_real
````

The matrix `n_real` contains the drawn flow, and the algorithm will estimate it according the embarkment and disembarkment counts. To compute the latters, we will need the flow on edges, which can be obtained with the `compute_x_from_n` function (for convenience, this function give 3 outputs, $x_mat, $x_wit and $x_btw, respectively all flows, flows within lines and flows between lines)

```R
# Compute the flow on transfer edges
x_btw = compute_x_from_n(n_real, 
                         sp_data$edge_ref, 
                         sp_data$sp_ref, 
                         sp_data$sp_edge_link)$x_btw
```

We can now compute the embarkment and disembarkment counts on every nodes 

```R
# Compute the embarkment counts
flow_l_in = rowSums(n_real) + colSums(x_btw)
# Compute the disembarkment counts
flow_l_out = colSums(n_real) + rowSums(x_btw)
```

Finally, we can use the function `compute_origin_destination()` in order to get an estimation of the flow based on the embarkment and disembarkment counts

```R
n_algo = compute_origin_destination(flow_l_in,  # embarkment counts
                                    flow_l_out, # disembarkment counts
                                    sp_data$edge_ref,   # edges reference
                                    sp_data$sp_ref,     # shortest-paths reference
                                    sp_data$sp_edge_link,   # sp-edges incidence matrix
                                    min_p_ntwk=0.001,   # minimum embarkment/disembarkement proportion hyperparameter
                                    display_it=F)   # do not display information along iterations
```
We can now compute the MME and MTE errors 

```R
# Compute the MTE
mean_transport_error = sum(abs(n_alg - n_real)) / sum(n_real)
# Compute the MME
x_btw_alg = compute_x_from_n(n_alg, 
                             sp_data$edge_ref, 
                             sp_data$sp_ref, 
                             sp_data$sp_edge_link)$x_btw
mean_margin_error = sum(abs(flow_l_in - rowSums(n_alg) - colSums(x_btw_alg))) + 
  sum(abs(flow_l_out - colSums(n_alg) - rowSums(x_btw_alg))) / 
  (2 * sum(flow_l_in))
```

## Organization of the repository 

All useful functions in order to run the algorithm are contained in the `local_functions.R` script. Most of them are documented. The rest of the scripts permit to create the results found in the article:

* `0.0_tutorial.R`: contains the tutorial, as seen in the previous section.
* `1.0_tl_build_data.R`: permits to build shortest-paths data and balance the lines counts for the "transport lausannois" (tl) data.
* `2.0_tl_compute_OD.R`: permits to compute the estimated transportation flow on tl data.
* `3.x` scripts: allow to map results from the estimated transportation flow.
* `4.x` scripts: create the results for toy examples.

The folders contain the following:

* `archives`: old scripts 
* `latex_files`: various notes and presentations about the method
* `multilines_data`: the tl data
* `results`: various results produced by the method
* `utilities`: utility scripts.
