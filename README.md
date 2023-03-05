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
  <figcaption>Tutorial network</figcaption>
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



