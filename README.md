
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Phoradentron interactions

<!-- badges: start -->
<!-- badges: end -->

Instructions to run the code used to generate results in the manuscript
“Interaction network of *Phoradendron* and its hosts and the influence
of phylogenetic, geographic, and environmental factors on the
probability of interaction”

All analyses were run in a computer with the following specifications:

CPU: Intel(R) Core(TM) i7-6700 CPU @ 3.40GH with 64GB RAM You’ll still
need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

# File description and steps:

Download repository from GitHub at
<https://anonymous.4open.science/r/phoradendron_interactions/> . The
script directory contains src scripts (with helper functions) and
numerated scripts. All the outputs of the scripts will appear in the
*data* directory. In order to have replicability you need to run scripts
sequentially and store the results in *data* directory

- Script 0 download the input data from a cloud server. If the server is
  down please contact us.
- Script 1 and 2 are the taxonomic harmonization
- Script 3 create the host parasite host Genus data frame
- Script 4 Take a sample of a species for each host genus in order to
  have a species to prune the phylogenetic tree
- Script 5 and 6 prune the phylogenetic tree of host and *Phoradendron*
- Script 7 harmonize phylogenetic tree taxa with host parasite data
  frame
- Script 9 Create separate files for nodes and edges and the interaction
  biadjacency matrix. This is a binary matrix where host are in rows and
  parasites are in columns. If the interaction is present scores 1 if
  not scores 0.
- Script 11 calculates the nestedness value
- Script 12 calculates the modularity value
- Scripts 13 and 14 creates the presence absence matrix (PAM) for
  geographical and environmental data of host
- Script 15 calculates the jaccard index for both PAM.
- Script 16 calculate the linear models. These are Logistic regressions
  between shared *Phoradendron* and phylogenetric, geographic and
  environmental distance between host.
- Script 17 generate area host-range relationship
- Script 18 shows the connectivity $mu$ relationship
- Script 19 - 22 Create plots
- Script 23 infer and plot the connectivity as a trait in the
  *Phoradendron* phylogenetic tree
