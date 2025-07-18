# =========================
#  0. SETUP
# =========================

# For questions, please contact
# n.sekulovski@uva.nl

# Set working directory if necessary 

# Load background functions
# install.packages("easybgm")  # CRAN version 
# remotes::install_github("KarolineHuth/easybgm", ref = "developer")

library(easybgm)

# Load data
data <- read.csv("2_Tutorial/exampledata.csv")[, -1]


# -----------------------------------------------------------------------------------------------------------------

# =========================
# 1. Analysis
# =========================

# 1. Fit the model
?easybgm
res <- easybgm(data = data,          #(M) n*p matrix of responses (mandatory)
               type = "continuous",  #(M) type of data (mandatory)
               package = "BGGM",     #(O) package to use (optional)
               iter = 1e4,           #(O) no. iterations sampler 1e4
               save = FALSE,          #(O) Should samples be stored
               centrality = FALSE,   #(O) Should centrality be computed
               progress = TRUE)      #(O) Should the progress bar be plotted


# summary of the results 
summary(res, evidence_thresh = 10)

# -----------------------------------------------------------------------------------------------------------------

# =========================
# 2. Visualization
# =========================


# a. Plot evidence plot (Testing)
plot_edgeevidence(res, evidence_thresh = 10)

# To help with the interpretation of networks, we'll split it in two
par(mfrow = c(1, 2))
plot_edgeevidence(res, edge.width = 3, 
                  split = T, legend = T, 
                  evidence_thresh = 10)

# -----------------------------------------------------------------------------------------------------------------
# b. Plot network model (Estimation)
par(mfrow = c(1, 1))
plot_network(res, exc_prob = 0.91) # exc_prob is by default set to 0.5

# customizing the plot
Names_data <- colnames(data)
groups_data <- c(rep("DASS", 3), rep("Personality", 10))
plot_network(res, exc_prob = 0.5, layout = "spring", 
             nodeNames = Names_data, groups = groups_data, 
             color= c("#fbb20a","#E59866"), 
             theme = "Borkulo", dashed = T)

# -----------------------------------------------------------------------------------------------------------------


res <- easybgm(data = data,          #(M) n*p matrix of responses
               type = "continuous",  #(M) type of data
               package = "BGGM",  #(O) type of sampling algorithm
               iter = 1e4,           #(O) no. iterations sampler 1e5
               save = TRUE,          #(O) Should samples be stored
               centrality = TRUE,   #(O) Should centrality be computed
               progress = FALSE)      #(O) Should the progress bar be plotted


# c. Plot forest plot
plot_parameterHDI(res)

# -----------------------------------------------------------------------------------------------------------------
# d. Plot strength centrality

plot_centrality(res)

# =========================
# 3. Priors 
# =========================
# Note that the help file says that bgms has two options for the 
# interaction prior; this is no longer the case, only the Cauchy prior 
# (with an adjustable scale parameter) is available. Additionally,
# the stochastic block as a prior on the network structure
# (i.e., edge prior) is available for use however, a paper
# explaining the method is still in preparation.

# If you use the Beta-Bernoulli prior, please note that
# there might be an issue when calculating the inclusion Bayes factor
# due to an issue in the calculation of the prior odds
# in this case, simply use the posterior inclusion probability
# as a measure of evidence for the inclusion of an edge. Or
# BF = posterior inclusion probability/(1-posterior inclusion probability)
# only in case you use beta_bernoulli_alpha = 1 and beta_bernoulli_beta = 1

res <- easybgm(data = Wenchuan[, 1:5],#(M) n*p matrix of responses
               type = "ordinal",  #(M) type of data
               package = "bgms",  #(O) type of sampling algorithm
               iter = 3e3,           #(O) no. iterations (for illustrative purposes)
               save = FALSE,          #(O) Should samples be stored
               centrality = TRUE,   #(O) Should centrality be computed
               progress = TRUE,      #(O) Should the progress bar be plotted
               inclusion_probability = 0.7)

par(mfrow = c(1, 2))
plot_edgeevidence(res, edge.width = 3, split = F, 
                  legend = F, evidence_thresh = 10)
summary(res)


