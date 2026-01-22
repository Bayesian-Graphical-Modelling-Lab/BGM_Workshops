# =========================
#  0. SETUP
# =========================

# For questions, please contact
# n.sekulovski@uva.nl

# Set working directory if necessary

# Install the package
# install.packages("easybgm")  # CRAN version
# remotes::install_github("KarolineHuth/easybgm", ref = "developer")

# load the package 
library(easybgm)


# Load data
data <- read.csv("exampledata.csv")[, -1]
dim(data) # 13 variables, 2000 observations

# -----------------------------------------------------------------------------------------------------------------

# =========================
# 1. Analysis
# =========================

# 1. Fit the model
?easybgm

fit <- easybgm(data = data,         #(M) n*p data frame or matrix
               type = "continuous", #(M) type of data (mandatory)
               package = "BDgraph", #(O) package to use (optional)
               iter = 1e4,          #(O) no. iterations sampler 1e4
               save = TRUE,         #(O) Should samples be stored
               centrality = TRUE,   #(O) Should centrality be computed
               progress = TRUE)     #(O) Should the progress bar be plotted


# summary of the fit
summary(fit)

# -----------------------------------------------------------------------------------------------------------------

# =========================
# 2. Visualization
# =========================

# a. Plot evidence plot (Testing - Is there an edge?)
plot_edgeevidence(fit, evidence_thresh = 10)

# To help with the interpretation of networks, we'll split it in two
par(mfrow = c(1, 2))
plot_edgeevidence(fit, edge.width = 3,
                  split = T, legend = T,
                  evidence_thresh = 10)

# -----------------------------------------------------------------------------------------------------------------
# b. Plot network model (Estimation: What is the strength and direction of the edge?)

par(mfrow = c(1, 1))
plot_network(fit) # exc_prob is by default set to 0.5

# customizing the plot
Names_data <- colnames(data)
groups_data <- c(rep("DASS", 3), rep("Personality", 10))
plot_network(fit, exc_prob = 0.5, layout = "spring",
             nodeNames = Names_data, groups = groups_data,
             color= c("#fbb20a","#E59866"),
             theme = "Fried", dashed = T)


# c.  plot HDI intervals of the estimated parameters
plot_parameterHDI(fit)

# -----------------------------------------------------------------------------------------------------------------
# d. Plot strength centrality i.e., degree to which each node is connected to other nodes in the estimated network.
plot_centrality(fit)

# -----------------------------------------------------------------------------------------------------------------
# e. Structure plots
plot_complexity_probabilities(fit)
plot_structure(fit)

# =========================
# 3. Priors
# =========================
# install.packages("bgms")
library(bgms) # for the dataset
fit <- easybgm(data = Wenchuan[, 1:5],#(M) n*p data frame or matrix 
               type = "ordinal",      #(M) type of data
               package = "bgms",      #(O) type of sampling algorithm
               iter = 3e3,            #(O) no. iterations (for illustrative purposes)
               save = FALSE,          #(O) Should samples be stored
               centrality = TRUE,     #(O) Should centrality be computed
               progress = TRUE,       #(O) Should the progress bar be plotted
               inclusion_probability = 0.7)

par(mfrow = c(1, 2))
plot_edgeevidence(fit, edge.width = 3, split = F,
                  legend = F, evidence_thfith = 10)
summary(fit)

# -----------------------------------------------------------------------------------------------------------------

# ================================================
# Fit a mixed model (Copula model)
# ===============================================

not_cont <- c(rep(0, 3), rep(1, 10))
fit <- easybgm(data = data,          #(M) n*p matrix of responses (mandatory)
               type = "mixed",       #(M) type of data (mandatory)
               package = "BDgraph",  #(O) package to use (optional)
               not_cont = not_cont,  #(O) vector of 0s and 1s indicating
               iter = 1e4,           #(O) no. iterations sampler 1e4
               save = FALSE,         #(O) Should samples be stored
               centrality = FALSE,   #(O) Should centrality be computed
               progress = TRUE)      #(O) Should the progress bar be plotted

# -----------------------------------------------------------------------------------------------------------------

# ==========================================
# Optional: Differences between two groups
# =========================================

# Load the data

clinical <- read_csv("data_clinical.csv")
population <- read_csv("data_population.csv")


# estimate the model
?bgmCompare
fit_compare <- bgmCompare(x = clinical,
                          y = population,
                          iter = 1e2)

# Extract the posterior incusion probabilities of the differences
pip <- fit_compare$indicator[
  lower.tri(fit_compare$indicator)
  ]
pip

# calculate the inclusion BF for the differences
bf <- pip/(1-pip)
bf

# extract the estimated (posterior) difference parameters 

diff_parameters <- fit_compare$pairwise_difference[
  lower.tri(fit_compare$pairwise_difference)
]
diff_parameters

# -----------------------------------------------------------------------------------------------------------------
# ==========================================
# Optional: Clustering Analysis
# =========================================

# For a accessible tutorial on how to perform a clustering analysis see: 
# https://www.nikolasekulovski.com/blog/post2/
