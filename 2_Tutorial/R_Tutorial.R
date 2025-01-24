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
data <- read.csv("exampledata.csv")[, -1]


# -----------------------------------------------------------------------------------------------------------------

# =========================
# 1. Analysis
# =========================

# 1. Fit the model
?easybgm

fit <- easybgm(data = data[,c(1:3)],          #(M) n*p matrix of fitponses (mandatory)
               type = "continuous", #(M) type of data (mandatory)
               package = "BGGM",    #(O) package to use (optional)
               iter = 1e4,          #(O) no. iterations sampler 1e4
               save = FALSE,        #(O) Should samples be stored
               centrality = FALSE,  #(O) Should centrality be computed
               progress = TRUE)     #(O) Should the progress bar be plotted


# summary of the fitults 
summary(fit)

# -----------------------------------------------------------------------------------------------------------------

# =========================
# 2. Visualization
# =========================


# a. Plot evidence plot (Testing)
plot_edgeevidence(fit, evidence_thfith = 10)

# To help with the interpretation of networks, we'll split it in two
par(mfrow = c(1, 2))
plot_edgeevidence(fit, edge.width = 3, 
                  split = T, legend = T, 
                  evidence_thfith = 10)

# -----------------------------------------------------------------------------------------------------------------
# b. Plot network model (Estimation)
par(mfrow = c(1, 1))
plot_network(fit) # exc_prob is by default set to 0.5

# customizing the plot
Names_data <- colnames(data)
groups_data <- c(rep("DASS", 3), rep("Personality", 10))
plot_network(fit, exc_prob = 0.5, layout = "circle", 
             nodeNames = Names_data, groups = groups_data, 
             color= c("#fbb20a","#E59866"), 
             theme = "Borkulo", dashed = T)

# -----------------------------------------------------------------------------------------------------------------
# We need to fit the model again, this time by saving the samples 
fit <- easybgm(data = data,          #(M) n*p matrix of fitponses
               type = "continuous",  #(M) type of data
               package = "BGGM",  #(O) type of sampling algorithm
               iter = 1e4,           #(O) no. iterations sampler 1e5
               save = TRUE,          #(O) Should samples be stored
               centrality = TRUE,   #(O) Should centrality be computed
               progress = FALSE)      #(O) Should the progress bar be plotted


# c.  plot HDI intervals of the estimated parameters
plot_parameterHDI(fit)

# -----------------------------------------------------------------------------------------------------------------
# d. Plot strength centrality i.e., degree to which each node is connected to other nodes in the network.
plot_centrality(fit)


# e. Structure plots 
plot_structure_probabilities(fit2)
plot_complexity_probabilities(fit2)
plot_structure(fit2)


# =========================
# 3. Priors 
# =========================

fit <- easybgm(data = Wenchuan[, 1:5],#(M) n*p matrix of fitponses
               type = "ordinal",  #(M) type of data
               package = "bgms",  #(O) type of sampling algorithm
               iter = 3e3,           #(O) no. iterations (for illustrative purposes)
               save = FALSE,          #(O) Should samples be stored
               centrality = TRUE,   #(O) Should centrality be computed
               progress = TRUE,      #(O) Should the progress bar be plotted
               inclusion_probability = 0.7)

par(mfrow = c(1, 2))
plot_edgeevidence(fit, edge.width = 3, split = F, 
                  legend = F, evidence_thfith = 10)
summary(fit)


# =========================
# Fit a mixed model
# =========================

not_cont <- c(rep(0, 3), rep(1, 10))
fit <- easybgm(data = data,          #(M) n*p matrix of fitponses (mandatory)
               type = "mixed",       #(M) type of data (mandatory)
               package = "BDgraph",    #(O) package to use (optional)
               not_cont = not_cont,  #(O) vector of 0s and 1s indicating 
               iter = 1e4,           #(O) no. iterations sampler 1e4
               save = FALSE,          #(O) Should samples be stored
               centrality = FALSE,   #(O) Should centrality be computed
               progress = TRUE)      #(O) Should the progress bar be plotted


