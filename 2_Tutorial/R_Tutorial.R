# =========================
#  0. SETUP
# =========================

# For questions, contact
# n.sekulovski@uva.nl

# Set working directory

# Load background functions
install.packages("easybgm")
# remotes::install_github("KarolineHuth/easybgm", ref = "developer")

library(easybgm)

# Load data
data <- read.csv("exampledata.csv")[, -1]


# -----------------------------------------------------------------------------------------------------------------

# =========================
# 1. Analysis
# =========================

# 1. Fit the model
res <- easybgm(data = data,          #(M) n*p matrix of responses
               type = "continuous",  #(M) type of data
               package = "BGGM",     #(O) package to use
               iter = 1e4,           #(O) no. iterations sampler 1e5
               save = FALSE,          #(O) Should samples be stored
               centrality = FALSE,   #(O) Should centrality be computed
               progress = TRUE)      #(O) Should the progress bar be plotted

summary(res, evidence_thresh = 3)

# -----------------------------------------------------------------------------------------------------------------

# =========================
# 2. Visualization
# =========================


# a. Plot evidence plot (Testing)
plot_edgeevidence(res, evidence_thresh = 10)

# To help with the interpretation of networks, we'll split it in two
par(mfrow = c(1, 2))
plot_edgeevidence(res, edge.width = 3, split = T, legend = F, evidence_thresh = 10)

# -----------------------------------------------------------------------------------------------------------------
# b. Plot network model (Estimation)
par(mfrow = c(1, 1))
plot_network(res, exc_prob = 0.5, dashed = T)

# customizing the plot
Names_data <- colnames(data)
groups_data <- c(rep("DASS", 3), rep("Personality", 10))
plot_network(res, exc_prob = .999, layout = "spring", nodeNames = Names_data, groups = groups_data, 
             color= c("#fbb20a","#E59866"), theme = "Borkulo", dashed = T
             )

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
# d. Plot strength centrality (will not discussed)

#plot_centrality(res)

# =========================
# 3. Priors 
# =========================

?bgm
res <- easybgm(data = as.matrix(data[1:300, 5:10]),          #(M) n*p matrix of responses
               type = "ordinal",  #(M) type of data
               package = "bgms",  #(O) type of sampling algorithm
               iter = 3e3,           #(O) no. iterations sampler 1e5
               save = TRUE,          #(O) Should samples be stored
               centrality = TRUE,   #(O) Should centrality be computed
               progress = TRUE,      #(O) Should the progress bar be plotted
               inclusion_probability = 0.7)

