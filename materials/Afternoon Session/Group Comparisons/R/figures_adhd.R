library(bgms)
library(easybgm)
library(qgraph)
library(svglite)
library(ggplot2)


group_indicator = ADHD[, 1]
data = ADHD[, -1]

fit_diag = bgm(
  x = data[group_indicator == 1, ],
  iter = 2000, # Number of MCMC iterations
  edge_selection = FALSE, #No edge selection
  seed = 1234)

fit_no_diag = bgm(
  x = data[group_indicator == 0, ],
  iter = 2000, # Number of MCMC iterations
  edge_selection = FALSE, #No edge selection
  seed = 1234)

layout = qgraph::qgraph(
  input = fit_diag$posterior_mean_pairwise,
  DoNotPlot = TRUE,
  layout = "spring"
)$layout

global_max <- max(
  (fit_diag$posterior_mean_pairwise),
  (fit_nodiag$posterior_mean_pairwise)
)

dev.off()
svglite::svglite(
  file = "qgraph_adhd_diag.svg",
  width = 10,
  height = 8
)

qgraph::qgraph(
  fit_diag$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(ADHD[,-1]),
  theme = "TeamFortress",
  vsize = 7,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)
dev.off()



svglite::svglite(
  file = "qgraph_adhd_nodiag.svg",
  width = 10,
  height = 8
)

qgraph::qgraph(
  fit_nodiag$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(ADHD[,-1]),
  theme = "TeamFortress",
  vsize = 7,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)
dev.off()


###### 

samples_diag = do.call(rbind, fit_diag$raw_samples$pairwise)
sample_diag = samples_diag[,116]
samples_nodiag = do.call(rbind, fit_nodiag$raw_samples$pairwise)
sample_nodiag = samples_nodiag[,116]


######

dev.off()

svglite::svglite(
  file = "ggplot_dens_susatt_talks.svg",
  width = 10,
  height = 8
)

# 1. Explicit trimming (illustrative range)
trim_range <- c(-10, 3)

sample_diag_trim <- sample_diag[
  sample_diag > trim_range[1] & sample_diag < trim_range[2]
]

sample_nodiag_trim <- sample_nodiag[
  sample_nodiag > trim_range[1] & sample_nodiag < trim_range[2]
]

# 2. Build data frame
df_left <- rbind(
  data.frame(value = sample_diag_trim, group = "Diagnosis"),
  data.frame(value = sample_nodiag_trim, group = "No diagnosis")
)

# 3. Means (from full samples, not trimmed)
mu_diag   <- mean(sample_diag_trim)
mu_nodiag <- mean(sample_nodiag_trim)

# 4. Plot
ggplot(df_left, aes(x = value, color = group, fill = group)) +
  geom_density(
    #aes(y = after_stat(scaled)),
    adjust = 3,
    alpha = 0.25,
    linewidth = 0.8
  ) +
  geom_vline(xintercept = mu_diag, color = "#F8766D", linewidth = 1) +
  geom_vline(xintercept = mu_nodiag, color = "#00BFC4", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = trim_range) +
  labs(
    title = "Posterior density for susatt-talks interaction",
    subtitle = "Independent analyses",
    x = "Edge weight difference",
    y = "Posterior density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12)
  )

dev.off()


######

easy_fit = easybgm_compare(
  data = data,
  group_indicator = group_indicator,
  iter = 2000,
  package = "bgms",
  type = "ordinal",
  seed = 1234)

dev.off()

svglite::svglite(
  file = "easybgm_adhd_edgeevidence_3.svg",
  width = 10,
  height = 8
)

plot_edgeevidence(
  easy_fit, 
  layout = layout,
  legend = FALSE,
  vsize = 7,
  labels = colnames(data),
  evidence_thresh = 3)

dev.off()


svglite::svglite(
  file = "easybgm_adhd_edgeevidence_10.svg",
  width = 10,
  height = 8
)

plot_edgeevidence(
  easy_fit, 
  layout = layout,
  legend = FALSE,
  vsize = 7,
  labels = colnames(data),
  evidence_thresh = 10)

dev.off()


######

easy_fit_beta_bern = easybgm_compare(
  data = data,
  group_indicator = group_indicator,
  iter = 2000,
  package = "bgms",
  type = "ordinal",
  seed = 1234,
  difference_prior = "Beta-Bernoulli",
  beta_bernoulli_alpha = 1,
  beta_bernoulli_beta = 1
)

svglite::svglite(
  file = "easybgm_adhd_edgeevidence_10_beta_bern.svg",
  width = 10,
  height = 8
)

plot_edgeevidence(
  easy_fit_beta_bern, 
  #layout = layout,
  legend = FALSE,
  vsize = 7,
  labels = colnames(data),
  evidence_thresh = 10)

dev.off()