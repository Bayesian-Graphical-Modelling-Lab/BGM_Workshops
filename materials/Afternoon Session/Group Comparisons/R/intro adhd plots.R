library(easybgm)

library(bgms)
?ADHD

# Network estimation
fit = bgm(x = ADHD[, -1], # Remove group ID column
          iter = 1000, # Number of MCMC iterations
          seed = 1234)

fit_easy = easybgm(data = ADHD[, -1], # Remove group ID column
                   iter = 1000, # Number of MCMC iterations
                   package = "bgms",
                   type = "ordinal",
                   seed = 1234)

# Group analyses
diagnosed_group = ADHD[ADHD$group == 1, -1]
undiagnosed_group = ADHD[ADHD$group != 1, -1]

fit_diag = bgm(x = diagnosed_group,
               iter = 1000, # Number of MCMC iterations
               edge_selection = FALSE, #No edge selection
               seed = 1234)

fit_nodiag = bgm(x = undiagnosed_group,
               iter = 1000, # Number of MCMC iterations
               edge_selection = FALSE, #No edge selection
               seed = 1234)



layout = qgraph::qgraph(
  input = fit$posterior_mean_pairwise,
  DoNotPlot = TRUE,
  layout = "spring"
)$layout

global_max <- max(
  (fit_diag$posterior_mean_pairwise),
  (fit_nodiag$posterior_mean_pairwise)
)

dev.off()
pdf(file = "easybgm_adhd.pdf", width = 10, height = 10, useDingbats = FALSE)
plot_network(fit_easy, legend = FALSE, layout = layout)

dev.off()
pdf(file = "qgraph_adhd.pdf", width = 10, height = 10, useDingbats = FALSE)
qgraph::qgraph(
  fit$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(ADHD[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)

dev.off()
#pdf(file = "qgraph_adhd_diag.pdf", width = 10, height = 10, useDingbats = FALSE)
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
#pdf(file = "qgraph_adhd_nodiag.pdf", width = 10, height = 10, useDingbats = FALSE)
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


#fit_diag$raw_samples$parameter_names$pairwise[[116]]
samples_diag = do.call(rbind, fit_diag$raw_samples$pairwise)
sample_diag = samples_diag[,116]
samples_nodiag = do.call(rbind, fit_nodiag$raw_samples$pairwise)
sample_nodiag = samples_nodiag[,116]

# network comparison

fit_compare = bgmCompare(x = ADHD[, -1], 
                 group_indicator = ADHD[, 1],
                 difference_selection = FALSE,
                 iter = 2e3,
                 seed = 1234,
                 difference_scale = 2.5)

d = fit_compare$posterior_summary_pairwise_differences[, 2]
diff = matrix(0, nrow = 18, ncol = 18)
diff[upper.tri(diff)] = d
diff = diff + t(diff)

dev.off()
pdf(file = "qgraph_diff.pdf", width = 10, height = 10, useDingbats = FALSE)
qgraph::qgraph(
  diff,
  layout = layout,
  labels = colnames(ADHD[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE
)




# Sanity checks
plot(coef(fit_compare)$pairwise_effects_groups[,2], coef(fit_diag)$pairwise[lower.tri(coef(fit_diag)$pairwise)])
abline(0,1)
plot(coef(fit_compare)$pairwise_effects_groups[,1], coef(fit_nodiag)$pairwise[lower.tri(coef(fit_diag)$pairwise)])
abline(0,1)


#### Density plot
library(svglite)
dev.off()
#pdf(file = "ggplot_dens_susatt_talks.pdf", width = 10, height = 8, useDingbats = FALSE)
svglite::svglite(
  file = "ggplot_dens_susatt_talks.svg",
  width = 10,
  height = 8
)

library(ggplot2)

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


easy_fit = easybgm_compare(data = ADHD[, -1],
                             group_indicator = ADHD[, 1],
                             iter = 2000,
                             package = "bgms",
                             type = "ordinal",
                             seed = 1234)

plot_edgeevidence(easy_fit, layout = layout, legend = FALSE)


dev.off()
pdf(file = "easybgm_diff.pdf", width = 10, height = 10, useDingbats = FALSE)
plot_edgeevidence(easy_fit, layout = layout, legend = FALSE,
                  labels = colnames(ADHD[,-1]), evidence_threshold = 3)
dev.off()





easy_fit = easybgm_compare(data = ADHD[, -1],
                           group_indicator = ADHD[, 1],
                           iter = 2000,
                           package = "bgms",
                           type = "ordinal", difference_prior = "Beta-Bernoulli")
plot_edgeevidence(easy_fit, legend = FALSE,
                  labels = colnames(ADHD[,-1]))



# Prior scale

plot.ecdf(easy_fit$parameters[lower.tri(easy_fit$parameters)])
plot.ecdf(rcauchy(1e3), add = TRUE, col = "blue", lwd = 4)
plot.ecdf(rcauchy(1e3, 0, .25), add = TRUE, col = "darkred", lwd = 4)

easy_fit_scale = easybgm_compare(data = ADHD[, -1],
                                 group_indicator = ADHD[, 1],
                                 iter = 2000,
                                 package = "bgms",
                                 type = "ordinal", 
                                 difference_scale = 0.25)

plot.ecdf(easy_fit_scale$parameters[lower.tri(easy_fit_scale$parameters)])
plot.ecdf(rcauchy(1e3), add = TRUE, col = "blue", lwd = 4)
plot.ecdf(rcauchy(1e3, 0, .25), add = TRUE, col = "darkred", lwd = 4)



easy_fit_scale2 = easybgm_compare(
  data = data,
  group_indicator = group_indicator,
  iter = 2000,
  package = "bgms",
  type = "ordinal",
  seed = 1234,
  difference_scale = 2, 
  progress = FALSE
)

