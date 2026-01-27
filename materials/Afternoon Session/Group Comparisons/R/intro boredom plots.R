library(easybgm)

library(bgms)
?Boredom

# Network estimation
fit = bgm(x = Boredom[, -1], # Remove group ID column
          iter = 1000, # Number of MCMC iterations
          seed = 1234)

fit_easy = easybgm(data = Boredom[, -1], # Remove group ID column
                   iter = 2000, # Number of MCMC iterations
                   package = "bgms",
                   type = "ordinal",
                   seed = 1234)

# Group analyses
french_group = Boredom[Boredom$language == "fr", -1]
english_group = Boredom[Boredom$language != "fr", -1]

fit_fr = bgm(x = french_group,
             iter = 1000, # Number of MCMC iterations
             edge_selection = FALSE, #No edge selection
             seed = 1234)

fit_en = bgm(x = english_group,
             iter = 1000, # Number of MCMC iterations
             edge_selection = FALSE, #No edge selection
             seed = 1234)


layout = qgraph::qgraph(
  input = fit$posterior_mean_pairwise,
  DoNotPlot = TRUE,
  layout = "spring"
)$layout

global_max <- max(
  (fit_fr$posterior_mean_pairwise),
  (fit_en$posterior_mean_pairwise)
)

dev.off()
pdf(file = "easybgm_bored.pdf", width = 10, height = 10, useDingbats = FALSE)
plot_network(fit_easy, legend = FALSE, layout = layout)

dev.off()
pdf(file = "qgraph_bored.pdf", width = 10, height = 10, useDingbats = FALSE)
qgraph::qgraph(
  fit$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(Boredom[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)

dev.off()
pdf(file = "qgraph_bored_fr.pdf", width = 10, height = 10, useDingbats = FALSE)
qgraph::qgraph(
  fit_fr$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(Boredom[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)

dev.off()
pdf(file = "qgraph_bored_en.pdf", width = 10, height = 10, useDingbats = FALSE)
qgraph::qgraph(
  fit_en$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(Boredom[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)
dev.off()


par(mfrow = c(1,2))
qgraph::qgraph(
  fit_fr$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(Boredom[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)
qgraph::qgraph(
  fit_en$posterior_mean_pairwise,
  layout = layout,
  labels = colnames(Boredom[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE,
  maximum = global_max
)


# fit_fr$raw_samples$parameter_names$pairwise[[21]]
samples_fr = do.call(rbind, fit_fr$raw_samples$pairwise)
sample_fr = samples_diag[,21]
samples_en = do.call(rbind, fit_en$raw_samples$pairwise)
sample_en = samples_nodiag[,21]






#### Density plot
dev.off()
pdf(file = "ggplot_dens_stimul_sit.pdf", width = 10, height = 8, useDingbats = FALSE)

library(ggplot2)

# 1. Explicit trimming (illustrative range)
trim_range <- c(-5, 2.5)

sample_fr_trim <- sample_fr[
  sample_fr > trim_range[1] & sample_fr < trim_range[2]
]

sample_en_trim <- sample_en[
  sample_en > trim_range[1] & sample_en < trim_range[2]
]

# 2. Build data frame
df_left <- rbind(
  data.frame(value = sample_fr_trim, group = "French"),
  data.frame(value = sample_en_trim, group = "English")
)

# 3. Means (from full samples, not trimmed)
mu_fr   <- mean(sample_fr_trim)
mu_en <- mean(sample_en_trim)

# 4. Plot
ggplot(df_left, aes(x = value, color = group, fill = group)) +
  geom_density(
    #aes(y = after_stat(scaled)),
    adjust = 3,
    alpha = 0.25,
    linewidth = 0.8
  ) +
  geom_vline(xintercept = mu_en, color = "#F8766D", linewidth = 1) +
  geom_vline(xintercept = mu_fr, color = "#00BFC4", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = trim_range) +
  labs(
    title = "Posterior density for stimulation-sit_around interaction",
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



















# network comparison

fit_compare = bgmCompare(
  x = Boredom[, -1], 
  group_indicator = 1 * (Boredom[, 1] == "fr"),
  difference_selection = FALSE,
  iter = 2e3,
  seed = 1234,
  difference_scale = 2.5)

d = fit_compare$posterior_summary_pairwise_differences[, 2]
diff = matrix(0, nrow = 8, ncol = 8)
diff[upper.tri(diff)] = d
diff = diff + t(diff)

dev.off()
pdf(file = "qgraph_boredom_diff.pdf", width = 10, height = 10, useDingbats = FALSE)
qgraph::qgraph(
  diff,
  layout = layout,
  labels = colnames(Boredom[,-1]),
  theme = "TeamFortress",
  vsize = 10,
  esize = 20,
  legend = FALSE
)




# Sanity checks
plot(coef(fit_compare)$pairwise_effects_groups[,2], 
     coef(fit_en)$pairwise[lower.tri(coef(fit_en)$pairwise)])
abline(0,1)
plot(coef(fit_compare)$pairwise_effects_groups[,1], 
     coef(fit_fr)$pairwise[lower.tri(coef(fit_fr)$pairwise)])
abline(0,1)




easy_fit = easybgm_compare(
  data = Boredom[, -1],
  group_indicator = 1 * (Boredom[, 1] == "fr"),
  iter = 2000,
  package = "bgms",
  type = "ordinal",
  seed = 1234)

plot_edgeevidence(easy_fit, layout = layout, legend = FALSE)


dev.off()
pdf(file = "easybgm_bored_diff.pdf", width = 10, height = 10, useDingbats = FALSE)
plot_edgeevidence(easy_fit, layout = layout, legend = FALSE,
                  labels = colnames(Boredom[,-1]))
dev.off()
