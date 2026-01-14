# -------------------------------------------------------------
# Visualizing Beta-Bernoulli Prior and Implied Network Complexity
# -------------------------------------------------------------

library(ggplot2)
library(cowplot)

# --- Parameters --------------------------------------------------------------
alpha <- 3     # Beta parameter α
beta  <- 8     # Beta parameter β
p     <- 8     # Number of nodes
m     <- p * (p - 1) / 2   # Number of possible edges

# --- 1. Beta density on edge inclusion probability --------------------------
pi_vals <- seq(0, 1, length.out = 400)
beta_df <- data.frame(
  pi = pi_vals,
  density = dbeta(pi_vals, alpha, beta)
)

g1 <- ggplot(beta_df, aes(x = pi, y = density)) +
  geom_area(fill = "#74a9cf", alpha = 0.7) +
  geom_line(size = 1, color = "#0570b0") +
  labs(
    title = bquote("Beta(" * .(alpha) * ", " * .(beta) * ") prior on edge probability"),
    x = expression(pi),
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# --- 2. Implied prior on network complexity (number of edges) ---------------
# The marginal prior for k given Beta(α, β) is Beta-Binomial(m, α, β)
k_vals <- 0:m
beta_binom_df <- data.frame(
  k = k_vals,
  prob = choose(m, k_vals) * beta(k_vals + alpha, m - k_vals + beta) / beta(alpha, beta)
)

g2 <- ggplot(beta_binom_df, aes(x = k, y = prob)) +
  geom_col(fill = "#74c476", color = "white") +
  labs(
    title = "Implied prior on network complexity",
    x = "Number of edges (k)",
    y = "Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# --- Combine plots side by side ---------------------------------------------
plot_grid(g1, g2, nrow = 1, rel_widths = c(1, 1.2))
