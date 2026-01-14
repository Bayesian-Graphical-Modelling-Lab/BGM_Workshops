#Old values
structure = 1:8
prior = rep(1 / 8, 8)
posterior = c(.01, 0.21, .1,.02,.17,.32,.05,.12)

in_12 = c(2,5,6,8)
ex_12 = c(1,3,4,7)
BF12 = (sum(posterior[in_12]) / sum(posterior[ex_12])) / (sum(prior[in_12])/sum(prior[ex_12]))

#New values
posterior = c(1, 30, 3,2, 25, 49, 4,1)
posterior = posterior / sum(posterior)
posterior = round(posterior, 2)
posterior[6] = posterior[6] - (sum(posterior) - 1)
BF = (sum(posterior[in_]) / sum(posterior[ex_])) / (sum(prior[in_])/sum(prior[ex_]))

in_23 = c(3,6,7,8)
ex_23 = c(1,2,4,5)
BF23 = (sum(posterior[in_23]) / sum(posterior[ex_23])) / (sum(prior[in_23])/sum(prior[ex_23]))

in_13 = c(4,5,7,8)
ex_13 = c(1,2,3,6)
BF13 = (sum(posterior[in_13]) / sum(posterior[ex_13])) / (sum(prior[in_13])/sum(prior[ex_13]))

#Plots
pdata = posterior / prior

par(mar = c(5,4,1,1), cex = 1.7)
plot(structure, prior, axes = FALSE, xlab = "", ylab = "", main = "", ylim = c(0, 1), pch = 21,
     bg = palette.colors(palette = "Okabe-Ito")[2],
     col = palette.colors(palette = "Okabe-Ito")[2])
axis(1)
axis(2, las = 1)
mtext("Structure", line = 4, side = 1, cex = 1.7)
mtext("Prior Probability", line = 3, side = 2, las = 0, cex = 1.7)


par(mar = c(5,4,1,1), cex = 1.7)
plot(structure, posterior, axes = FALSE, xlab = "", ylab = "", main = "", ylim = c(0, 1), pch = 21,
     bg = palette.colors(palette = "Okabe-Ito")[3],
     col = palette.colors(palette = "Okabe-Ito")[3])
axis(1)
axis(2, las = 1)
mtext("Structure", line = 4, side = 1, cex = 1.7)
mtext("Posterior Probability", line = 3, side = 2, las = 0, cex = 1.7)





complexity = c(0,1,1,1,2,2,2,3)
prior = beta(1 + complexity, 1 + 3 - complexity) / beta(1,1)
posterior = pdata * prior / sum(pdata * prior)

par(mar = c(5,4,1,1), cex = 1.7)
plot(structure, prior, axes = FALSE, xlab = "", ylab = "", main = "", ylim = c(0, 1), pch = 21,
     bg = palette.colors(palette = "Okabe-Ito")[2],
     col = palette.colors(palette = "Okabe-Ito")[2])
axis(1)
axis(2, las = 1)
mtext("Structure", line = 4, side = 1, cex = 1.7)
mtext("Prior Probability", line = 3, side = 2, las = 0, cex = 1.7)


par(mar = c(5,4,1,1), cex = 1.7)
plot(structure, posterior, axes = FALSE, xlab = "", ylab = "", main = "", ylim = c(0, 1), pch = 21,
     bg = palette.colors(palette = "Okabe-Ito")[3],
     col = palette.colors(palette = "Okabe-Ito")[3])
axis(1)
axis(2, las = 1)
mtext("Structure", line = 4, side = 1, cex = 1.7)
mtext("Posterior Probability", line = 3, side = 2, las = 0, cex = 1.7)

