load('sv_bs_ta_sc_comparison.RData')
library(tidyverse)
library(salso)
# Applying SALSO----------------------------------------------------------
set.seed(1)

sals <- salso(fit_3$Si, loss = 'binder'
              ,maxNClusters = 5)

# Adding clusters as a new column in train data
sals_clusters <- as.vector(sals)
data_w_clus <- as.data.frame(cbind(sals_clusters,train))

# Cluster Win % -----------------------------------------------------------
c1_games <- data_w_clus %>% filter(sals_clusters == 1)
win_per_c1 <- mean(as.numeric(c1_games$H_Win) - 1)

c2_games <- data_w_clus %>% filter(sals_clusters == 2)
win_per_c2 <- mean(as.numeric(c2_games$H_Win) - 1)

c3_games <- data_w_clus %>% filter(sals_clusters == 3)
win_per_c3 <- mean(as.numeric(c3_games$H_Win) - 1)

c4_games <- data_w_clus %>% filter(sals_clusters == 4)
win_per_c4 <- mean(as.numeric(c4_games$H_Win) - 1)

c5_games <- data_w_clus %>% filter(sals_clusters == 5)
win_per_c5 <- mean(as.numeric(c5_games$H_Win) - 1)



# Cluster covariate distributions----------------------------------

# Cluster 1
plot(density(c1_games$H_BS_PM_Stand),
     main = "Cluster 1: Density Plot of H_BS_PM_Stand",
     xlab = "Home Blocked Shots / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'mediumaquamarine',
     lwd = 2)
abline(v=c(quantile(c1_games$H_BS_PM_Stand, .025),quantile(c1_games$H_BS_PM_Stand, .975)), lty = 2)
abline(v=mean(c1_games$H_BS_PM_Stand), lty = 2)

plot(density(c1_games$H_SV_PM_Stand),
     main = "Cluster 1: Density Plot of H_SV_PM_Stand",
     xlab = "Home Saves / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'mediumaquamarine',
     lwd = 2)
abline(v=c(quantile(c1_games$H_SV_PM_Stand, .025),quantile(c1_games$H_SV_PM_Stand, .975)), lty = 2)
abline(v=mean(c1_games$H_SV_PM_Stand), lty = 2)

plot(density(c1_games$H_TA_PM_Stand),
     main = "Cluster 1: Density Plot of H_TA_PM_Stand",
     xlab = "Home Takeaways / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'mediumaquamarine',
     lwd = 2)
abline(v=c(quantile(c1_games$H_TA_PM_Stand, .025),quantile(c1_games$H_TA_PM_Stand, .975)), lty = 2)
abline(v=mean(c1_games$H_TA_PM_Stand), lty = 2)

plot(density(c1_games$H_SC_PM_Stand),
     main = "Cluster 1: Density Plot of H_SC_PM_Stand",
     xlab = "Home Scoring Chances / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'mediumaquamarine',
     lwd = 2)
abline(v=c(quantile(c1_games$H_SC_PM_Stand, .025),quantile(c1_games$H_SC_PM_Stand, .975)), lty = 2)
abline(v=mean(c1_games$H_SC_PM_Stand), lty = 2)

# Cluster 2
plot(density(c2_games$H_BS_PM_Stand),
     main = "Cluster 2: Density Plot of H_BS_PM_Stand",
     xlab = "Home Blocked Shots / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'darkorange',
     lwd = 2)
abline(v=c(quantile(c2_games$H_BS_PM_Stand, .025),quantile(c2_games$H_BS_PM_Stand, .975)), lty = 2)
abline(v=mean(c2_games$H_BS_PM_Stand), lty = 2)

plot(density(c2_games$H_SV_PM_Stand),
     main = "Cluster 2: Density Plot of H_SV_PM_Stand",
     xlab = "Home Saves / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'darkorange',
     lwd = 2)
abline(v=c(quantile(c2_games$H_SV_PM_Stand, .025),quantile(c2_games$H_SV_PM_Stand, .975)), lty = 2)
abline(v=mean(c2_games$H_SV_PM_Stand), lty = 2)

plot(density(c2_games$H_TA_PM_Stand),
     main = "Cluster 2: Density Plot of H_TA_PM_Stand",
     xlab = "Home Takeaways / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'darkorange',
     lwd = 2)
abline(v=c(quantile(c2_games$H_TA_PM_Stand, .025),quantile(c2_games$H_TA_PM_Stand, .975)), lty = 2)
abline(v=mean(c2_games$H_TA_PM_Stand), lty = 2)


plot(density(c2_games$H_SC_PM_Stand),
     main = "Cluster 2: Density Plot of H_SC_PM_Stand",
     xlab = "Home Scoring Chances / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'darkorange',
     lwd = 2)
abline(v=c(quantile(c2_games$H_SC_PM_Stand, .025),quantile(c2_games$H_SC_PM_Stand, .975)), lty = 2)
abline(v=mean(c2_games$H_SC_PM_Stand), lty = 2)

# Cluster 3
plot(density(c3_games$H_BS_PM_Stand))
abline(v=c(quantile(c3_games$H_BS_PM_Stand, .025),quantile(c3_games$H_BS_PM_Stand, .975)), lty = 2)
abline(v=mean(c3_games$H_BS_PM_Stand), lty = 2)

plot(density(c3_games$H_SV_PM_Stand))
abline(v=c(quantile(c3_games$H_SV_PM_Stand, .025),quantile(c3_games$H_SV_PM_Stand, .975)), lty = 2)
abline(v=mean(c3_games$H_SV_PM_Stand), lty = 2)

plot(density(c3_games$H_TA_PM_Stand))
abline(v=c(quantile(c3_games$H_TA_PM_Stand, .025),quantile(c3_games$H_TA_PM_Stand, .975)), lty = 2)
abline(v=mean(c3_games$H_TA_PM_Stand), lty = 2)

plot(density(c3_games$H_SC_PM_Stand))
abline(v=c(quantile(c3_games$H_SC_PM_Stand, .025),quantile(c3_games$H_SC_PM_Stand, .975)), lty = 2)
abline(v=mean(c3_games$H_SC_PM_Stand), lty = 2)

# Cluster 4
plot(density(c4_games$H_BS_PM_Stand))
abline(v=c(quantile(c4_games$H_BS_PM_Stand, .025),quantile(c4_games$H_BS_PM_Stand, .975)), lty = 2)
abline(v=mean(c4_games$H_BS_PM_Stand), lty = 2)

plot(density(c4_games$H_SV_PM_Stand))
abline(v=c(quantile(c4_games$H_SV_PM_Stand, .025),quantile(c4_games$H_SV_PM_Stand, .975)), lty = 2)
abline(v=mean(c4_games$H_SV_PM_Stand), lty = 2)

plot(density(c4_games$H_TA_PM_Stand))
abline(v=c(quantile(c4_games$H_TA_PM_Stand, .025),quantile(c4_games$H_TA_PM_Stand, .975)), lty = 2)
abline(v=mean(c4_games$H_TA_PM_Stand), lty = 2)

plot(density(c4_games$H_SC_PM_Stand))
abline(v=c(quantile(c4_games$H_SC_PM_Stand, .025),quantile(c4_games$H_SC_PM_Stand, .975)), lty = 2)
abline(v=mean(c4_games$H_SC_PM_Stand), lty = 2)

# Cluster 5
plot(density(c5_games$H_BS_PM_Stand),
     main = "Cluster 5: Density Plot of H_BS_PM_Stand",
     xlab = "Home Blocked Shots / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'palegreen',
     lwd = 2)
abline(v=c(quantile(c5_games$H_BS_PM_Stand, .025),quantile(c5_games$H_BS_PM_Stand, .975)), lty = 2)
abline(v=mean(c5_games$H_BS_PM_Stand), lty = 2)

plot(density(c5_games$H_SV_PM_Stand),
     main = "Cluster 5: Density Plot of H_SV_PM_Stand",
     xlab = "Home Saves / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'palegreen',
     lwd = 2)
abline(v=c(quantile(c5_games$H_SV_PM_Stand, .025),quantile(c5_games$H_SV_PM_Stand, .975)), lty = 2)
abline(v=mean(c5_games$H_SV_PM_Stand), lty = 2)

plot(density(c5_games$H_TA_PM_Stand),
     main = "Cluster 5: Density Plot of H_TA_PM_Stand",
     xlab = "Home Takeaways / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'palegreen',
     lwd = 2)
abline(v=c(quantile(c5_games$H_TA_PM_Stand, .025),quantile(c5_games$H_TA_PM_Stand, .975)), lty = 2)
abline(v=mean(c5_games$H_TA_PM_Stand), lty = 2)

plot(density(c5_games$H_SC_PM_Stand),
     main = "Cluster 5: Density Plot of H_SC_PM_Stand",
     xlab = "Home Scoring Chances / Minute of Gameplay, Standardized",
     ylab = "Density",
     col = 'palegreen',
     lwd = 2)
abline(v=c(quantile(c5_games$H_SC_PM_Stand, .025),quantile(c5_games$H_SC_PM_Stand, .975)), lty = 2)
abline(v=mean(c5_games$H_SC_PM_Stand), lty = 2)

# Adding cluster, win labels ----------------------------------------------------------

data_w_clus$name_clusters <- 0
data_w_clus$name_clusters[which(data_w_clus$sals_clusters == 1)] <- 'Cluster 1'

data_w_clus$name_clusters[which(data_w_clus$sals_clusters == 2)] <- 'Cluster 2'

data_w_clus$name_clusters[which(data_w_clus$sals_clusters == 3)] <- 'Cluster 3'

data_w_clus$name_clusters[which(data_w_clus$sals_clusters == 4)] <- 'Cluster 4'

data_w_clus$name_clusters[which(data_w_clus$sals_clusters == 5)] <- 'Cluster 5'

data_w_clus$name_clusters[which(data_w_clus$sals_clusters == 6)] <- 'Cluster 6'

data_w_clus$H_Win_name <- 0
data_w_clus$H_Win_name[which(data_w_clus$H_Win == 0)] <- 'Loss'
data_w_clus$H_Win_name[which(data_w_clus$H_Win == 1)] <- 'Win'

# 3D plot ----------------------------------------------------------------
library(plotly)



legend_labels <- c(
  paste("Cluster 1 (", round(win_per_c1, 3) * 100, "% Wins)"),
  paste("Cluster 2 (", round(win_per_c2, 3)* 100, "% Wins)"),
  paste("Cluster 3 (", round(win_per_c3, 3)* 100, "% Wins)"),
  paste("Cluster 4 (", round(win_per_c4, 3)* 100, "% Wins)"),
  paste("Cluster 5 (", round(win_per_c5, 3)* 100, "% Wins)")
)



data_w_clus$legend_labels <- factor(data_w_clus$sals_clusters, labels = legend_labels)


new_fig <- plot_ly(
  data = data_w_clus,
  x = ~H_SV_PM_Stand,
  y = ~H_TA_PM_Stand,
  #y = ~H_SC_PM_Stand,
  z = ~H_BS_PM_Stand,
  type = "scatter3d",
  mode = "markers",
  color = ~legend_labels,
  alpha = 1,
  symbol = ~H_Win_name,
  symbols = c('circle', 'x'),
  marker = list(size = 5, line = list(color = 'rgb(0,0,0)', width = 0)))


# Adding plot labels----------
x_ax <- 'Std. Saves per Minute'
y_ax <- 'Std. Takeaways per Minute'
# y_ax <- 'Std. Scoring Chances per Minute'
z_ax <- 'Std. Blocked Shots per Minute'


new_fig <- new_fig %>% layout(
  scene = list(
    xaxis = list(title = x_ax),
    yaxis = list(title = y_ax),
    zaxis = list(title = z_ax),
  legend = list(title = list(text = 'Clusters'))
))

new_fig
# can filter desired clusters and rotate about axes, try with TA instead of SC
