# Supplementary Tables 1-2 and Figures 1-3: DY robustness (horizon, lags, window)
source("R/setup.R")
inf <- load_inf("data/cpi_regions.xlsx")
reg <- colnames(inf)

# static: H = 1..12 (L = 1); L = 1..6 (H = 3)
stat <- function(fits) {
  m <- function(k) do.call(rbind, lapply(fits, function(r) c(r[[k]])))
  out <- lapply(c(TO = "TO", FROM = "FROM", NET = "NET"), function(k) spread(m(k)))
  t <- data.frame(row.names = reg, TO = out$TO$median, TO_range = out$TO$range,
                  FROM = out$FROM$median, FROM_range = out$FROM$range, NET = out$NET$median, NET_range = out$NET$range)
  s <- spread(m("TCI")); t["TSI", ] <- c("", "", s$median, s$range, "", ""); t
}
t_h <- stat(lapply(1:12, function(h) dy(inf, h = h)))
t_l <- stat(lapply(1:6, function(l) dy(inf, p = l)))
write.csv(t_h, "output/tables/supp_table1_dy_horizon.csv"); write.csv(t_l, "output/tables/supp_table2_dy_lags.csv")
save_tex(t_h, "output/tables/supp_table1_dy_horizon.tex", "Sensitivity to the forecast horizon")
save_tex(t_l, "output/tables/supp_table2_dy_lags.tex", "Sensitivity to the lag length")

# dynamic: mean and [min, max] across specifications
band <- function(fits) {
  n <- min(sapply(fits, function(r) nrow(r$NET)))
  a <- simplify2array(lapply(fits, function(r) tail(r$NET, n)))
  list(d = tail(index(inf), n), mean = apply(a, c(1, 2), mean), min = apply(a, c(1, 2), min), max = apply(a, c(1, 2), max))
}
plot_band <- function(z) {
  df <- do.call(rbind, lapply(seq_along(reg), function(j)
    data.frame(date = z$d, region = reg[j], mean = z$mean[, j], min = z$min[, j], max = z$max[, j])))
  df$region <- factor(df$region, reg)
  ggplot(df, aes(date)) + geom_ribbon(aes(ymin = min, ymax = max), fill = "gray70", alpha = 0.5) +
    geom_line(aes(y = mean), linewidth = 0.3) + facet_wrap(~region, ncol = 3) +
    coord_cartesian(ylim = c(-60, 60)) + scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(x = "", y = "") + theme_minimal(base_size = 14) + theme(strip.text = element_text(face = "bold"))
}
ggsave("output/figures/supp_fig1_dy_horizon.png", plot_band(band(lapply(1:12, function(h) dy(inf, h = h, w = W)))),
       width = 14, height = 9, dpi = 300)
ggsave("output/figures/supp_fig2_dy_lags.png", plot_band(band(lapply(1:6, function(l) dy(inf, p = l, w = W)))),
       width = 14, height = 9, dpi = 300)

rw <- c(100, 150, 200)
df <- do.call(rbind, lapply(rw, function(w) {
  r <- dy(inf, w = w); d <- tail(index(inf), nrow(r$NET))
  do.call(rbind, lapply(seq_along(reg), function(j) data.frame(date = d, region = reg[j], NET = r$NET[, j], RW = paste("RW =", w))))
}))
df$region <- factor(df$region, reg)
g <- ggplot(df, aes(date, NET, color = RW)) + geom_line(linewidth = 0.4) + facet_wrap(~region, ncol = 3) +
  coord_cartesian(ylim = c(-60, 60)) + scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = c("RW = 100" = "red", "RW = 150" = "blue", "RW = 200" = "black")) +
  labs(x = "", y = "", color = "") + theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave("output/figures/supp_fig3_dy_window.png", g, width = 14, height = 9, dpi = 300)
