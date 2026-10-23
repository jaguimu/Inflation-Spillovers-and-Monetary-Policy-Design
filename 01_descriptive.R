# Summary statistics, lag selection and Figure 1
source("R/setup.R")
library(moments); library(tseries)
inf <- load_inf("data/cpi_regions.xlsx")

sumstats <- function(x) {
  s <- function(p) stars(p)
  sapply(as.data.frame(x), function(v) c(
    Mean = sprintf("%.3f%s", mean(v), s(t.test(v)$p.value)),
    SD = sprintf("%.3f", sd(v)),
    Skewness = sprintf("%.3f%s", skewness(v), s(agostino.test(v)$p.value)),
    `Ex.Kurtosis` = sprintf("%.3f%s", kurtosis(v) - 3, s(anscombe.test(v)$p.value)),
    JB = sprintf("%.3f%s", jarque.bera.test(v)$statistic, s(jarque.bera.test(v)$p.value)),
    `Q(1)` = sprintf("%.3f%s", Box.test(v, 1, "Ljung-Box")$statistic, s(Box.test(v, 1, "Ljung-Box")$p.value)),
    `Q(2)` = sprintf("%.3f%s", Box.test(v, 2, "Ljung-Box")$statistic, s(Box.test(v, 2, "Ljung-Box")$p.value))))
}
tab <- as.data.frame(sumstats(inf))
write.csv(tab, "output/tables/summary_stats.csv")
save_tex(tab, "output/tables/summary_stats.tex", "Summary statistics")
write.csv(as.data.frame(t(cor(inf))), "output/tables/correlations.csv")

sel <- VARselect(inf, lag.max = 10, type = "trend")
print(sel$selection)
write.csv(t(sel$criteria), "output/tables/lag_selection.csv")

# Figure 1
df <- fortify.zoo(inf) |> rename(date = Index) |> pivot_longer(-date, names_to = "region", values_to = "inf")
ep <- data.frame(xmin = as.Date(c("2007-01-01", "2017-03-01", "2021-01-01")),
                 xmax = as.Date(c("2009-05-31", "2017-12-31", "2024-12-31")))
g <- ggplot(df, aes(date, inf)) +
  geom_rect(data = ep, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.2) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ region, scales = "free_y") +
  labs(x = "", y = "") + theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))
ggsave("output/figures/fig1_inflation.png", g, width = 12, height = 7, dpi = 300)
