# Figures 2-3: rolling DY (2012), bootstrap bands
source("R/setup.R")
inf <- load_inf("data/cpi_regions.xlsx")
sp <- dy(inf, w = W)
d <- tail(index(inf), nrow(sp$TCI))
reg <- colnames(inf)

if (RUN_BOOT) {
  bs <- boot_samples(inf)
  b <- lapply(seq_along(bs), function(i) {
    if (i %% 50 == 0) cat("boot", i, "\n")
    r <- dy(bs[[i]], w = W)
    list(TCI = c(r$TCI), TO = r$TO, FROM = r$FROM, NET = r$NET)
  })
  saveRDS(list(TCI = sapply(b, `[[`, "TCI"), TO = simplify2array(lapply(b, `[[`, "TO")),
               FROM = simplify2array(lapply(b, `[[`, "FROM")), NET = simplify2array(lapply(b, `[[`, "NET"))),
          "output/boot/dy_dynamic.rds")
}
b <- readRDS("output/boot/dy_dynamic.rds")
sdv <- function(a) apply(a, seq_len(length(dim(a)) - 1), sd, na.rm = TRUE)

tci_sd <- sdv(b$TCI)
write.csv(data.frame(date = d, TCI = c(sp$TCI), lb = c(sp$TCI) - 1.96 * tci_sd, ub = c(sp$TCI) + 1.96 * tci_sd),
          "output/tables/dy_dynamic_tci.csv", row.names = FALSE)
ggsave("output/figures/fig2_dy_tci.png", plot_ci(d, c(sp$TCI), tci_sd, ylim = c(60, 80)), width = 10, height = 5, dpi = 300)

for (k in c("NET", "TO", "FROM")) {
  s <- sdv(b[[k]]); m <- sp[[k]]
  write.csv(data.frame(date = d, m, check.names = FALSE), sprintf("output/tables/dy_dynamic_%s.csv", tolower(k)), row.names = FALSE)
  yl <- if (k == "NET") c(-50, 50) else c(0, 120)
  ps <- lapply(seq_along(reg), function(j) plot_ci(d, m[, j], s[, j], reg[j], yl, zero = k == "NET", size = 0.6))
  f <- c(NET = "fig3_dy_net", TO = "dy_to", FROM = "dy_from")[k]
  ggsave(sprintf("output/figures/%s.png", f), wrap_plots(ps, ncol = 3), width = 14, height = 9, dpi = 300)
}
