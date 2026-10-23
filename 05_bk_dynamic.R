# Figures 4-6: rolling BK (2018), bootstrap bands
source("R/setup.R")
inf <- load_inf("data/cpi_regions.xlsx")
sp <- bk(inf, w = W)
d <- tail(index(inf), nrow(sp$TCI))
reg <- colnames(inf)
bands <- c(short = 2, long = 3)

if (RUN_BOOT) {
  bs <- boot_samples(inf)
  b <- lapply(seq_along(bs), function(i) {
    if (i %% 50 == 0) cat("boot", i, "\n")
    r <- bk(bs[[i]], w = W)
    list(TCI = r$TCI[, bands], TO = r$TO[, , bands], FROM = r$FROM[, , bands], NET = r$NET[, , bands])
  })
  saveRDS(lapply(c(TCI = "TCI", TO = "TO", FROM = "FROM", NET = "NET"),
                 function(k) simplify2array(lapply(b, `[[`, k))), "output/boot/bk_dynamic.rds")
}
b <- readRDS("output/boot/bk_dynamic.rds")
sdv <- function(a) apply(a, seq_len(length(dim(a)) - 1), sd, na.rm = TRUE)

# Figure 4
ts <- sdv(b$TCI)
ttl <- c(short = "(A) Short-Term", long = "(B) Long-Term")
p4 <- lapply(seq_along(bands), function(k) plot_ci(d, sp$TCI[, bands[k]], ts[, k], ttl[k], c(0, 65)))
ggsave("output/figures/fig4_bk_tci.png", wrap_plots(p4, ncol = 2), width = 12, height = 5, dpi = 300)
write.csv(data.frame(date = d, short = sp$TCI[, 2], long = sp$TCI[, 3]), "output/tables/bk_dynamic_tci.csv", row.names = FALSE)

# Figures 5-6 (NET), TO and FROM
for (k in c("NET", "TO", "FROM")) {
  s <- sdv(b[[k]])
  for (j in seq_along(bands)) {
    m <- sp[[k]][, , bands[j]]
    write.csv(data.frame(date = d, m, check.names = FALSE),
              sprintf("output/tables/bk_dynamic_%s_%s.csv", tolower(k), names(bands)[j]), row.names = FALSE)
    yl <- if (k == "NET") c(-50, 50) else c(0, 100)
    ps <- lapply(seq_along(reg), function(i) plot_ci(d, m[, i], s[, i, j], reg[i], yl, zero = k == "NET", size = 0.6))
    f <- if (k == "NET") c("fig5_bk_net_short", "fig6_bk_net_long")[j] else sprintf("bk_%s_%s", tolower(k), names(bands)[j])
    ggsave(sprintf("output/figures/%s.png", f), wrap_plots(ps, ncol = 3), width = 14, height = 9, dpi = 300)
  }
}
