# Table 2 and Table A2: BK (2018), short run [pi, pi/3), long run [pi/3, 0)
source("R/setup.R")
inf <- load_inf("data/cpi_regions.xlsx")
sp <- bk(inf)
bands <- c(short = 2, long = 3)
for (b in names(bands)) print(sp$TABLE[, , bands[b]])

est <- lapply(bands, function(k) list(TO = c(sp$TO[, , k]), FROM = c(sp$FROM[, , k]), NET = c(sp$NET[, , k]),
                                      TCI = sp$TCI[k], NPDC = sp$NPDC[, , 1, k]))
if (RUN_BOOT) {
  bs <- boot_samples(inf)
  bb <- lapply(seq_along(bs), function(i) {
    if (i %% 100 == 0) cat("boot", i, "\n")
    r <- bk(bs[[i]])
    lapply(bands, function(k) list(TO = c(r$TO[, , k]), FROM = c(r$FROM[, , k]), NET = c(r$NET[, , k]),
                                   TCI = r$TCI[k], NPDC = num(r$NPDC[, , 1, k])))
  })
  saveRDS(bb, "output/boot/bk_static.rds")
}
has_b <- file.exists("output/boot/bk_static.rds")
if (has_b) bb <- readRDS("output/boot/bk_static.rds")

for (b in names(bands)) {
  e <- est[[b]]
  if (has_b) {
    g <- function(k) do.call(rbind, lapply(bb, function(z) z[[b]][[k]]))
    t2 <- data.frame(TO = fmt(e$TO, wald_p(e$TO, g("TO"))), FROM = fmt(e$FROM, wald_p(e$FROM, g("FROM"))),
                     NET = fmt(e$NET, wald_p(e$NET, g("NET"))), row.names = colnames(inf))
    t2["FSI", ] <- c("", fmt(e$TCI, wald_p(e$TCI, g("TCI"))), "")
    pp <- wald_p2(e$NPDC, simplify2array(lapply(bb, function(z) z[[b]]$NPDC)))
    ta2 <- as.data.frame(matrix(fmt(e$NPDC, pp), nrow(e$NPDC), dimnames = dimnames(e$NPDC)))
  } else {
    t2 <- round(data.frame(TO = e$TO, FROM = e$FROM, NET = e$NET, row.names = colnames(inf)), 2)
    t2["FSI", ] <- c(NA, round(e$TCI, 2), NA); ta2 <- as.data.frame(round(e$NPDC, 2))
  }
  write.csv(t2, sprintf("output/tables/table2_bk_%s.csv", b))
  save_tex(t2, sprintf("output/tables/table2_bk_%s.tex", b), paste("Frequency domain spillovers,", b, "run"))
  write.csv(ta2, sprintf("output/tables/tableA2_bk_pairwise_%s.csv", b))
  save_tex(ta2, sprintf("output/tables/tableA2_bk_pairwise_%s.tex", b), paste("Net pairwise spillovers,", b, "run"))
}
