# Table 1 and Table A1: DY (2012), full sample
source("R/setup.R")
inf <- load_inf("data/cpi_regions.xlsx")
sp <- dy(inf)
tab <- num(sp$TABLE)
npdc <- sp$NPDC[, , 1]
print(sp$TABLE)

est <- data.frame(TO = c(sp$TO), FROM = c(sp$FROM), NET = c(sp$NET), row.names = colnames(inf))
tci <- c(sp$TCI)

if (RUN_BOOT) {
  bs <- boot_samples(inf)
  b <- lapply(seq_along(bs), function(i) {
    if (i %% 100 == 0) cat("boot", i, "\n")
    r <- dy(bs[[i]])
    list(TO = c(r$TO), FROM = c(r$FROM), NET = c(r$NET), TCI = c(r$TCI), NPDC = num(r$NPDC[, , 1]))
  })
  saveRDS(b, "output/boot/dy_static.rds")
}
if (file.exists("output/boot/dy_static.rds")) {
  b <- readRDS("output/boot/dy_static.rds")
  g <- function(k) do.call(rbind, lapply(b, `[[`, k))
  p <- sapply(c("TO", "FROM", "NET"), function(k) wald_p(est[[k]], g(k)))
  t1 <- data.frame(TO = fmt(est$TO, p[, "TO"]), FROM = fmt(est$FROM, p[, "FROM"]),
                   NET = fmt(est$NET, p[, "NET"]), row.names = rownames(est))
  t1["TSI", ] <- c("", fmt(tci, wald_p(tci, g("TCI"))), "")
  pp <- wald_p2(npdc, simplify2array(lapply(b, `[[`, "NPDC")))
  ta1 <- as.data.frame(matrix(fmt(npdc, pp), nrow(npdc), dimnames = dimnames(npdc)))
} else {
  t1 <- round(rbind(est, TSI = c(NA, tci, NA)), 2); ta1 <- as.data.frame(round(npdc, 2))
}
write.csv(t1, "output/tables/table1_dy.csv"); save_tex(t1, "output/tables/table1_dy.tex", "Time domain spillovers")
write.csv(ta1, "output/tables/tableA1_dy_pairwise.csv"); save_tex(ta1, "output/tables/tableA1_dy_pairwise.tex", "Net pairwise spillovers")
write.csv(tab, "output/tables/dy_connectedness_table.csv")
