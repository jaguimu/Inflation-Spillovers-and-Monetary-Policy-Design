# Supplementary Tables 3, 5 and 6: 25 cities, DY and BK
source("R/setup.R")
inf <- load_inf("data/cpi_cities.xlsx")
cty <- colnames(inf)

sp <- dy(inf)
fb <- bk(inf)
bands <- c(short = 2, long = 3)
est <- c(list(time = list(TO = c(sp$TO), FROM = c(sp$FROM), NET = c(sp$NET), TCI = c(sp$TCI))),
         lapply(bands, function(k) list(TO = c(fb$TO[, , k]), FROM = c(fb$FROM[, , k]), NET = c(fb$NET[, , k]), TCI = fb$TCI[k])))

if (RUN_BOOT) {
  bs <- boot_samples(inf)
  bb <- lapply(seq_along(bs), function(i) {
    if (i %% 100 == 0) cat("boot", i, "\n")
    r <- dy(bs[[i]]); f <- bk(bs[[i]])
    c(list(time = list(TO = c(r$TO), FROM = c(r$FROM), NET = c(r$NET), TCI = c(r$TCI))),
      lapply(bands, function(k) list(TO = c(f$TO[, , k]), FROM = c(f$FROM[, , k]), NET = c(f$NET[, , k]), TCI = f$TCI[k])))
  })
  saveRDS(bb, "output/boot/cities_static.rds")
}
has_b <- file.exists("output/boot/cities_static.rds")
if (has_b) bb <- readRDS("output/boot/cities_static.rds")

nm <- c(time = "supp_table3_cities_dy", short = "supp_table5_cities_bk_short", long = "supp_table6_cities_bk_long")
for (s in names(est)) {
  e <- est[[s]]
  if (has_b) {
    g <- function(k) do.call(rbind, lapply(bb, function(z) z[[s]][[k]]))
    t <- data.frame(TO = fmt(e$TO, wald_p(e$TO, g("TO"))), FROM = fmt(e$FROM, wald_p(e$FROM, g("FROM"))),
                    NET = fmt(e$NET, wald_p(e$NET, g("NET"))), row.names = cty)
    t["TSI", ] <- c("", fmt(e$TCI, wald_p(e$TCI, g("TCI"))), "")
  } else {
    t <- round(data.frame(TO = e$TO, FROM = e$FROM, NET = e$NET, row.names = cty), 2)
    t["TSI", ] <- c(NA, round(e$TCI, 2), NA)
  }
  write.csv(t, sprintf("output/tables/%s.csv", nm[s])); save_tex(t, sprintf("output/tables/%s.tex", nm[s]), nm[s])
}
