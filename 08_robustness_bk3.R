# Supplementary Table 4 and Figure 4: three bands, [pi, pi/3), [pi/3, pi/12), [pi/12, 0)
source("R/setup.R")
inf <- load_inf("data/cpi_regions.xlsx")
reg <- colnames(inf)
bands <- c(short = 2, medium = 3, long = 4)

sp <- bk(inf, part = PART3)
for (b in names(bands)) print(sp$TABLE[, , bands[b]])
est <- lapply(bands, function(k) list(TO = c(sp$TO[, , k]), FROM = c(sp$FROM[, , k]), NET = c(sp$NET[, , k]), TCI = sp$TCI[k]))

if (RUN_BOOT) {
  bs <- boot_samples(inf)
  bb <- lapply(seq_along(bs), function(i) {
    if (i %% 100 == 0) cat("boot", i, "\n")
    r <- bk(bs[[i]], part = PART3)
    lapply(bands, function(k) list(TO = c(r$TO[, , k]), FROM = c(r$FROM[, , k]), NET = c(r$NET[, , k]), TCI = r$TCI[k]))
  })
  saveRDS(bb, "output/boot/bk3_static.rds")
}
has_b <- file.exists("output/boot/bk3_static.rds")
if (has_b) bb <- readRDS("output/boot/bk3_static.rds")

for (b in names(bands)) {
  tab <- sp$TABLE[, , bands[b]]
  tab <- tab[!(rownames(tab) %in% c("Inc.Own", "NPDC")), ]
  e <- est[[b]]
  if (has_b) {
    g <- function(k) do.call(rbind, lapply(bb, function(z) z[[b]][[k]]))
    tab["TO", seq_along(reg)] <- fmt(e$TO, wald_p(e$TO, g("TO")))
    tab["Net", seq_along(reg)] <- fmt(e$NET, wald_p(e$NET, g("NET")))
    tab[seq_along(reg), "FROM"] <- fmt(e$FROM, wald_p(e$FROM, g("FROM")))
  }
  write.csv(tab, sprintf("output/tables/supp_table4_bk3_%s.csv", b))
  save_tex(as.data.frame(tab), sprintf("output/tables/supp_table4_bk3_%s.tex", b), paste("Three bands,", b, "run"))
}

# Figure 4: rolling NET by band
fr <- bk(inf, part = PART3, w = W)
d <- tail(index(inf), nrow(fr$TCI))
df <- do.call(rbind, lapply(seq_along(reg), function(j) data.frame(
  date = d, region = reg[j], `Short-run` = fr$NET[, j, 2], `Medium-run` = fr$NET[, j, 3], `Long-run` = fr$NET[, j, 4],
  check.names = FALSE))) |> pivot_longer(-c(date, region), names_to = "band", values_to = "NET")
df$region <- factor(df$region, reg)
g <- ggplot(df, aes(date, NET, color = band)) + geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray", linewidth = 0.1) +
  scale_color_manual(values = c(`Short-run` = "black", `Medium-run` = "blue", `Long-run` = "green")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + coord_cartesian(ylim = c(-30, 60)) +
  facet_wrap(~region, ncol = 3) + labs(x = "", y = "", color = "") + theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave("output/figures/supp_fig4_bk3_net.png", g, width = 14, height = 9, dpi = 300)
