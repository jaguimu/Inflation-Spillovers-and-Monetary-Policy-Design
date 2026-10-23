# Full replication. Bootstrap (5,000 draws) is slow: Sys.setenv(RUN_BOOT = "FALSE") for point estimates only.
# 03 and 05 need the bootstrap files in output/boot.
for (f in c("01_descriptive.R", "02_dy_static.R", "03_dy_dynamic.R", "04_bk_static.R",
            "05_bk_dynamic.R", "06_robustness_dy.R", "07_robustness_cities.R", "08_robustness_bk3.R")) {
  message(f); source(f, local = new.env())
}
