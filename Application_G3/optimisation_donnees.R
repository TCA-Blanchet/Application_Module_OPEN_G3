library(sf)
library(dplyr)

communes_light <- readRDS("data/processed/communes_light.rds")
departements <- readRDS("data/processed/departements.rds")
aop_communes <- readRDS("data/processed/aop_communes.rds")
aop_centroides <- readRDS("data/processed/aop_centroides.rds")

cat("Tailles AVANT optimisation:\n")
cat("  communes_light:", round(object.size(communes_light)/1024^2, 2), "MB\n")
cat("  departements:", round(object.size(departements)/1024^2, 2), "MB\n")
cat("  aop_communes:", round(object.size(aop_communes)/1024^2, 2), "MB\n")
cat("  aop_centroides:", round(object.size(aop_centroides)/1024^2, 2), "MB\n")

communes_avec_aop <- unique(aop_communes$CI)
communes_light <- communes_light %>%
  filter(INSEE_COM %in% communes_avec_aop)

communes_light <- st_simplify(communes_light, dTolerance = 200)
departements <- st_simplify(departements, dTolerance = 500)
aop_communes <- st_simplify(aop_communes, dTolerance = 200)

communes_light <- st_set_precision(communes_light, 1000000)
departements <- st_set_precision(departements, 1000000)
aop_communes <- st_set_precision(aop_communes, 1000000)

communes_light <- st_cast(communes_light, "MULTIPOLYGON")
departements <- st_cast(departements, "MULTIPOLYGON")
aop_communes <- st_cast(aop_communes, "MULTIPOLYGON")

saveRDS(communes_light, "data/processed/communes_light.rds", compress = "xz")
saveRDS(departements, "data/processed/departements.rds", compress = "xz")
saveRDS(aop_communes, "data/processed/aop_communes.rds", compress = "xz")
saveRDS(aop_centroides, "data/processed/aop_centroides.rds", compress = "xz")

cat("\nTailles APRÈS optimisation:\n")
cat("  communes_light:", round(file.info("data/processed/communes_light.rds")$size/1024^2, 2), "MB\n")
cat("  departements:", round(file.info("data/processed/departements.rds")$size/1024^2, 2), "MB\n")
cat("  aop_communes:", round(file.info("data/processed/aop_communes.rds")$size/1024^2, 2), "MB\n")
cat("  aop_centroides:", round(file.info("data/processed/aop_centroides.rds")$size/1024^2, 2), "MB\n")

total_avant <- 91.19 + 9.43 + 67.61 + 0.03
total_apres <- file.info("data/processed/communes_light.rds")$size/1024^2 +
  file.info("data/processed/departements.rds")$size/1024^2 +
  file.info("data/processed/aop_communes.rds")$size/1024^2 +
  file.info("data/processed/aop_centroides.rds")$size/1024^2

cat("\n======================================\n")
cat("TOTAL AVANT:", round(total_avant, 2), "MB\n")
cat("TOTAL APRÈS:", round(total_apres, 2), "MB\n")
cat("GAIN:", round((1 - total_apres/total_avant) * 100, 1), "%\n")
cat("======================================\n")