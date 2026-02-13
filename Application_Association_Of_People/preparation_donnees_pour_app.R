library(sf)
library(dplyr)
library(readr)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

communes <- st_read("data/carte/COMMUNE.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  select('INSEE_COM', 'NOM', 'INSEE_DEP', 'geometry')

departements <- st_read("data/carte/DEPARTEMENT.shp", quiet = TRUE) %>%
  st_transform(4326)

aop_data <- read_csv("data/AOP/aop_4_categories.csv", show_col_types = FALSE) %>%
  rename(AOP = 'Aire_geographique')

aop_data <- aop_data %>%
  mutate(Categorie = case_when(
    Categorie == "Produit carné" ~ "Produits Carnés",
    Categorie == "Produit laitier" ~ "Produits Laitiers",
    TRUE ~ Categorie
  ))

aop_communes <- aop_data %>%
  left_join(communes, by = c("CI" = "INSEE_COM")) %>%
  filter(!is.na(geometry)) %>%
  st_as_sf()

aop_centroides <- aop_communes %>%
  group_by(AOP) %>%
  summarise(
    n_communes = n(),
    departements = paste(unique(Departement), collapse = ", "),
    categorie = first(Categorie),
    .groups = "drop"
  ) %>%
  st_set_agr("constant") %>%
  st_centroid()

saveRDS(communes, "data/processed/communes_light.rds")
saveRDS(departements, "data/processed/departements.rds")
saveRDS(aop_communes, "data/processed/aop_communes.rds")
saveRDS(aop_centroides, "data/processed/aop_centroides.rds")

cat(sprintf("\n✓ %s communes | %s départements | %s AOP\n", 
            nrow(communes), nrow(departements), nrow(aop_centroides)))