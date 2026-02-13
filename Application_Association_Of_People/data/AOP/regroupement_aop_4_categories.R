# ============================================================================
# REGROUPEMENT DES AOP FRANÇAISES EN 4 CATÉGORIES
# ============================================================================
# 
# Catégories :
# 1. Boissons
# 2. Produit laitier
# 3. Produit carné
# 4. Autres
#
# ============================================================================

# Chargement des données
# ----------------------------------------------------------------------------
data <- read.csv('2025-10-09-comagri-communes-aires-ao.csv', 
                 sep=';', 
                 fileEncoding='latin1',
                 stringsAsFactors = FALSE)

colnames(data) <- c('CI', 'Departement', 'Commune', 'Art', 'Aire_geographique', 'IDA')
data <- data[, c('CI', 'Departement', 'Commune', 'Aire_geographique', 'IDA')]

# Définition des catégories
# ----------------------------------------------------------------------------

# CATÉGORIE 1 : Boissons
# Toutes les boissons
boissons_patterns <- c(
  "Bourgogne", "Beaujolais", "Bordeaux", "Alsace", "Champagne", 
  "Châteauneuf", "Condrieu", "Cornas", "Côte Rôtie", "Crozes", "Hermitage",
  "Gigondas", "Lirac", "Saint-Joseph", "Saint-Péray", "Tavel", "Vacqueyras",
  "Cairanne", "Rasteau", "Vinsobres", "Côtes du Rhône", "Ventoux", "Luberon",
  "Anjou", "Bourgueil", "Chinon", "Muscadet", "Pouilly-Fumé", "Sancerre",
  "Saumur", "Savennières", "Touraine", "Vouvray", "Montlouis", "Quincy",
  "Reuilly", "Menetou-Salon", "Orléans",
  "Arbois", "Bugey", "Château-Chalon", "L'Etoile", "Macvin", "Roussette",
  "Vin de Savoie", "Seyssel", "Crépy",
  "Banyuls", "Collioure", "Corbières", "Fitou", "Languedoc", "Minervois",
  "Rivesaltes", "Roussillon", "Clairette", "Limoux", "Maury", "Picpoul",
  "Saint-Chinian", "Faugères", "Cabardès",
  "Bandol", "Bellet", "Cassis", "Corse", "Palette", "Patrimonio",
  "Côtes de Provence", "Coteaux d'Aix", "Les Baux",
  "Bergerac", "Buzet", "Cahors", "Fronton", "Gaillac", "Jurançon",
  "Madiran", "Monbazillac", "Pacherenc", "Béarn", "Irouléguy",
  "Marcillac", "Tursan", "Saint-Mont",
  "Chablis", "Meursault", "Puligny", "Chassagne", "Pommard", "Volnay",
  "Gevrey", "Chambolle", "Vosne", "Nuits", "Corton", "Montrachet",
  "Mercurey", "Givry", "Rully", "Montagny", "Mâcon", "Saint-Véran",
  "Brouilly", "Morgon", "Fleurie", "Juliénas", "Chénas", "Moulin-à-Vent",
  "Saint-Amour", "Régnié", "Chiroubles",
  "Pauillac", "Margaux", "Saint-Julien", "Saint-Estèphe", "Pomerol",
  "Saint-Emilion", "Graves", "Sauternes", "Barsac", "Médoc", "Pessac",
  "Fronsac", "Lalande", "Côtes de Bordeaux",
  "Corrèze", "Côtes d'Auvergne", "Saint-Pourçain", "Moselle",
  "Coteaux", "premier cru", "grand cru", "Villages",
  "Armagnac", "Calvados", "Cognac", "Kirsch", "Rhum", "Whisky",
  "Eau-de-vie", "Marc", "Fine", "Pineau", "Floc",
  "Cidre", "Pommeau", "Cornouaille", "Domfront", "Pays d'Auge"
)

# Liste explicite de boissons pour cas spéciaux
boissons_liste <- c(
  "Ajaccio", "Bellet", "Rosé de Loire", "Rosé des Riceys",
  "Irancy", "Vézelay", "Saint-Bris", "Bouzeron",
  "Château-Grillet", "Coulée de Serrant", "Quarts de Chaume", "Bonnezeaux",
  "Jasnières", "Cheverny", "Cour-Cheverny", "Valençay",
  "Châteaumeillant", "Côte roannaise", "Côtes du Forez",
  "Entraygues - Le Fel", "Estaing", "Marcillac",
  "Grignan-les-Adhémar", "Duché d'Uzès", "Costières de Nîmes",
  "Terrasses du Larzac", "Pic Saint-Loup", "La Clape", "La Livinière",
  "Boutenac", "Grés de Montpellier", "Laudun",
  "Brulhois", "Côtes de Millau", "Côtes du Marmandais",
  "Haut-Poitou", "Fiefs Vendéens", "Gros Plant du Pays nantais",
  "Côtes de Duras", "Rosette", "Saussignac", "Montravel",
  "Haut-Montravel", "Côtes de Montravel", "Pécharmant",
  "Côtes de Bergerac", "Saint-Sardos", "Coteaux du Quercy",
  "Coteaux du Lyonnais", "Coteaux du Giennois", "Coteaux du Vendômois",
  "Coteaux du Loir", "Coteaux de l'Aubance", "Coteaux du Layon",
  "Coteaux de Saumur", "Coteaux d'Ancenis", "Coteaux de Die",
  "Coteaux Varois en Provence", "Coteaux champenois",
  "Blanche Armagnac", "Blaye", "Cadillac", "Cérons", "Loupiac",
  "Sainte-Croix-du-Mont", "Entre-deux-Mers", "Graves de Vayres",
  "Premières Côtes de Bordeaux", "Canon Fronsac", "Listrac-Médoc",
  "Moulis", "Haut-Médoc", "Puisseguin-Saint-Emilion",
  "Lussac Saint-Emilion", "Montagne-Saint-Emilion", "Saint-Georges-Saint-Emilion",
  "Crémant d'Alsace", "Crémant de Bordeaux", "Crémant de Bourgogne",
  "Crémant de Die", "Crémant de Limoux", "Crémant de Loire", "Crémant du Jura",
  "Grand Roussillon", "Muscat de Beaumes-de-Venise", "Muscat de Frontignan",
  "Muscat de Lunel", "Muscat de Mireval", "Muscat de Rivesaltes",
  "Muscat de Saint-Jean-de-Minervois", "Muscat du Cap Corse", "Muscat du Ventoux",
  "Rasteau tranquille", "Châtillon-en-Diois", "Pierrevert"
)


# CATÉGORIE 2 : PRODUIT LAITIER
# Tous les fromages + beurres et crèmes
produits_laitiers <- c(
  # Fromages
  "Abondance", "Banon", "Beaufort", "Bleu d'Auvergne",
  "Bleu de Gex haut Jura ou Bleu de Septmoncel", "Bleu des Causses",
  "Bleu du Vercors-Sassenage", "Brie de Meaux", "Brie de Melun",
  "Brocciu", "Brousse du Rove", "Camembert de Normandie", "Cantal",
  "Chabichou du Poitou", "Chaource", "Charolais", "Chavignol",
  "Chevrotin", "Comté", "Epoisses", "Fourme d'Ambert",
  "Fourme de Montbrison", "Laguiole", "Langres", "Livarot",
  "Maroilles", "Mont d'Or ou Vacherin du Haut-Doubs", "Morbier",
  "Mothais sur feuille", "Munster", "Neufchâtel", "Ossau-Iraty",
  "Picodon", "Pont-l'Évêque", "Pouligny-Saint-Pierre", "Pélardon",
  "Reblochon de Savoie", "Rigotte de Condrieu", "Rocamadour",
  "Roquefort", "Saint-Nectaire", "Sainte-Maure de Touraine",
  "Salers", "Selles-sur-Cher", "Tome des Bauges", "Valençay",
  
  # Beurres et crèmes
  "Beurre Charentes-Poitou", "Beurre de Bresse",
  "Beurre et crème d'Isigny", "Crème de Bresse"
)


# CATÉGORIE 3 : PRODUIT CARNÉ
# Toutes les viandes et charcuteries
produits_carnes <- c(
  # Viandes
  "Buf de Charolles", "Barèges-Gavarnie", "Fin Gras du Mézenc",
  "Maine-Anjou", "Porc noir de Bigorre", "Taureau de Camargue",
  "Prés-salés de la baie de Somme", "Prés-salés du Mont-Saint-Michel",
  
  # Volailles
  "Volaille de Bresse ou poulet de Bresse, poularde de Bresse, chapon de ",
  "Dinde de Bresse", "Poulet du Bourbonnais",
  
  # Charcuteries
  "Jambon du Kintoa", "Jambon noir de Bigorre",
  "Jambon sec de Corse ou Jambon sec de Corse - Prisuttu",
  "Coppa de Corse ou Coppa de Corse - Coppa di Corsica",
  "Lonzo de Corse ou Lonzo de Corse - Lonzu", "Kintoa"
)


# Liste des produits "Autres" (liste exhaustive)
# ----------------------------------------------------------------------------
autres_produits_liste <- c(
  # Huiles
  "Huile d'olive d'Aix-en-Provence", "Huile d'olive de Corse - Oliu di Corsica",
  "Huile d'olive de Haute-Provence", "Huile d'olive de Nice",
  "Huile d'olive de Nyons", "Huile d'olive de Nîmes",
  "Huile d'olive de Provence", "Huile d'olive de la vallée des Baux-de-Provence",
  "Huile d'olive du Languedoc", "Huile de noix du Périgord",
  "Huile essentielle de lavande de Haute-Provence",
  # Fruits et légumes
  "Abricots rouges du Roussillon", "Ail violet de Cadours",
  "Béa du Roussillon", "Chasselas de Moissac",
  "Châtaigne d'Ardèche", "Châtaigne des Cévennes",
  "Coco de Paimpol", "Figue de Solliès",
  "Lentille verte du Puy", "Lucques du Languedoc",
  "Mirabelle de Lorraine", "Noix de Grenoble",
  "Noix du Périgord", "Oignon de Roscoff",
  "Oignon doux des Cévennes", "Olive de Nice",
  "Olive de Nîmes", "Pomme de terre de l'Ile de Ré",
  "Pomme du Limousin",
  # Bois
  "Bois de Chartreuse", "Bois du Jura",
  # Produits divers
  "Farine de châtaigne corse  Farina castagnina corsa",
  "Foin de Crau", "Miel de Corse - Mele di Corsica",
  "Miel de sapin des Vosges",
  "Piment d'Espelette ou Piment d'Espelette - Ezpeletako Biperra",
  "Sable de Camargue",
  "Moules de bouchot de la baie du Mont-Saint-Michel"
)


# Fonction de catégorisation
# ----------------------------------------------------------------------------
categoriser_aop <- function(aop) {
  
  # Test Produit laitier (prioritaire)
  if (aop %in% produits_laitiers) {
    return("Produit laitier")
  }
  
  # Test Produit carné
  if (aop %in% produits_carnes) {
    return("Produit carné")
  }
  
  # Test Autres produits (liste exhaustive)
  if (aop %in% autres_produits_liste) {
    return("Autres")
  }
  
  # Test Boissons - liste explicite d'abord
  if (aop %in% boissons_liste) {
    return("Boissons")
  }
  
  # Test Boissons - patterns
  for (pattern in boissons_patterns) {
    if (grepl(pattern, aop, ignore.case = FALSE)) {
      return("Boissons")
    }
  }
  
  # Sinon : Boissons (par défaut pour tout ce qui reste)
  # Car la grande majorité des AOP sont des vins
  return("Boissons")
}


# Application de la catégorisation
# ----------------------------------------------------------------------------
cat("Catégorisation en cours...\n")
data$Categorie <- sapply(data$Aire_geographique, categoriser_aop)


# Résumés et statistiques
# ----------------------------------------------------------------------------
cat("\n========================================\n")
cat("RÉSUMÉ DES CATÉGORIES\n")
cat("========================================\n\n")

# Nombre de communes par catégorie
cat("--- Nombre de communes par catégorie ---\n\n")
resume_communes <- as.data.frame(table(data$Categorie))
colnames(resume_communes) <- c("Catégorie", "Nombre_communes")
resume_communes <- resume_communes[order(-resume_communes$Nombre_communes), ]
print(resume_communes, row.names = FALSE)

# Nombre d'AOP uniques par catégorie
cat("\n\n--- Nombre d'AOP uniques par catégorie ---\n\n")
aop_uniques_cat <- unique(data[, c("Aire_geographique", "Categorie")])
aop_par_categorie <- as.data.frame(table(aop_uniques_cat$Categorie))
colnames(aop_par_categorie) <- c("Catégorie", "Nombre_AOP")
aop_par_categorie <- aop_par_categorie[order(-aop_par_categorie$Nombre_AOP), ]
print(aop_par_categorie, row.names = FALSE)

cat("\n\nTotal AOP dans le fichier:", length(unique(data$Aire_geographique)), "\n")


# Détail de la catégorie "Autres"
# ----------------------------------------------------------------------------
cat("\n\n========================================\n")
cat("DÉTAIL CATÉGORIE 'AUTRES'\n")
cat("========================================\n\n")

autres_aop <- unique(data$Aire_geographique[data$Categorie == "Autres"])
cat("Nombre d'AOP dans 'Autres':", length(autres_aop), "\n\n")
cat("Liste des AOP 'Autres':\n")
cat(paste(sort(autres_aop), collapse = "\n"), "\n")


# Export des résultats
# ----------------------------------------------------------------------------
write.csv(data, "aop_4_categories.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(aop_par_categorie, "resume_4_categories.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# Export liste par catégorie
for (cat_name in unique(data$Categorie)) {
  aop_liste <- unique(data$Aire_geographique[data$Categorie == cat_name])
  aop_liste <- sort(aop_liste)
  filename <- paste0("data/liste_", 
                     gsub("[, &]", "_", cat_name), 
                     ".txt")
  writeLines(aop_liste, filename, useBytes = TRUE)
}

cat("\n========================================\n")
cat("✓ Fichiers exportés avec succès:\n")
cat("  - aop_4_categories.csv (données complètes)\n")
cat("  - resume_4_categories.csv (résumé)\n")
cat("  - liste_*.txt (listes par catégorie)\n")
cat("========================================\n\n")


# Exemples d'utilisation
# ----------------------------------------------------------------------------
cat("\n========================================\n")
cat("EXEMPLES D'UTILISATION\n")
cat("========================================\n\n")

cat("# Filtrer les produits laitiers:\n")
cat("produits_laitiers <- data[data$Categorie == 'Produit laitier', ]\n\n")

cat("# Filtrer les boissons:\n")
cat("boissons <- data[data$Categorie == 'Boissons', ]\n\n")

cat("# Compter les communes par département pour les fromages:\n")
cat("fromages <- data[data$Categorie == 'Produit laitier', ]\n")
cat("table(fromages$Departement)\n\n")

cat("# Liste des AOP carnées:\n")
cat("unique(data$Aire_geographique[data$Categorie == 'Produit carné'])\n\n")
