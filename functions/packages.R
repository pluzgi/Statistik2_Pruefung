# Liste der benötigten Pakete
packages <- c("scales", "tidyverse", "ggplot2", "readr", "knitr", "kableExtra", "car", "MASS", "psych", "nortest", "corrplot", "lmtest", "DataExplorer", "dplyr")

# Installiere fehlende Pakete
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Lade alle Pakete
lapply(packages, library, character.only = TRUE)