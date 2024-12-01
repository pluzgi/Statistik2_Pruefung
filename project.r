# Einlesen der Pakete
source("functions/packages.R")

# 1. Datenauswahl und Beschreibung: ----
# 1.1. Datensatz wählen: 
file_path_to <- "data/input/cleaned_vehicle_dataset.csv"
# Datensatz einlesen
data <-read.csv(file_path_to)

# 1.2. Beschreibung: 
str(data)

# Inhalt des Datensatzes: Automobilverkäufe in den USA (cleaned_vehicle_dataset.csv)
# Quelle: Der Datensatz wurde bereitgestellt und enthält Daten zu Fahrzeugverkäufen
# in den USA, die verschiedene numerische und kategoriale Variablen umfassen.
# Er ist vollständig und weist keine fehlenden Daten auf.
# Der Datensatz enthält Verkaufsinformationen über Fahrzeuge in den USA und umfasst
# 265.640 Beobachtungen mit 22 Variablen. Ziel ist es, Faktoren zu identifizieren,
# die den Preis von Fahrzeugen beeinflussen, und durch statistische Tests
# und Modellierungen Erkenntnisse zu gewinnen. Besonderer Fokus liegt auf der Analyse
# von Bedingungen, die den Fahrzeugpreis beeinflussen, sowie auf geographischen und
# fahrzeugbezogenen Eigenschaften.


# A.Erstellen eines Profiling-Reports
create_report(data)
# Beobachtungen: 265.640
# Variablen: 22 (davon 16 kategoriale und 6 numerische).

# 2. Explorative Datenanalyse
# 2.1 Deskriptive Statistik
# 2.1.1 Berechnung von zentralen Tendenzen und Streuungsmaßen für relevante numerische Variablen.
# Auswahl der relevanten numerischen Variablen
# price: Zielvariable
# odometer: Kilometerstand, ein potenzieller Prädiktor für den Preis
# year: Baujahr des Fahrzeugs, zeigt mögliche Alterseffekte auf den Preis
# lat: Geografische Breite, um regionale Preisunterschiede zu analysieren

numerics <- data %>% dplyr::select(price, odometer, year, lat)
numeric_stats <- data.frame(
  Variable = colnames(numerics),
  Mittelwert = sapply(numerics, mean, na.rm = TRUE),
  Median = sapply(numerics, median, na.rm = TRUE),
  Minimum = sapply(numerics, min, na.rm = TRUE),
  Maximum = sapply(numerics, max, na.rm = TRUE),
  Standardabweichung = sapply(numerics, sd, na.rm = TRUE)
)
print(numeric_stats)

# 2.1.2 Häufigkeiten und Beschreibungen für Kategoriale Variablen
# Auswahl der relevanten kategorialen Variablen
# condition: Zustand des Fahrzeugs, ein direkter Indikator für den Preis
# manufacturer: Hersteller des Fahrzeugs, ein möglicher Einflussfaktor auf den Preis
# drive: Antriebsart (4WD, FWD, RWD), kann Preisunterschiede erklären
# fuel: Art des Kraftstoffs (Gas, Diesel, Elektro), potenziell relevante Preisfaktoren
categoricals <- data %>% dplyr::select(condition, manufacturer, drive, fuel)

# Häufigkeiten der kategorialen Variablen
cat_frequencies <- lapply(categoricals, table)
cat_frequencies

# 2.1 Deskriptive Statistik -----

# Auswahl von numerischen Variablen
numerics <- data %>% select_if(is.numeric)

# Berechnung der zentralen Tendenzen und Streuungsmaße
numeric_stats <- data.frame(
  Variable = colnames(numerics),
  Mittelwert = sapply(numerics, mean, na.rm = TRUE),
  Median = sapply(numerics, median, na.rm = TRUE),
  Minimum = sapply(numerics, min, na.rm = TRUE),
  Maximum = sapply(numerics, max, na.rm = TRUE),
  Standardabweichung = sapply(numerics, sd, na.rm = TRUE)
)

# Ausgabe der deskriptiven Statistik
print(numeric_stats)

# Auswahl relevanter Spalten zur Umwandlung in Faktoren
data <- data %>%
  mutate(
    condition = as.factor(condition),
    manufacturer = as.factor(manufacturer),
    drive = as.factor(drive),
    fuel = as.factor(fuel),
    transmission = as.factor(transmission),
    size = as.factor(size),
    type = as.factor(type)
  )

# Analyse für nominale und ordinale Variablen
categoricals <- data %>% select_if(is.factor)

# Häufigkeiten für kategorische Variablen
cat_frequencies <- lapply(categoricals, table)
cat_frequencies



# 2.2 Grafische Darstellung -----

# Histogramme -----
# Histogramm: Verkaufspreis
ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "#bc785a", color = "black") +
  ggtitle("Histogramm des Verkaufspreises") +
  xlab("Preis") +
  ylab("Häufigkeit")

# Histogramm: Kilometerstand
ggplot(data, aes(x = odometer)) +
  geom_histogram(bins = 30, fill = "#bc785a", color = "black") +
  ggtitle("Histogramm des Kilometerstands") +
  xlab("Kilometerstand") +
  ylab("Häufigkeit") +
  scale_x_continuous(labels = scales::comma)

# Boxplots -----

# Boxplot: Verkaufspreis nach Zustand
ggplot(data, aes(x = condition, y = price)) +
  geom_boxplot(fill = "#00cadc") +
  ggtitle("Verkaufspreis nach Zustand") +
  xlab("Zustand") +
  ylab("Preis")

# Boxplot: Verkaufspreis nach Antriebsart
ggplot(data, aes(x = drive, y = price)) +
  geom_boxplot(fill = "#00cadc") +
  ggtitle("Verkaufspreis nach Antriebsart") +
  xlab("Antriebsart") +
  ylab("Preis")

# Streudiagramme -----

# Streudiagramm: Kilometerstand vs. Preis
ggplot(data, aes(x = odometer, y = price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggtitle("Kilometerstand vs. Verkaufspreis") +
  xlab("Kilometerstand") +
  ylab("Preis")

# Streudiagramm: Preis vs. Baujahr
ggplot(data, aes(x = year, y = price)) +
  geom_point(alpha = 0.6, color = "#bc785a") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggtitle("Baujahr vs. Verkaufspreis") +
  xlab("Baujahr") +
  ylab("Preis") +
  scale_x_continuous(breaks = seq(min(data$year, na.rm = TRUE), 
                                  max(data$year, na.rm = TRUE), 
                                  by = 1))

# Empirische Verteilungsfunktion -----

# Empirische Verteilungsfunktion des Verkaufspreises
ggplot(data, aes(x = price)) +
  stat_ecdf(geom = "step") +
  ggtitle("Empirische Verteilungsfunktion des Verkaufspreises") +
  xlab("Preis") +
  ylab("Kumulierte Häufigkeit") +
  scale_x_continuous(limits = c(0, max(data$price, na.rm = TRUE)))

# Empirische Verteilungsfunktion des Kilometerstandes
ggplot(data, aes(x = odometer)) +
  stat_ecdf(geom = "step") +
  ggtitle("Empirische Verteilungsfunktion des Kilometerstands") +
  xlab("Kilometerstand") +
  ylab("Kumulierte Häufigkeit") +
  scale_x_continuous(labels = scales::comma)

# 3. Wahrscheinlichkeitstheorie anwenden
# 3.1 Verteilungsanpassung
# Wir prüfen, ob der price einer theoretischen Normalverteilung folgt, indem
# wir einen QQ-Plot und zwei Goodness-of-Fit-Tests durchführen.

# Gemäß Fahrmeir et al. (2023) sind Goodness-of-fit-Tests statistische Verfahren, mit denen überprüft wird,
# ob die Verteilung eines interessierenden Merkmals ausreichend gut zu einer
# vorgegebenen theoretischen Verteilung passt, wie beispielsweise der Normalverteilung,
# indem sie die beobachtete Verteilung mit der erwarteten vergleichen.

# 3. Wahrscheinlichkeitstheorie anwenden
# 3.1 Verteilungsanpassung

# QQ-Plot for Normality Check
ggplot(data, aes(sample = price)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot für den Verkaufspreis") +
  xlab("Theoretische Quantile") +
  ylab("Empirische Quantile")

# Kolmogorov-Smirnov Test
cat("Kolmogorov-Smirnov-Test für Verkaufspreis:\n")
ks_test <- ks.test(data$price[1:1000], "pnorm", mean = mean(data$price), sd = sd(data$price))
print(ks_test)

# Anderson-Darling Test
ad_test <- nortest::ad.test(data$price)
print("Anderson-Darling-Test für Verkaufspreis:")
print(ad_test)


# 3.2 Wahrscheinlichkeitsberechnungen -----
# Wir berechnen die Wahrscheinlichkeiten für bestimmte Ereignisse,
# basierend auf der angenommenen Verteilung.
# Die angenommene Verteilung ist die Normalverteilung mit dem Mittelwert
# und der Standardabweichung der `price`-Variable.

# Berechnung von Mittelwert und Standardabweichung
mean_price <- mean(data$price, na.rm = TRUE)
std_dev_price <- sd(data$price, na.rm = TRUE)

# Wahrscheinlichkeit 1: Preis > 50.000 USD
prob_greater_50000 <- 1 - pnorm(50000, mean = mean_price, sd = std_dev_price)
cat("Wahrscheinlichkeit, dass der Preis > 50.000 USD ist:", prob_greater_50000, "\n")

# Wahrscheinlichkeit 2: Preis zwischen 20.000 und 40.000 USD
prob_between_20000_40000 <- pnorm(40000, mean = mean_price, sd = std_dev_price) - 
                            pnorm(20000, mean = mean_price, sd = std_dev_price)
cat("Wahrscheinlichkeit, dass der Preis zwischen 20.000 und 40.000 USD liegt:", prob_between_20000_40000, "\n")

# Wahrscheinlichkeit 3: Preis < 10.000 USD
prob_less_10000 <- pnorm(10000, mean = mean_price, sd = std_dev_price)
cat("Wahrscheinlichkeit, dass der Preis < 10.000 USD ist:", prob_less_10000, "\n")

# Empirischer Vergleich: Anteil der Beobachtungen mit Preis > 50.000 USD
empirical_greater_50000 <- mean(data$price > 50000)
cat("Empirischer Anteil von Preisen > 50.000 USD:", empirical_greater_50000, "\n")

# Vergleich der Wahrscheinlichkeiten
cat("Vergleich der Wahrscheinlichkeiten:\n")
cat("- Wahrscheinlichkeit basierend auf der Normalverteilung:", prob_greater_50000, "\n")
cat("- Empirische Wahrscheinlichkeit:", empirical_greater_50000, "\n")


# 4. Konfidenzintervalle
# Konfidenzintervall für den Mittelwert einer numerischen Variable (price)
# Funktion zur Berechnung des Konfidenzintervalls
mean_ci <- function(data, conf_level = 0.95) {
  mean_value <- mean(data, na.rm = TRUE)
  stderr <- sd(data, na.rm = TRUE) / sqrt(length(data))
  error_margin <- qnorm(1 - (1 - conf_level) / 2) * stderr
  lower_bound <- mean_value - error_margin
  upper_bound <- mean_value + error_margin
  return(c(lower_bound, upper_bound))
}

# Konfidenzintervall für den Mittelwert von `price`
price_ci <- mean_ci(data$price)
cat("95%-Konfidenzintervall für den Mittelwert des Verkaufspreises:", price_ci, "\n")

# Konfidenzintervall für den Anteil einer kategorialen Variable (condition)
# Berechnung des Anteils und des Konfidenzintervalls für die Kategorie "excellent" in `condition`
condition_counts <- table(data$condition)
prop_excellent <- condition_counts["excellent"] / sum(condition_counts)

# Funktion zur Berechnung des Konfidenzintervalls für einen Anteil
prop_ci <- function(prop, n, conf_level = 0.95) {
  error_margin <- qnorm(1 - (1 - conf_level) / 2) * sqrt((prop * (1 - prop)) / n)
  lower_bound <- prop - error_margin
  upper_bound <- prop + error_margin
  return(c(lower_bound, upper_bound))
}

# Konfidenzintervall für den Anteil von "excellent"
n_total <- sum(condition_counts)
excellent_ci <- prop_ci(prop_excellent, n_total)
cat("95%-Konfidenzintervall für den Anteil der Fahrzeuge mit Zustand 'excellent':", excellent_ci, "\n")

# Punkt-Schätzung
proportion_excellent <- mean(data$condition == "excellent")
proportion_excellent


# 5. Statistische Tests -----

# Hypothesenformulierung:
# Test 1: Überprüfung, ob der Mittelwert des Verkaufspreises (`price`) 20.000 USD beträgt.
# Nullhypothese (H0): Der Mittelwert des Verkaufspreises beträgt 20.000 USD.
# Alternativhypothese (H1): Der Mittelwert des Verkaufspreises ist ungleich 20.000 USD.

# Berechnung von Mittelwert und Standardabweichung
mean_price <- mean(data$price, na.rm = TRUE)
std_dev_price <- sd(data$price, na.rm = TRUE)
n <- nrow(data)

# Voraussetzungen prüfen: Normalverteilung der Daten
cat("Shapiro-Wilk-Test für Normalverteilung von `price`:\n")
shapiro_result <- shapiro.test(sample(data$price, 5000)) # Stichprobe von 5000 Werten
print(shapiro_result) # Ergebnis als Liste mit `print` ausgeben
if (shapiro_result$p.value > 0.05) {
  cat("Daten sind normalverteilt. Voraussetzungen erfüllt.\n")
} else {
  cat("Daten sind nicht normalverteilt. Vorsicht bei der Interpretation des Tests.\n")
}

# Durchführung des Ein-Stichproben-t-Tests
t_test_price <- t.test(data$price, mu = 20000, alternative = "two.sided")

# Ausgabe des Ergebnisses des Tests
cat("Ein-Stichproben-t-Test für den Mittelwert von `price`:\n")
print(t_test_price)  # Korrekte Ausgabe des gesamten Testergebnisses

# Interpretation des Ergebnisses
cat("p-Wert:", t_test_price$p.value, "\n")

if (t_test_price$p.value < 0.05) {
  cat("Das Ergebnis ist signifikant (p =", t_test_price$p.value, "). Wir lehnen die Nullhypothese ab.\n")
} else {
  cat("Das Ergebnis ist nicht signifikant (p =", t_test_price$p.value, "). Wir können die Nullhypothese nicht ablehnen.\n")
}

# Test 2: Überprüfung, ob der Anteil der Fahrzeuge im Zustand 'excellent' 25% beträgt.
# Nullhypothese (H0): Der Anteil der Fahrzeuge im Zustand 'excellent' beträgt 25%.
# Alternativhypothese (H1): Der Anteil der Fahrzeuge im Zustand 'excellent'
# ist ungleich 25%.

# Berechnung des Anteils
excellent_count <- sum(data$condition == "excellent", na.rm = TRUE)
proportion_excellent <- excellent_count / n

# Voraussetzungen prüfen: Binomialverteilung der Daten
cat("Proportion für 'excellent':", proportion_excellent, "\n")

# Durchführung eines z-Tests für Anteile
# Da die Stichprobe groß ist, können wir den z-Test verwenden.
z_stat <- (proportion_excellent - 0.25) / sqrt((0.25 * (1 - 0.25)) / n)
p_value_z <- 2 * (1 - pnorm(abs(z_stat))) # zweiseitig

cat("z-Test für den Anteil `excellent`:\n")
cat("z-Statistik:", z_stat, "\n")
cat("p-Wert:", p_value_z, "\n")

# Interpretation des Ergebnisses
if (p_value_z < 0.05) {
  cat("Das Ergebnis ist signifikant. Wir lehnen die Nullhypothese ab.\n")
  cat("p-Wert:", p_value_z, "\n")
} else {
  cat("Das Ergebnis ist nicht signifikant. Wir können die Nullhypothese nicht ablehnen.\n")
  cat("p-Wert:", p_value_z, "\n")
}

# 6. Zweistichprobentests -----
# Vergleich des Verkaufspreises zwischen Fahrzeugen im Zustand `excellent` und anderen Zuständen

# Gruppen erstellen
group1 <- data$price[data$condition == "excellent"]
group2 <- data$price[data$condition != "excellent"]

# Durchführung des Mann-Whitney-U-Tests
wilcox_test <- wilcox.test(group1, group2, exact = FALSE)

# Ergebnisse ausgeben
cat("Mann-Whitney-U-Test für den Verkaufspreis:\n")
cat("U-Wert:", wilcox_test$statistic, "\n")  # Ausgabe des U-Wertes
cat("p-Wert:", wilcox_test$p.value, "\n")

# Interpretation
if (wilcox_test$p.value < 0.05) {
  cat("Das Ergebnis ist signifikant. Der Verkaufspreis unterscheidet sich zwischen den Gruppen.\n")
} else {
  cat("Das Ergebnis ist nicht signifikant. Kein Unterschied im Verkaufspreis zwischen den Gruppen.\n")
}

# 7. Tests für Zusammenhangsmaße -----

# Pearson- und Spearman-Korrelation -----
# Variablenwahl: Preis und Kilometerstand
num_var1 <- data$price
num_var2 <- data$odometer

# Pearson-Korrelation (linearer Zusammenhang)
pearson_corr <- cor.test(num_var1, num_var2, method = "pearson")
cat("Pearson-Korrelation:\n")
cat("Korrelationskoeffizient:", pearson_corr$estimate, "\n")
cat("p-Wert:", pearson_corr$p.value, "\n")

# Spearman-Korrelation (monotoner Zusammenhang)
spearman_corr <- cor.test(num_var1, num_var2, method = "spearman")
cat("Spearman-Korrelation:\n")
cat("Korrelationskoeffizient:", spearman_corr$estimate, "\n")
cat("p-Wert:", spearman_corr$p.value, "\n")

# Visualisierung der Korrelation: Streudiagramm
ggplot(data, aes(x = odometer, y = price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggtitle("Streudiagramm: Kilometerstand vs. Preis") +
  xlab("Kilometerstand") +
  ylab("Preis") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Chi-Quadrat-Test -----
# Variablenwahl: Zustand und Antriebsart
cat_var1 <- data$condition
cat_var2 <- data$drive

# Kreuztabelle erstellen
contingency_table <- table(cat_var1, cat_var2)

# Chi-Quadrat-Test
chi_test <- chisq.test(contingency_table)
cat("Chi-Quadrat-Test:\n")
cat("Chi-Quadrat-Wert:", chi_test$statistic, "\n")
cat("p-Wert:", chi_test$p.value, "\n")

# Visualisierung der kategorialen Beziehung: Mosaikplot
mosaicplot(contingency_table, main = "Mosaikplot: Zustand vs. Antriebsart", color = TRUE)


# 8. Lineare Regression -----

# Nach Wollschläger (2020) misst die Korrelation zwischen zwei quantitativen
# Variablen die Stärke ihres linearen Zusammenhangs. Ähnlich untersucht die
# lineare Regression den linearen Zusammenhang zwischen Variablen, um die Werte einer
# Zielvariable (auch Kriterium genannt) auf Basis der Werte anderer Variablen (Prädiktoren,
# Kovariaten oder Kovariablen) vorherzusagen.

# Einfache lineare Regression
simple_model <- lm(price ~ odometer, data = data)
cat("Einfache lineare Regression: Verkaufspreis ~ Kilometerstand\n")
summary(simple_model)

# Multiple lineare Regression - Modell 1 (Kilometerstand + Baujahr)
multiple_model_1 <- lm(price ~ odometer + year, data = data)
cat("\nMultiple lineare Regression 1: Verkaufspreis ~ Kilometerstand + Baujahr\n")
summary(multiple_model_1)

# Multiple lineare Regression - Modell 2 (Kilometerstand + Baujahr + Zustand)
multiple_model_2 <- lm(price ~ odometer + year + condition, data = data)
cat("\nMultiple lineare Regression 2: Verkaufspreis ~ Kilometerstand + Baujahr + Zustand\n")
summary(multiple_model_2)

# Modellbewertung - Ausgabe von R² für alle Modelle
cat("\nModellbewertung:\n")
cat("Einfache Regression R-Quadrat:", summary(simple_model)$r.squared, "\n")
cat("Multiple Regression 1 R-Quadrat:", summary(multiple_model_1)$r.squared, "\n")
cat("Multiple Regression 2 R-Quadrat:", summary(multiple_model_2)$r.squared, "\n")

# F-Test - Optimierte Berechnung und Ausgabe von F-Wert und p-Wert für alle Modelle
cat("\nF-Test Ergebnisse:\n")

# Funktion zur Berechnung und Ausgabe von F-Wert und p-Wert
output_f_test <- function(model, model_name) {
  f_stat <- summary(model)$fstatistic
  if (!is.null(f_stat)) { # Überprüfen, ob F-Statistik vorhanden ist
    f_value <- f_stat[1]
    df1 <- f_stat[2]
    df2 <- f_stat[3]
    p_value <- pf(f_value, df1, df2, lower.tail = FALSE)
    cat(model_name, ": F-Wert:", f_value, ", p-Wert:", p_value, "\n")
  } else {
    cat(model_name, ": Keine F-Statistik verfügbar.\n")
  }
}

# Einfache Regression
output_f_test(simple_model, "F-Test einfache Regression")

# Multiple Regression 1
output_f_test(multiple_model_1, "F-Test multiple Regression 1")

# Multiple Regression 2
output_f_test(multiple_model_2, "F-Test multiple Regression 2")


# Residuenanalyse
cat("\nResiduenanalyse:\n")

# Linearität prüfen: Residuen vs. vorhergesagte Werte
par(mfrow = c(2, 2)) # Grafiken in einem 2x2-Layout
plot(simple_model, which = 1, main = "Residuen vs. Vorhergesagte Werte: Einfaches Modell")
plot(multiple_model_2, which = 1, main = "Residuen vs. Vorhergesagte Werte: Multiples Modell")

# Normalverteilung der Residuen prüfen: Histogramm und QQ-Plot
hist(simple_model$residuals, main = "Histogramm der Residuen: Einfaches Modell", xlab = "Residuen")
qqnorm(simple_model$residuals, main = "QQ-Plot der Residuen: Einfaches Modell")
qqline(simple_model$residuals)

# Für multiple Regression
hist(multiple_model_2$residuals, main = "Histogramm der Residuen: Multiples Modell", xlab = "Residuen")
qqnorm(multiple_model_2$residuals, main = "QQ-Plot der Residuen: Multiples Modell")
qqline(multiple_model_2$residuals)

# Shapiro-Wilk-Test für Normalverteilung der Residuen
cat("Shapiro-Wilk-Test einfache Regression:\n")

# Zufällige Stichprobe der Residuen ziehen
simple_model_residuals_sample <- sample(simple_model$residuals, size = min(length(simple_model$residuals), 5000))
print(shapiro.test(simple_model_residuals_sample))

cat("Shapiro-Wilk-Test multiple Regression:\n")
# Zufällige Stichprobe der Residuen ziehen
multiple_model_residuals_sample <- sample(multiple_model_2$residuals, size = min(length(multiple_model_2$residuals), 5000))
print(shapiro.test(multiple_model_residuals_sample))