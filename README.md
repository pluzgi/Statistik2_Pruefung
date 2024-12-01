Analyse des Automobilverkaufsdatensatzes

Überblick
Dieser Code führt eine umfassende Analyse eines Datensatzes durch, der Automobilverkäufe in den USA enthält. Die Analyse umfasst Schritte von der Datenbereinigung über explorative Datenanalyse bis hin zu fortgeschrittenen statistischen Tests und Modellierungen. Ziel ist es, Faktoren zu identifizieren, die den Fahrzeugpreis beeinflussen, sowie fundierte Einsichten aus den Daten zu gewinnen.

Struktur des Codes
1. Einlesen der Pakete und Daten
Pakete: Der Code lädt die erforderlichen R-Pakete für die Datenanalyse über die Datei functions/packages.R.

Datensatz: Der Datensatz cleaned_vehicle_dataset.csv wird eingelesen.

Datenbeschreibung:
Enthält 265.640 Beobachtungen und 22 Variablen.
Beinhaltet numerische und kategoriale Variablen (z. B. Preis, Kilometerstand, Zustand, Antriebsart).

2. Explorative Datenanalyse
2.1 Deskriptive Statistik
Numerische Variablen: Es werden zentrale Tendenzen (Mittelwert, Median) und Streuungsmaße (Standardabweichung, Minimum, Maximum) berechnet.
Wichtige Variablen: price (Verkaufspreis), odometer (Kilometerstand), year (Baujahr), lat (Geografische Breite).
Kategoriale Variablen: Häufigkeiten werden für Variablen wie condition (Zustand), manufacturer (Hersteller), drive (Antriebsart) und fuel (Kraftstoffart) berechnet.
2.2 Grafische Darstellung
Histogramme:
Verteilung von Verkaufspreis und Kilometerstand.
Boxplots:
Vergleich des Preises nach Zustand und Antriebsart.
Streudiagramme:
Zusammenhang zwischen Kilometerstand/Baujahr und Preis.
Empirische Verteilungsfunktionen (ECDF):
Kumulierte Verteilung von Preis und Kilometerstand.

3. Wahrscheinlichkeitstheorie
Verteilungsanpassung:
QQ-Plots und Goodness-of-Fit-Tests (Kolmogorov-Smirnov und Anderson-Darling) prüfen, ob der Preis einer Normalverteilung folgt.
Wahrscheinlichkeitsberechnungen:
Wahrscheinlichkeit, dass der Preis bestimmte Werte über- oder unterschreitet.


README: Analyse des Automobilverkaufsdatensatzes
Überblick
Dieser Code führt eine umfassende Analyse eines Datensatzes durch, der Automobilverkäufe in den USA enthält. Die Analyse umfasst Schritte von der Datenbereinigung über explorative Datenanalyse bis hin zu fortgeschrittenen statistischen Tests und Modellierungen. Ziel ist es, Faktoren zu identifizieren, die den Fahrzeugpreis beeinflussen, sowie fundierte Einsichten aus den Daten zu gewinnen.

Struktur des Codes
1. Einlesen der Pakete und Daten
Pakete: Der Code lädt die erforderlichen R-Pakete für die Datenanalyse über die Datei functions/packages.R.
Datensatz: Der Datensatz cleaned_vehicle_dataset.csv wird eingelesen.
Datenbeschreibung:
Enthält 265.640 Beobachtungen und 22 Variablen.
Beinhaltet numerische und kategoriale Variablen (z. B. Preis, Kilometerstand, Zustand, Antriebsart).

2. Explorative Datenanalyse
2.1 Deskriptive Statistik
Numerische Variablen: Es werden zentrale Tendenzen (Mittelwert, Median) und Streuungsmaße (Standardabweichung, Minimum, Maximum) berechnet.
Wichtige Variablen: price (Verkaufspreis), odometer (Kilometerstand), year (Baujahr), lat (Geografische Breite).
Kategoriale Variablen: Häufigkeiten werden für Variablen wie condition (Zustand), manufacturer (Hersteller), drive (Antriebsart) und fuel (Kraftstoffart) berechnet.
2.2 Grafische Darstellung
Histogramme:
Verteilung von Verkaufspreis und Kilometerstand.
Boxplots:
Vergleich des Preises nach Zustand und Antriebsart.
Streudiagramme:
Zusammenhang zwischen Kilometerstand/Baujahr und Preis.
Empirische Verteilungsfunktionen (ECDF):
Kumulierte Verteilung von Preis und Kilometerstand.

3. Wahrscheinlichkeitstheorie
Verteilungsanpassung:
QQ-Plots und Goodness-of-Fit-Tests (Kolmogorov-Smirnov und Anderson-Darling) prüfen, ob der Preis einer Normalverteilung folgt.
Wahrscheinlichkeitsberechnungen:
Wahrscheinlichkeit, dass der Preis bestimmte Werte über- oder unterschreitet.

4. Konfidenzintervalle
Berechnung von 95%-Konfidenzintervallen:
Für den Mittelwert des Preises.
Für den Anteil der Fahrzeuge im Zustand excellent.

5. Statistische Tests
Ein-Stichproben-t-Test:
Überprüfung, ob der mittlere Verkaufspreis 20.000 USD beträgt.
z-Test für Anteile:
Test, ob der Anteil der Fahrzeuge im Zustand excellent 25 % beträgt.

6. Zweistichprobentests
Mann-Whitney-U-Test:
Vergleich des Verkaufspreises zwischen Fahrzeugen im Zustand excellent und allen anderen Zuständen.


README: Analyse des Automobilverkaufsdatensatzes
Überblick
Dieser Code führt eine umfassende Analyse eines Datensatzes durch, der Automobilverkäufe in den USA enthält. Die Analyse umfasst Schritte von der Datenbereinigung über explorative Datenanalyse bis hin zu fortgeschrittenen statistischen Tests und Modellierungen. Ziel ist es, Faktoren zu identifizieren, die den Fahrzeugpreis beeinflussen, sowie fundierte Einsichten aus den Daten zu gewinnen.

Struktur des Codes
1. Einlesen der Pakete und Daten
Pakete: Der Code lädt die erforderlichen R-Pakete für die Datenanalyse über die Datei functions/packages.R.
Datensatz: Der Datensatz cleaned_vehicle_dataset.csv wird eingelesen.
Datenbeschreibung:
Enthält 265.640 Beobachtungen und 22 Variablen.
Beinhaltet numerische und kategoriale Variablen (z. B. Preis, Kilometerstand, Zustand, Antriebsart).
2. Explorative Datenanalyse
2.1 Deskriptive Statistik
Numerische Variablen: Es werden zentrale Tendenzen (Mittelwert, Median) und Streuungsmaße (Standardabweichung, Minimum, Maximum) berechnet.
Wichtige Variablen: price (Verkaufspreis), odometer (Kilometerstand), year (Baujahr), lat (Geografische Breite).
Kategoriale Variablen: Häufigkeiten werden für Variablen wie condition (Zustand), manufacturer (Hersteller), drive (Antriebsart) und fuel (Kraftstoffart) berechnet.
2.2 Grafische Darstellung
Histogramme:
Verteilung von Verkaufspreis und Kilometerstand.
Boxplots:
Vergleich des Preises nach Zustand und Antriebsart.
Streudiagramme:
Zusammenhang zwischen Kilometerstand/Baujahr und Preis.
Empirische Verteilungsfunktionen (ECDF):
Kumulierte Verteilung von Preis und Kilometerstand.

3. Wahrscheinlichkeitstheorie
Verteilungsanpassung:
QQ-Plots und Goodness-of-Fit-Tests (Kolmogorov-Smirnov und Anderson-Darling) prüfen, ob der Preis einer Normalverteilung folgt.
Wahrscheinlichkeitsberechnungen:
Wahrscheinlichkeit, dass der Preis bestimmte Werte über- oder unterschreitet.

4. Konfidenzintervalle
Berechnung von 95%-Konfidenzintervallen:
Für den Mittelwert des Preises.
Für den Anteil der Fahrzeuge im Zustand excellent.

5. Statistische Tests
Ein-Stichproben-t-Test:
Überprüfung, ob der mittlere Verkaufspreis 20.000 USD beträgt.
z-Test für Anteile:
Test, ob der Anteil der Fahrzeuge im Zustand excellent 25 % beträgt.

6. Zweistichprobentests
Mann-Whitney-U-Test:
Vergleich des Verkaufspreises zwischen Fahrzeugen im Zustand excellent und allen anderen Zuständen.

7. Zusammenhangsmaße
Korrelationen:
Pearson- und Spearman-Korrelation analysieren die Beziehung zwischen Preis und Kilometerstand.
Chi-Quadrat-Test:
Überprüfung des Zusammenhangs zwischen Zustand und Antriebsart.

8. Lineare Regression
8.1 Einfache lineare Regression
Ziel: Analyse des Einflusses des Kilometerstands auf den Preis.
Ergebnis: Signifikanter negativer Einfluss des Kilometerstands auf den Preis.
8.2 Multiple lineare Regression
Zwei Modelle:
Modell 1: Preis ~ Kilometerstand + Baujahr.
Modell 2: Preis ~ Kilometerstand + Baujahr + Zustand.
Ergebnis: Die Hinzunahme der Variablen Zustand erhöht die Erklärungskraft des Modells geringfügig.

9. Residuenanalyse
Visualisierung:
Residuen vs. vorhergesagte Werte, Histogramme und QQ-Plots.
Normalitätstest:
Shapiro-Wilk-Test prüft die Normalverteilung der Residuen.

Voraussetzungen
Installierte R-Pakete (ggplot2, dplyr, nortest etc.).
Zugriff auf den Datensatz cleaned_vehicle_dataset.csv.