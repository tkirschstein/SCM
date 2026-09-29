# Supply Chain Management – Kursmaterialien

**Hochschule RheinMain | Master International Management | 1. Fachsemester | Wintersemester 2026/27**

Prof. Dr. Thomas Kirschstein

---

## Kursüberblick

Dieses Repository enthält alle Materialien zur Lehrveranstaltung *Supply Chain Management* im Masterstudiengang International Management: Vorlesungsfolien, den begleitenden Reader, sechs Fallstudien und die zugehörige R-Funktionsbibliothek.

Die Veranstaltung vermittelt die strategischen und quantitativen Grundlagen des Supply Chain Managements. Im Mittelpunkt steht die Übertragung von Methoden aus der Vorlesung auf möglichst realistische Problemstellungen. Das geschieht in Fallstudien, die in Gruppen bearbeitet, im Plenum präsentiert und gemeinsam reflektiert werden.

| | |
|---|---|
| **Studiengang** | Master International Management, 1. Fachsemester |
| **Umfang** | 2 SWS (seminaristische Vorlesung) |
| **Termine** | donnerstags, 29.10.2026 – 28.01.2027 (12 Termine, keine Veranstaltung am 24.12. und 31.12.) |
| **Sprache** | Deutsch (Fachliteratur überwiegend Englisch) |
| **Lehrformat** | Inverted Classroom: Vorbereitung mit dem Reader, in der Veranstaltung kurzer Input, Fallstudienpräsentationen und gemeinsame Reflexion |
| **Prüfungsleistung** | Individueller Reflexionsbericht (siehe unten), keine Klausur |
| **Voraussetzungen** | Grundkenntnisse in Statistik; Programmierkenntnisse sind hilfreich, aber nicht erforderlich (R-Startercode wird bereitgestellt) |

**Lernziele:** Nach der Veranstaltung können die Studierenden

- Supply Chains als Netzwerke beschreiben und ihre strategische Ausrichtung (Strategic Fit) beurteilen,
- Zielkonflikte zwischen Kosten, Service und Nachhaltigkeit erkennen und bewerten,
- Standortentscheidungen mit qualitativen (Nutzwertanalyse, AHP), kontinuierlichen (Steiner-Weber) und diskreten Modellen (WLP) vorbereiten,
- Bestandsentscheidungen unter Unsicherheit (Pooling, Newsvendor) treffen,
- die Ursachen des Bullwhip-Effekts erklären und Gegenmaßnahmen bewerten sowie
- die Aussagekraft und Grenzen quantitativer Modelle für reale Managemententscheidungen kritisch reflektieren.

---

## Ablaufplan

Jede Veranstaltung umfasst 90 Minuten. Es werden jeweils 2 Themen behandelt. Das jeweilige Kapitel ist **vor** dem Termin zu studieren (Reader & Folien). Die Kapitel werden in den Veranstaltungen **nicht** vollständig präsentiert, sondern nur Rückfragen disktiert und ausgewählte Inhalte vertieft bzw. wiederholt. 

| Nr. | Datum | Teil 1  | Teil 2 |
|:--:|---|---|---|
| 1 | Do, 29.10.2026 | Kick-off und Organisation; Einführung und Grundbegriffe des SCM (Kap. 1) | Ausgabe aller Fallstudien & Beergame (1) |
| 2 | Do, 05.11.2026 | Wertschöpfung, strategische Fertigung und Strategic Fit (Kap. 2) | Kick-off-Gespräche mit den Gruppen  |
| 3 | Do, 12.11.2026 | Ziele, Zielkonflikte und Logistikkosten (Kap. 3) | Konsultation |
| 4 | Do, 19.11.2026 | Standortplanung I: Nutzwertanalyse, AHP, Steiner-Weber (Kap. 4) | Konsultation  |
| 5 | Do, 26.11.2026 | Standortplanung II: diskrete Netzwerkplanung, WLP (Kap. 5) |Konsultation |
| 6 | Do, 03.12.2026 | Unsicherheit in Supply Chains: Pooling (Kap. 6) |Konsultation  |
| 7 | Do, 10.12.2026 | Das Newsvendor-Modell (Kap. 7) | **FS 1: Strategische Gestaltung – Werner & Mertz / Frosch** |
| 8 | Do, 17.12.2026 | Der Bullwhip-Effekt (Kap. 8) und **Beer Game** |  Beergame (2)|
| – | Do, 24.12.2026 | *keine Veranstaltung* | |
| – | Do, 31.12.2026 | *keine Veranstaltung* | |
| 9 | Do, 07.01.2027 | **FS 2: Distributionshub LATAM – AHP und Steiner-Weber** | **FS 3: Batterielogistik – WLP** |
| 10 | Do, 14.01.2027 | Zwischenfeedback Case Studies 1-3  | Konsultationen  |
| 11 | Do, 21.01.2027 | **FS 4: Risk Pooling im E-Commerce (Olist-Daten)** | **FS 5: Newsvendor – französische Bäckerei** |
| 12 | Do, 28.01.2027 | **FS 6: Bullwhip-Effekt – MTIS-Daten und Simulation** | (Beergame (3)) |

Die Fallstudien werden jeweils zwei bis drei Wochen nach dem zugehörigen Input präsentiert. Die Gruppen mit späteren Präsentationsterminen beginnen mit Datenbeschaffung und -aufbereitung bereits vor dem Input. Die Weihnachtspause verschafft den Gruppen 2–6 zusätzliche Bearbeitungszeit. Das Beer Game wird dreimal während der Veranstaltung gespielt und liefert ergänzende Daten für Fallstudie 6.

---

## Fallstudien

Die sechs Fallstudien werden in Gruppen von **3–6 Personen** bearbeitet; jede Gruppe übernimmt eine Fallstudie. Bis auf Fallstudie 1 arbeiten alle mit realen oder realitätsnahen Datensätzen.

| Nr. | Thema | Methoden | Daten |
|:--:|---|---|---|
| 1 | Strategische Gestaltung von SCs: Werner & Mertz / Frosch | Strategic Fit, SC-Treiber, Zielkonflikte, Closed-Loop-SC | Recherche (qualitativ) |
| 2 | Distributionshub für Lateinamerika | AHP, Nutzwertanalyse, Steiner-Weber, Haversine | Prognose- und Kostendaten (fiktiv), Weltbank-Indikatoren (LPI, WGI) |
| 3 | Batterielogistik Norddeutschland/Benelux | UFLP/CFLP als MILP, Add/Drop, Transportproblem, Szenarien | reale Werksstandorte, Mengen und Kosten realitätsnah geschätzt |
| 4 | Risk Pooling im E-Commerce: Zentral- oder Regionallager in Brasilien | Sicherheitsbestand, Quadratwurzelgesetz, Korrelation, Produkt-Pooling | Bestelldaten des Marktplatzes Olist (Kaggle) |
| 5 | Newsvendor in einer französischen Bäckerei | Newsvendor (normal/empirisch), Backtest, zensierte Nachfrage | Kassendaten *French bakery daily sales* (Kaggle) |
| 6 | Bullwhip-Effekt | Varianzverhältnis, Simulation, Chen-Schranke, Beer Game | U.S. Census MTIS über FRED |

**Ablauf je Gruppe:**

1. Kick-off-Gespräch & Konsultationen zu den Veranstaltungsterminen,
2. Zwischenstand etwa eine Woche vor der Präsentation,
3. Präsentation (30 Min.) mit Beantwortung der Reflexionsfragen und Diskussion (15 Min.),
4. Abgabe von Foliensatz (PDF) und – bei den datenbasierten Fallstudien – reproduzierbarem Quarto-/R-Code bis 24 Stunden vor dem Termin.

Jede Fallstudie endet mit **Reflexionsfragen**, die die Gruppe in der Präsentation beantwortet und die die anschließende Diskussion eröffnen. Die Diskussion wird von einer anderen Gruppe moderiert.

---

## Prüfungsleistung: Reflexionsbericht

Die Prüfungsleistung ist ein **individueller Reflexionsbericht**. Die Fallstudienpräsentationen und die anschließenden Diskussionen sind seine Grundlage. Die **aktive Teilnahme an den Diskussionen** ist Teil der Reflexion und geht in die **Benotung ein**. Insgesamt umfasst die Reflektion:

- **Reflexionsjournal (begleitend):** Zu jeder Fallstudienpräsentation halten alle Teilnehmenden etwa eine Seite fest: Kernaussage, Annahmen und Grenzen der Methode, Bezug zur eigenen Erfahrung bzw. zu einem bekannten Unternehmen, offene Fragen.
- **Reflexionsbericht:** Er umfasst zwei Teile:
  1. Präsentation der eigenen Fallstudie,
  2. Zusammenfassung der Reflektionsfragen & Diskussionsergbnisse,
- Umfang, Abgabetermin und Bewertungskriterien werden in der ersten Veranstaltung bekannt gegeben.

---

## Direkter Zugriff auf die Ressourcen

- Slides: https://tkirschstein.github.io/SCM/slides/scm-komplett.html
- Reader: https://tkirschstein.github.io/SCM/book/index
- Case-Studies: 
  - https://tkirschstein.github.io/SCM/case_studies/case_study_01_strategie_frosch
  - https://tkirschstein.github.io/SCM/case_studies/case_study_02_standort_ahp_steiner_weber
  - https://tkirschstein.github.io/SCM/case_studies/case_study_03_wlp_batterielogistik
  - https://tkirschstein.github.io/SCM/case_studies/case_study_04_pooling_einzelhandel
  - https://tkirschstein.github.io/SCM/case_studies/case_study_05_newsvendor_baeckerei
  - https://tkirschstein.github.io/SCM/case_studies/case_study_06_bullwhip

---

## Beer Game

Das **Beer Distribution Game** (Sterman 1989) simuliert den Bullwhip-Effekt in einer vierstufigen Supply Chain (Einzelhandel → Großhandel → Distributor → Hersteller). Wir spielen das Spiel mehrfach in der Veranstaltung in der Regel mit folgendem Ablauf:

1. Einführung in die Regeln/Annahmen (variieren während des Semesters),
2. Spielrunden/Simulation,
3. Auswertung der Bestell- und Bestandsverläufe und Diskussion.

Online-Plattform: [Transentis](https://beergame.transentis.com/de). 

Machen Sie sich gern vorab mit den Regeln und Ablauf vertraut.

---


## Aufbau des Repositories

```
SCM/
├── slides/                  # Vorlesungsfolien (Quarto revealjs) → docs/slides
│   ├── lectures/            # 8 Vorlesungseinheiten (01–08)
│   ├── scm-komplett.qmd     # Gesamtausgabe aller Einheiten
│   └── _quarto.yml
├── book/                    # Reader (Quarto Book) → docs/book
│   ├── chapters/            # Kapitel 1–9
│   ├── index.qmd
│   └── _quarto.yml
├── case_studies/            # 6 Fallstudien → docs/case_studies
│   └── _quarto.yml
├── R/
│   └── scm_functions.R      # R-Funktionsbibliothek des Kurses (roxygen2-dokumentiert)
└── docs/                    # gerenderte Ausgabe (HTML)
```

### Rendern

```bash
# Reader
cd book && quarto render

# Vorlesungsfolien (einzeln oder als Gesamtausgabe, siehe slides/README.md)
cd slides && quarto render lectures/01_einfuehrung-und-grundbegriffe-des-supply-chain-managements.qmd
cd slides && quarto render --profile full

# Fallstudien
cd case_studies && quarto render
```

Die Ausgabe landet jeweils im Ordner `docs/`. Das Arbeitsverzeichnis für R-Code ist der jeweilige Projektordner, die Funktionsbibliothek wird daher mit `source("../R/scm_functions.R")` eingebunden.

---

## Software

- **R** ≥ 4.3 mit **RStudio** oder Positron
- **Quarto** ≥ 1.4 (<https://quarto.org>)

```r
install.packages(c(
  "tidyverse",        # Datenaufbereitung und Grafiken
  "lubridate",        # Datumsfunktionen (Fallstudie 5)
  "zoo",              # gleitende Durchschnitte (Fallstudie 4)
  "knitr", "kableExtra",
  "plotly",           # interaktive Grafiken im Reader
  "ompr", "ompr.roi", "ROI", "ROI.plugin.glpk",  # MILP (Fallstudie 3)
  "lpSolve",          # Transportproblem
  "MASS"
))
```

Für die Fallstudien 4 und 5 ist ein (kostenloses) **Kaggle-Konto** erforderlich, um die Datensätze herunterzuladen.

---

## Literatur

| Titel | Autoren | Relevanz |
|---|---|---|
| *Supply Chain Management: Strategy, Planning, and Operation* | Chopra & Meindl | alle Themen |
| *Matching Supply with Demand* | Cachon & Terwiesch | Pooling, Newsvendor, Bullwhip |
| *Sustainable Logistics and Supply Chain Management* | Grant et al. | Nachhaltigkeit |

Weitere Quellen sind im Reader und in den Fallstudien angegeben (`literature/!references.bib`).

---

## Kontakt

Prof. Dr. Thomas Kirschstein – thomas.kirschstein@hs-rm.de

Fehler und Verbesserungsvorschläge bitte als GitHub-Issue melden oder an thomas.kirschstein@hs-rm.de.
