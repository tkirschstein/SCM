# Global Supply Chain Management – Course Materials

**Hochschule RheinMain | Master International Management | 1st semester | Winter semester 2026/27**

Prof. Dr. Matthias Kalverkamp & Prof. Dr. Thomas Kirschstein

---

## Course overview

This repository contains all materials for the course *Supply Chain Management* in the Master's programme International Management: lecture slides, the companion course reader, six case studies and the accompanying R function library.

The course covers the strategic and quantitative foundations of supply chain management. Its focus is on transferring the methods from the lecture to problems that are as realistic as possible. This happens in case studies that are worked on in groups, presented in class and reflected on together.

| | |
|---|---|
| **Programme** | Master International Management, 1st semester |
| **Scope** | 2 SWS (weekly contact hours; seminar-style lecture) |
| **Sessions** | Thursdays, 29 Oct 2026 – 28 Jan 2027 (12 sessions; no class on 24 Dec and 31 Dec) |
| **Language** | English |
| **Teaching format** | Inverted classroom: preparation with the reader and slides; in class, questions, selected in-depth topics, case study presentations and joint reflection |
| **Assessment** | Individual reflection report (see below); no written exam |
| **Prerequisites** | Basic statistics; programming skills are helpful but not required (R starter code is provided) |

**Learning objectives:** After the course, students are able to

- describe supply chains as networks and assess their strategic orientation (strategic fit),
- identify and evaluate trade-offs between cost, service and sustainability,
- prepare location decisions with qualitative (scoring model, AHP), continuous (Steiner–Weber) and discrete models (WLP),
- make inventory decisions under uncertainty (risk pooling, newsvendor),
- explain the causes of the bullwhip effect and evaluate countermeasures, and
- critically reflect on the explanatory power and limitations of quantitative models for real management decisions.

---

## Schedule

Each session lasts 90 minutes and covers two topics. The respective chapter must be studied **before** the session (reader & slides). The chapters are **not** presented in full in class; instead, we discuss questions and deepen or revisit selected content.

| No. | Date | Part 1 | Part 2 |
|:--:|---|---|---|
| 1 | Thu, 29 Oct 2026 | Kick-off and organisation; introduction and basic concepts of SCM (ch. 1) | Hand-out of all case studies & Beer Game (1) |
| 2 | Thu, 5 Nov 2026 | Value creation, strategic manufacturing and strategic fit (ch. 2) | Kick-off meetings with the groups |
| 3 | Thu, 12 Nov 2026 | Objectives, trade-offs and logistics costs (ch. 3) | Consultation |
| 4 | Thu, 19 Nov 2026 | Facility location planning I: scoring model, AHP, Steiner–Weber (ch. 4) | Consultation |
| 5 | Thu, 26 Nov 2026 | Facility location planning II: discrete network planning, WLP (ch. 5) | Consultation |
| 6 | Thu, 3 Dec 2026 | Uncertainty in supply chains: risk pooling (ch. 6) | Consultation |
| 7 | Thu, 10 Dec 2026 | The newsvendor model (ch. 7) | **CS 1: Strategic design – Werner & Mertz / Frosch** |
| 8 | Thu, 17 Dec 2026 | The bullwhip effect (ch. 8) and **Beer Game** | Beer Game (2) |
| – | Thu, 24 Dec 2026 | *no class* | |
| – | Thu, 31 Dec 2026 | *no class* | |
| 9 | Thu, 7 Jan 2027 | **CS 2: Distribution hub LATAM – AHP and Steiner–Weber** | **CS 3: Battery logistics – WLP** |
| 10 | Thu, 14 Jan 2027 | Interim feedback on case studies 1–3 | Consultations |
| 11 | Thu, 21 Jan 2027 | **CS 4: Risk pooling in e-commerce (Olist data)** | **CS 5: Newsvendor – French bakery** |
| 12 | Thu, 28 Jan 2027 | **CS 6: Bullwhip effect – MTIS data and simulation** | (Beer Game (3)) |

Each case study is presented two to three weeks after the corresponding input. Groups with later presentation dates start collecting and preparing their data before the input. The Christmas break gives groups 2–6 additional working time. The Beer Game is played three times during the course and provides additional data for case study 6.

---

## Case studies

The six case studies are worked on in groups of **3–6 students**; each group takes on one case study. Except for case study 1, all of them work with real or realistic datasets.

| No. | Topic | Methods | Data |
|:--:|---|---|---|
| 1 | Strategic supply chain design: Werner & Mertz / Frosch | Strategic fit, supply chain drivers, trade-offs, closed-loop supply chain | Desk research (qualitative) |
| 2 | Distribution hub for Latin America | AHP, scoring model, Steiner–Weber, haversine distance | Forecast and cost data (fictitious), World Bank indicators (LPI, WGI) |
| 3 | Battery logistics in Northern Germany/Benelux | UFLP/CFLP as MILP, add/drop heuristics, transportation problem, scenarios | Real plant locations, realistically estimated volumes and costs |
| 4 | Risk pooling in e-commerce: central or regional warehouses in Brazil | Safety stock, square-root law, correlation, product pooling | Order data of the Olist marketplace (Kaggle) |
| 5 | Newsvendor in a French bakery | Newsvendor (normal/empirical), backtest, censored demand | Point-of-sale data *French bakery daily sales* (Kaggle) |
| 6 | Bullwhip effect | Variance ratio, simulation, Chen bound, Beer Game | U.S. Census MTIS via FRED |

**Process for each group:**

1. Kick-off meeting & consultations during class sessions,
2. interim status about one week before the presentation,
3. presentation (30 min) including answers to the reflection questions, followed by discussion (15 min),
4. submission of the slide deck (PDF) and – for the data-based case studies – reproducible Quarto/R code no later than 24 hours before the session.

Each case study ends with **reflection questions**, which the group answers in its presentation and which open the subsequent discussion. The discussion is moderated by another group.

---

## Assessment: reflection report

The assessment is an **individual reflection report**. It is based on the case study presentations and the subsequent discussions. **Active participation in the discussions** is part of the reflection and **counts towards the grade**. Overall, the reflection comprises:

- **Reflection journal (ongoing):** for each case study presentation, all participants write about one page: key message, assumptions and limitations of the method, links to their own experience or to a company they know, open questions.
- **Reflection report:** it consists of two parts:
  1. presentation of the student's own case study,
  2. summary of the reflection questions & discussion results.
- Length, submission deadline and assessment criteria will be announced in the first session.

---

## Direct access to the resources

- Navigation: https://tkirschstein.github.io/SCM/
- Slides: https://tkirschstein.github.io/SCM/slides/scm-komplett.html
- Reader: https://tkirschstein.github.io/SCM/book/index
- Case studies:
  - https://tkirschstein.github.io/SCM/case_studies/case_study_01_strategie_frosch
  - https://tkirschstein.github.io/SCM/case_studies/case_study_02_standort_ahp_steiner_weber
  - https://tkirschstein.github.io/SCM/case_studies/case_study_03_wlp_batterielogistik
  - https://tkirschstein.github.io/SCM/case_studies/case_study_04_pooling_einzelhandel
  - https://tkirschstein.github.io/SCM/case_studies/case_study_05_newsvendor_baeckerei
  - https://tkirschstein.github.io/SCM/case_studies/case_study_06_bullwhip

---

## Beer Game

The **Beer Distribution Game** (Sterman 1989) simulates the bullwhip effect in a four-stage supply chain (retailer → wholesaler → distributor → manufacturer). We play the game several times during the course, usually as follows:

1. introduction to the rules/assumptions (these vary during the semester),
2. game rounds/simulation,
3. analysis of the order and inventory patterns and discussion.

Online platform: [Transentis](https://beergame.transentis.com/).

Feel free to familiarise yourself with the rules and procedure in advance.

---


## Repository structure

```
SCM/
├── slides/                  # Lecture slides (Quarto revealjs) → docs/slides
│   ├── lectures/            # 8 lecture units (01–08)
│   ├── scm-komplett.qmd     # Complete edition of all units
│   └── _quarto.yml
├── book/                    # Course reader (Quarto book) → docs/book
│   ├── chapters/            # Chapters 1–9
│   ├── index.qmd
│   └── _quarto.yml
├── case_studies/            # 6 case studies → docs/case_studies
│   └── _quarto.yml
├── R/
│   └── scm_functions.R      # Course R function library (roxygen2-documented)
└── docs/                    # Rendered output (HTML)
```

### Rendering

```bash
# Course reader
cd book && quarto render

# Lecture slides (single unit or complete edition, see slides/README.md)
cd slides && quarto render lectures/01_einfuehrung-und-grundbegriffe-des-supply-chain-managements.qmd
cd slides && quarto render --profile full

# Case studies
cd case_studies && quarto render
```

The output is written to the `docs/` folder in each case. The working directory for R code is the respective project folder, so the function library is loaded with `source("../R/scm_functions.R")`.

---

## Software

- **R** ≥ 4.3 with **RStudio** or Positron
- **Quarto** ≥ 1.4 (<https://quarto.org>)

```r
install.packages(c(
  "tidyverse",        # data preparation and graphics
  "lubridate",        # date functions (case study 5)
  "zoo",              # moving averages (case study 4)
  "knitr", "kableExtra",
  "plotly",           # interactive graphics in the reader
  "ompr", "ompr.roi", "ROI", "ROI.plugin.glpk",  # MILP (case study 3)
  "lpSolve",          # transportation problem
  "MASS"
))
```

A (free) **Kaggle account** is required to download the datasets for case studies 4 and 5.

If you want to familiarise yourself with R, an introductory course is available at [Data Science](https://ds-pl-r-book.netlify.app/).


---

## Literature

| Title | Authors | Relevance |
|---|---|---|
| *Supply Chain Management: Strategy, Planning, and Operation* | Chopra & Meindl | all topics |
| *Matching Supply with Demand* | Cachon & Terwiesch | risk pooling, newsvendor, bullwhip |
| *Sustainable Logistics and Supply Chain Management* | Grant et al. | sustainability |

Further sources are listed in the reader and in the case studies (`literature/!references.bib`).

---

## Contact

Prof. Dr. Thomas Kirschstein – thomas.kirschstein@hs-rm.de

Please report errors and suggestions for improvement as a GitHub issue or by e-mail to thomas.kirschstein@hs-rm.de.
