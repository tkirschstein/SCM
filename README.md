# Supply Chain Management — Course Materials

**HS RheinMain | Bachelor Program | 3rd Semester | English**

Prof. Dr. Thomas Kirschstein

---

## Course Overview

This repository contains all lecture, exercise, and case study materials for the bachelor-level Supply Chain Management course at Hochschule RheinMain. The course introduces students to the quantitative and strategic foundations of SCM, with a focus on hands-on modelling in R.

**Course goal**: Students develop competency in analysing, designing, and optimising supply chains — from strategic network design to operational planning under uncertainty.

**Credit hours**: 4 SWS (lecture + exercise session)  
**Workload**: 150 hours (60 contact, 90 self-study)  
**Assessment**: Written exam (60%) + Case study (40%)  
**Language**: English  
**Prerequisites**: Mathematics for economists (linear algebra, probability), introductory statistics, basic programming literacy

---

## Repository Structure

```
SCM/
├── slides/              # Quarto revealjs lecture slides (6 modules)
│   ├── 01_foundations/  # SCM basics, flows, planning hierarchy
│   ├── 02_strategy/     # Strategic fit, location planning, AHP
│   ├── 03_network_design/  # Transportation problem, WLP heuristics
│   ├── 04_transportation/  # Uncertainty, Newsvendor, Bullwhip
│   ├── 05_uncertainty/  # SC coordination, contracts, VMI
│   └── 06_coordination/ # Sustainable SCM, circular supply chains
├── book/                # Quarto HTML course reader
│   └── custom.css       # Matching CSS theme for the book
├── exercises/           # Exercise sheets with R solutions (5 blocks)
├── case_studies/        # Team-based case studies (2 cases)
├── data/                # Datasets for exercises and case studies
├── R/                   # Reusable R function library
│   └── scm_functions.R  # All course functions documented with roxygen2
└── _extensions/         # Custom Quarto reveal.js theme
    └── scm-theme.scss
```

---

## Modules

### Module 1 — SCM Foundations
What is a supply chain? Flows (material, financial, information), planning hierarchy (strategic/tactical/operational), key performance metrics (OTIF, inventory turns, COGS), and the trade-off triangle (cost – service – flexibility). Introduction to the DuPont tree as a strategic SCM lens.

### Module 2 — Location Planning and Strategic Fit
Strategic fit between competitive strategy and supply chain design. Location planning: Center of Gravity, Weiszfeld algorithm, and the Analytic Hierarchy Process (AHP) for multi-criteria site selection. Case: selecting a European distribution centre.

### Module 3 — Network Design
Transportation problem formulation. Solution methods: North-West Corner, Minimum Cost Method, Vogel's Approximation, and the simplex-based stepping-stone method. Warehouse Location Problem: Add Heuristic, Drop Heuristic, exact MILP with ompr/GLPK. Facility capacitation.

### Module 4 — Managing Transportation Uncertainty
Demand uncertainty and the single-period problem. Newsvendor Model: derivation, critical ratio, service levels, and expected profit. Extensions: lost sales, emergency orders, quick-response retailing. The Bullwhip Effect: causes, measurement, and mitigation strategies.

### Module 5 — Supply Chain Coordination
Decentralised decision-making and the double-marginalisation problem. Coordination contracts: buy-back, revenue sharing, quantity flexibility, two-part tariff. Vendor Managed Inventory (VMI) and Collaborative Planning, Forecasting and Replenishment (CPFR). Information sharing and the value of demand data.

### Module 6 — Sustainable and Circular Supply Chains
Sustainability in SCM: Scope 1/2/3 emissions, transport decarbonisation, and lifecycle thinking. Circular supply chains: reverse logistics, closed-loop systems, remanufacturing economics. ESG reporting frameworks (GRI, TCFD) and practical sustainability KPIs.

---

## Learning Materials

### Recommended Textbooks

| Title | Authors | Publisher | ISBN | Relevant Chapters |
|-------|---------|-----------|------|-------------------|
| *Supply Chain Management: Strategy, Planning, and Operation* (7th ed.) | Chopra & Meindl | Pearson, 2019 | 978-0-13-520072-0 | All modules |
| *Fundamentals of Supply Chain Management* | Mentzer et al. | Sage, 2004 | 978-0-7619-2849-4 | Modules 1, 5, 6 |
| *Operations Research: An Introduction* (10th ed.) | Taha | Pearson, 2017 | 978-0-13-438113-4 | Modules 3, 4 |
| *Matching Supply with Demand* (3rd ed.) | Cachon & Terwiesch | McGraw-Hill, 2012 | 978-0-07-352515-8 | Modules 4, 5 |
| *Sustainable Logistics and Supply Chain Management* (rev. ed.) | Grant et al. | Kogan Page, 2017 | 978-0-7494-8009-3 | Module 6 |

### Online Resources

**Open Courseware:**
- [MIT OpenCourseWare 15.762 Supply Chain Planning](https://ocw.mit.edu/courses/15-762j-supply-chain-planning-spring-2011/) — lecture notes and problem sets
- [MIT OpenCourseWare 15.760 Introduction to Operations Management](https://ocw.mit.edu/courses/15-760b-introduction-to-operations-management-spring-2004/)
- [MIT CTL SCM Blossoms](https://blossoms.mit.edu/scm) — short video explanations of key SCM concepts
- [APICS/ASCM Body of Knowledge](https://www.ascm.org/learning-development/certifications-credentials/cpim/) — CPIM and CSCP certification study materials

**Industry and News:**
- [Supply Chain Management Review (scmr.com)](https://www.scmr.com) — practitioner articles
- [Logistics Management (logisticsmgmt.com)](https://www.logisticsmgmt.com) — industry news
- [Supply Chain Dive (supplychaindive.com)](https://www.supplychaindive.com) — current events and case analysis

**Data and Research:**
- [Council of Supply Chain Management Professionals (CSCMP)](https://cscmp.org) — annual State of Logistics Report
- [World Bank Logistics Performance Index](https://lpi.worldbank.org/) — country-level logistics benchmarks

### Software and Tools

**R / RStudio Setup:**

```r
# Install all packages used in this course
install.packages(c(
  # Core data and plotting
  "tidyverse",    # dplyr, ggplot2, tidyr, readr, purrr
  "ggrepel",      # non-overlapping text labels in ggplot2
  "scales",       # axis formatting helpers

  # Optimisation
  "lpSolve",      # linear and transportation problem solver
  "ompr",         # MILP modelling layer
  "ompr.roi",     # ROI backend for ompr
  "ROI",          # R Optimization Infrastructure
  "ROI.plugin.glpk",  # GLPK solver binding

  # Geographic computation
  "sf",           # spatial features (optional — for proper maps)

  # Tables
  "knitr",
  "kableExtra"    # styled HTML/PDF tables
))
```

**Minimum R version**: 4.2.0  
**RStudio version**: 2023.06 or later (Quarto integration)  
**Quarto version**: 1.4 or later

**Recommended IDE setup:**
- R 4.3+ with RStudio 2023.12+
- Quarto CLI installed separately (quarto.org)
- GLPK installed system-wide: `sudo apt-get install glpk-utils` (Linux/WSL) or via Homebrew on macOS

---

## Interactive Learning Formats

### Beer Game Session

The **Beer Distribution Game** (Sterman, 1989) simulates the Bullwhip Effect in a 4-echelon supply chain (retailer → wholesaler → distributor → manufacturer). Students manage one echelon and observe how small demand fluctuations amplify upstream.

**Session format** (recommended: 60–90 minutes):
1. Rules briefing (10 min): explain order mechanics, backlog cost, holding cost
2. Game rounds 1–20 (30 min): students make decisions in silence
3. Debrief (20 min): plot inventory/order patterns across echelons; calculate total cost
4. Discussion (20 min): causes of bullwhip, managerial interventions

**Online platforms:**
- [beergame.org](https://www.beergame.org) — free, browser-based, multiplayer
- [MIT LearningEdge Beer Game](https://beergame.mit.edu) — MIT-hosted version with built-in analytics

### AHP Group Exercise

Groups of 4 select a site for a real-world facility (e.g. a regional e-commerce fulfilment centre). Each group:

1. Identifies 4–5 location alternatives (real cities/industrial parks)
2. Defines 4–6 decision criteria (labour costs, transport access, land availability, regulatory environment, proximity to customers)
3. Performs pairwise comparisons — each group member independently fills in the AHP matrix
4. Discusses inconsistencies in preferences
5. Presents their recommended location with AHP weights and sensitivity analysis

**R functions used**: `ahp_priority_vector()`, `ahp_consistency()` from `R/scm_functions.R`

### Supply Chain Design Hackathon

A 3-hour competitive exercise where teams design a complete supply chain for a given product scenario. Teams receive a brief with demand data, supply options, and cost parameters. Deliverables:

- Network design map (warehouses, routes)
- Total annual cost calculation
- Service level achieved
- One identified risk and mitigation

Winning team: lowest total cost with ≥95% service level. Graded on methodology and presentation.

---

## Assessment

| Component | Weight | Format | Notes |
|-----------|:------:|--------|-------|
| Written Exam | 60% | 90 min, closed book (formula sheet provided) | Modules 1–6, with calculations and interpretation |
| Case Study | 40% | Team report (12 pp) + 10 min presentation | Case Study 1 or 2, assigned by instructor |

**Exam format:**  
- Part A: Multiple choice / short answer (30 min)
- Part B: Quantitative problems — DuPont, Location, Transportation (30 min)
- Part C: Newsvendor / WLP with interpretation (30 min)
- Formula sheet: one A4 page provided by the examiner

**Case study grading** (see rubric in each case study file):
- Problem formulation (25%)
- Solution methodology (25%)
- Interpretation / management recommendations (30%)
- Presentation quality (20%)

**Pass threshold**: ≥50 points (out of 100) overall; ≥40 points in each case study criterion.

---

## Contributing

Issues, corrections, and improvements are welcome. To contribute:

1. **Report errors**: Open a GitHub Issue with the label `bug` or `content-error`. Include the file name and line number.

2. **Suggest exercises**: Open an Issue with the label `enhancement`. Describe the topic, the learning objective, and a sketch of the problem.

3. **Submit corrections via Pull Request**: Fork the repository, make changes in a feature branch, and submit a PR referencing the relevant Issue.

4. **Contact**: thomas.kirschstein@hs-rm.de

**Coding style for R files:**
- Use `snake_case` for function and variable names
- Document all exported functions with roxygen2-style `#'` comments
- Prefer base R for core algorithms; use tidyverse for data manipulation and ggplot2 for visualisation
- Include `stopifnot()` input validation in all exported functions

---

*Last updated: see `date: last-modified` in individual Quarto files.*  
*Repository maintained by the SCM teaching team at HS RheinMain, Wiesbaden.*
