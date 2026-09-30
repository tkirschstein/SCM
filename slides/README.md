# Lecture slides

## Rendering a single lecture

From the `slides/` directory, each file in `lectures/` can be rendered separately, for example:

```bash
quarto render lectures/06_unsicherheit-in-supply-chains-pooling.qmd
```

## Rendering the complete edition

The file `scm-komplett.qmd` includes the eight lecture units in numerical order. The `full` profile restricts the render run to this complete edition:

```bash
cd slides
quarto render --profile full
```

Alternatively, the complete edition can be generated directly:

```bash
quarto render scm-komplett.qmd
```

The bibliography is provided project-wide via `_quarto.yml` from `../literature/!references.bib`.
