# Llegir bé l'escrutini

Com anticipar el resultat final d'unes eleccions generals a Espanya ponderant les meses ja escrutades amb el resultat de les eleccions anteriors.

**Informe:** <https://jordi-munoz.github.io/escrutini/> (en català) · [versión en castellano](https://jordi-munoz.github.io/escrutini/informe_escrutini_es.html)

*English summary:* during election night, Spanish partial results are misleading because rural booths are counted first and big cities last. Reweighting the booths already counted so that their previous-election results match each province's previous result recovers the final vote shares (within 1 pp for every main party) with only 10–12% of booths counted, versus 52–80% for the raw partial count. Simulation with booth-level data for the 2016, April 2019, November 2019 and 2023 general elections.

## Estructura

- `R/00_functions.R`: lectura dels fitxers del Ministeri de l'Interior, calibratge (*raking*) i D'Hondt.
- `R/01_simulate.R`: simulació de l'escrutini → `output/sim_results.rds`.
- `R/02_figures.R`: figures en anglès (`output/figures/`), en català (`Rscript R/02_figures.R ca` → `output/figures_ca/`) o en castellà (`Rscript R/02_figures.R es` → `output/figures_es/`).
- `informe_escrutini.qmd` / `informe_escrutini_es.qmd`: informe en Quarto, en català i en castellà.
- `run_all.R`: executa la simulació i les figures.

## Dades

Les dades en brut no s'inclouen al repositori. Són els fitxers de resultats per mesa del Congrés (`02AAAAMM_MESA`) del [Ministeri de l'Interior](https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/) per a desembre de 2015, juny de 2016, abril de 2019, novembre de 2019 i juliol de 2023. Cal descomprimir-los a `data/`, per exemple `data/02202307_MESA/`.

## Reproduir

```sh
Rscript run_all.R                       # simulació (~5 min) i figures en anglès
Rscript R/02_figures.R ca               # figures en català
quarto render informe_escrutini.qmd     # informe HTML
```

Paquets d'R: `data.table`, `ggplot2`, `scales`, `ragg`, `knitr`.
