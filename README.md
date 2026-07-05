<div align = "center">

# EVD-COD17-butembo

Epidemiological situation report for the Ebola Virus Disease response in **Butembo & Katwa Health zones (city of Butembo)** (RDC)

</div>

## About

This project produces the routine epidemiological situation report
(*"Rapport de situation: ville de Butembo"*) prepared by Médecins Sans
Frontières with the support of the Ministère de la Santé, RDC.

It reads the cleaned EVD linelist for Butembo, alert, contact and transmission data from
the shared SharePoint folder, runs a set of analyses, and assembles the
results into a Word report. The analyses cover:

- **Overview** — confirmed cases by health zone and reporting facilities
- **Time** — epidemic curves (by onset and by notification date)
- **Person** — age/sex distribution of cases
- **Delays** — key delays in the response
- **Alerts** and **contact tracing** — follow-up overview and maps
- **Transmission** — transmission chains analysis
- **CFR** — case fatality calculation and adjustment. 
- **Health facilities** — patient care pathways before isolation

## Project layout

- `R/` — numbered analysis scripts, run in order (`0_global.R` sets up paths
  and shared config; `1_prep_data.R` cleans the data; `2_`–`10_` produce the
  figures written to `output/`).
- `report/` — the Quarto report (`butembo-report.qmd`), its template, and the
  one-command render pipeline (`_render.R`).
- `output/` — generated figures (gitignored).
- `data`, `local`, `temp` — local data / output storage (gitignored).

## Getting started

The project reads its data from a OneDrive/SharePoint folder. Set the path in
your `.Renviron` file (in your `HOME` or the project directory):

```r
SHAREPOINT_PATH="ADD YOUR SHAREPOINT PATH HERE"
```

Restart your R session so the updated `.Renviron` is loaded.

## Producing the report

Run the render pipeline, which regenerates all figures and renders the `.docx`:

```r
source(here::here("report", "_render.R"))
```

This runs the analysis scripts in order, stamps the report with the data
cut-off date, and writes `report/butembo-report.docx`.
