<div align = "center">

# EVD-COD17-butembo
Epidemiological situation report and analyses for the BDBV response in **Butembo & Katwa Health zones (city of Butembo)** (RDC). All sitreps can be found on [this sharepoint](https://msfintl.sharepoint.com/:f:/r/sites/GRP-PAR-RDC/CD153EBOLABUTEMBO/10%20M%C3%A9dical/17%20Epidemiologie/Analyses/rapport%20MSF/Butembo/ocp-sitreps-butembo?csf=1&web=1&e=1tHqJP).

Contact: hugo.soubrier@epicentre.msf.org / msff-butembo-ebola-epidemio@paris.msf.org

</div>

## About

This project produces the routine epidemiological situation report
(*"Rapport de situation: ville de Butembo"*) prepared by Médecins Sans
Frontières with the support of the Ministère de la Santé, RDC.

It reads the latest exports from the EVD linelist for Butembo, the alerts and contacts data, and the transmission data from the SharePoint folder, runs a set of analyses, and assembles the
results into a Word report. The analyses cover:

- **Overview** — confirmed cases by health zone and reporting facilities
- **Time** — epidemic curves (by onset and by notification date)
- **Person** — age/sex distribution of cases
- **Delays** — key delays in the response
- **Alerts** and **contact tracing** — follow-up overview and maps
- **Transmission** — transmission chains analysis
- **CFR** — case fatality calculation and adjustment. 
- **Health facilities** — patient care pathways before isolation
- **Treatment centre (CTE)** — bed occupancy and patient flows (admissions & exits) at CTE Kitatumba

## Data sources
All data are stored on the OCP sharepoint for the Butembo project and are only available to authorised access. 

### EVD linelist data
The epicentre Linelist used across the outbreak is manually filled every day using the data triangulated from the laboratory database, the local linelist, the case investigations, and the case narratives. 

### Alerts and Contacts data
Alerts and contact data are retrived from the daily sitreps produced by the Health zones of Katwa and Butembo. These sitreps provide aggregated counts of daily alerts and contacts metrics by health areas. 

### Transmission data
Transmission data are reconstructed using the cases investigations and narratives.

### CTE linelist data
A separate export for the CTE Kitatumba (*"Liste-linéaire CTE Kitatumba"*) holds the treatment-centre patient linelist and a daily bed-occupancy sheet. It feeds the treatment-centre occupancy and patient-flow tables (`R/etc_analysis.R`).

## Project layout

- `R/` — numbered analysis scripts, run in order (`0_global.R` sets up paths
  and shared config; `1_prep_data.R` cleans the data; `2_`–`10_` produce the
  figures written to `output/`). `etc_analysis.R` is a standalone script that
  builds the CTE occupancy and patient-flow tables.
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

This runs the analysis scripts in order, stamps the report with the data cut-off date, and writes `report/butembo-report.docx`.
