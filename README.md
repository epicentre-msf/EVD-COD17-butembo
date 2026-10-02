<div align = "center">

# EVD-COD17-butembo
Epidemiological situation report and analyses for the BDBV response in **Butembo & Katwa Health zones (city of Butembo)** (RDC). All sitreps can be found on [this sharepoint](https://msfintl.sharepoint.com/:f:/r/sites/GRP-PAR-RDC/CD153EBOLABUTEMBO/10%20M%C3%A9dical/17%20Epidemiologie/Analyses/rapport%20MSF/Butembo/ocp-sitreps-butembo?csf=1&web=1&e=1tHqJP).

Contact: hugo.soubrier@epicentre.msf.org / msff-butembo-ebola-epidemio@paris.msf.org

</div>

## About

This repository holds the code for the epidemiological surveillance of the
BDBV response in Butembo, prepared by Médecins Sans Frontières with the
support of the Ministère de la Santé, RDC. It contains three things:

- **Data prep** — scripts that clean the EVD linelist, health-facility visits,
  alerts and contacts data, and export the datasets used downstream.
- **Dashboard** — a Shiny app (`R/butembo_dashboard/`) for exploring the
  surveillance data: epicurves, maps, delays, health facilities, laboratory
  results, data quality and a case timeline.
- **Ad-hoc analyses** — one-off scripts answering specific questions, such as
  the vaccinated-case linelist and the Beni dataset.

## Data sources
All data are stored on the OCP sharepoint for the Butembo project and are only available to authorised access. 

### EVD linelist data
The epicentre Linelist used across the outbreak is manually filled every day using the data triangulated from the laboratory database, the local linelist, the case investigations, and the case narratives. 

### CTE linelist data
A separate export for the CTE Kitatumba and CT UCG holds the treatment-centre patient linelist and a daily bed-occupancy sheet.

## Project layout

- `R/` — data prep and ad-hoc scripts.
  - `0_global.R` — paths and shared config, sourced by every script.
  - `1_prep_data.R` — cleans the linelist, facility visits, alerts and
    contacts. The only script that reads SharePoint; it writes the cleaned
    data and `app_data.rds` for the dashboard.
  - `01b_prep_beni_data.R` — imports and cleans the Beni data.
  - `fn_quality.R` — data-quality tables shown in the dashboard.
  - `prep_for_sharing.R` — de-identified linelist for sharing outside the epi team.
  - `vaccinated_cases.R` — shareable vaccinated-case linelist.
  - `butembo_dashboard/` — the Shiny app, one `mod_*.R` file per tab.
  - `archives/` — retired situation-report scripts.
- `report/` — legacy Quarto situation report (`butembo-report.qmd`) and its
  render pipeline (`_render.R`).
- `output/`, `data/`, `local/`, `temp/` — local data and generated files (gitignored).

## Getting started

The project reads its data from a OneDrive/SharePoint folder. Set the path in
your `.Renviron` file (in your `HOME` or the project directory):

```r
SHAREPOINT_PATH="ADD YOUR SHAREPOINT PATH HERE"
```

Restart your R session so the updated `.Renviron` is loaded.

## Updating the data and dashboard

Clean the latest exports and refresh the dashboard data:

```r
source(here::here("R", "1_prep_data.R"))
```

Two switches at the top of the script control the outputs: `EXPORT_TO_SHAREPOINT`
writes the cleaned data back to SharePoint, and `SEND_TO_SERVER` rsyncs
`app_data.rds` to the dashboard on the server.

To run the dashboard locally:

```r
shiny::runApp(here::here("R", "butembo_dashboard"))
```

To update the deployed dashboard, pull the code on the server, then rerun
the data prep with `SEND_TO_SERVER <- TRUE`:

```sh
ssh episerv "cd EVD-COD17-butembo && git pull"
```