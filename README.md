<div align = "center">

# EVD-COD17-butembo
Epidemiological surveillance for the BDBV response in **Butembo & Katwa Health zones (city of Butembo)** (RDC). All sitreps can be found on [this sharepoint](https://msfintl.sharepoint.com/:f:/r/sites/GRP-PAR-RDC/CD153EBOLABUTEMBO/10%20M%C3%A9dical/17%20Epidemiologie/Analyses/rapport%20MSF/Butembo/ocp-sitreps-butembo?csf=1&web=1&e=1tHqJP).

Contact: hugo.soubrier@epicentre.msf.org / msff-butembo-ebola-epidemio@paris.msf.org

</div>

## About

Code for the epidemiological surveillance of the BDBV response in Butembo,
prepared by Médecins Sans Frontières with the support of the Ministère de la
Santé, RDC:

- **Data pipeline**: cleans the EVD linelist, lab results, health-facility
  visits, alerts and contacts, and builds the dashboard data.
- **Dashboard**: a Shiny app (`R/butembo_dashboard/`) with epicurves, maps,
  delays, health facilities, lab results, alerts, contacts, transmission
  chains, data quality and a reference map.
- **Ad-hoc analyses**: one-off scripts for specific questions.

No data is stored in this repository. All data live on the OCP SharePoint and
are only available with authorised access.

## Project layout

```
update_data.sh            run the data pipeline
deploy_code.sh            deploy the code to the server
R/
  0_global.R              packages, config and SharePoint paths, sourced by every script
  cli_args.R              command-line flags for 1_prep_data.R
  utils.R                 shared helpers
  1_prep_data.R           main pipeline: clean all data, export, build app_data.rds
  02_prep_alerts.R        stack the health-zone alert registers
  fn_quality.R            data-quality tables for the dashboard
  update_geobase.R        copy the latest OutbreakTools geobase
  reference-map.R         build the reference map (health zones, areas, facilities)
  butembo_dashboard/      Shiny app, one mod_*.R file per tab
  prep_for_sharing.R      de-identified linelist for sharing outside the epi team
  vaccinated_cases.R      vaccinated-case linelist
  transmission_chains.R   transmission chain tables
  etc_analysis.R          CTE occupancy and flows
  kitatumba_admissions.R  CTE Kitatumba admissions and exits
  01b_prep_beni_data.R    Beni data
  kayna_analysis.R        Kayna lab results (draft)
  archives/               retired sitrep scripts
report/                   retired Quarto sitrep (sources scripts now in R/archives)
```

Created when the scripts run (gitignored): `local/` (cleaned linelists,
duplicates, quality checks), `output/` (plots and tables) and
`R/butembo_dashboard/data/app_data.rds`.

## Setup

1. **Sync the SharePoint libraries** with OneDrive: the Butembo project
   workplace (`17 Epidemiologie`), *Epi Platform-BVD – Data and Tools*,
   *Ebola Outbreaks - COD_UGA-2026* and *OutbreakTools - GeoBase*.
2. **Point R to the synced folder** in `.Renviron` (in your `HOME` or the
   project directory), then restart R:
   ```r
   SHAREPOINT_PATH="path/to/your/OneDrive/folder"
   ```
   Folder names below it are set in `R/0_global.R`. Adjust them if your sync
   names differ.
3. **Install the packages.** Most are on CRAN. These come from GitHub:
   ```r
   remotes::install_github(c(
     "epicentre-msf/epishiny", "epicentre-msf/epithemes",
     "epicentre-msf/epivis", "epicentre-msf/qxl", "reconhub/epicontacts"
   ))
   ```
   `rpxl` (reads password-protected `.xlsb` exports) and `epishiny.contacts`
   are also needed for some scripts.
4. **To deploy**, add an `episerv` host to your `~/.ssh/config`.

## Updating the data

```sh
./update_data.sh            # clean data, export to SharePoint, send app_data.rds to episerv
./update_data.sh --help     # list the flags
```

| Flag | Effect |
|---|---|
| `--update-alerts` | rerun `02_prep_alerts.R` first (default: reuse the latest export) |
| `--update-lab` | rebuild the lab data (default: reuse it from the last `app_data.rds`) |
| `--update-geobase` | copy the latest geobase; a new version also rebuilds the reference map |
| `--no-export` | do not write exports to SharePoint |
| `--no-send` | do not send `app_data.rds` to episerv |

## Dashboard

Run it locally once `update_data.sh` has built `app_data.rds`:

```r
shiny::runApp(here::here("R", "butembo_dashboard"))
```