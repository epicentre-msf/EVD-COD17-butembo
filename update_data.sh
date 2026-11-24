#!/usr/bin/env bash
# Update the Butembo data: alerts, then linelist / facilities / dashboard bundle.
#
# Usage: ./update_data.sh [flags]
#   --skip-alerts        reuse the last alerts export (skips R/02_prep_alerts.R)
#   --update-geobase     copy the latest OutbreakTools geobase before loading it
#   --no-export          do not write exports to SharePoint
#   --no-send            do not rsync app_data.rds to the server
#   --lab                rebuild the lab data
#   --geo-tol=<degrees>  simplification of the app layers (0 keeps full detail)
# All flags except --skip-alerts are passed to R/1_prep_data.R (see R/cli_args.R).

set -euo pipefail

# run from the repo root whatever the current directory
cd "$(dirname "$0")"

run_alerts=true
prep_args=()
for arg in "$@"; do
  case "$arg" in
    -h | --help)
      sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    --skip-alerts) run_alerts=false ;;
    --update-geobase | --no-export | --no-send | --lab | --geo-tol=*)
      prep_args+=("$arg")
      ;;
    *)
      echo "Unknown flag: $arg (see ./update_data.sh --help)" >&2
      exit 1
      ;;
  esac
done

if $run_alerts; then
  echo "==> Alerts (R/02_prep_alerts.R)"
  Rscript R/02_prep_alerts.R
fi

echo "==> Data prep (R/1_prep_data.R ${prep_args[*]:-})"
Rscript R/1_prep_data.R ${prep_args[@]+"${prep_args[@]}"}

echo "==> Done"
