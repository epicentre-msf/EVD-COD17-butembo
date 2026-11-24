#!/usr/bin/env bash
# Update the Butembo data: linelist / facilities / dashboard bundle, reusing the
# latest alerts export unless --update-alerts.
#
# Usage: ./update_data.sh [flags]
#   --update-alerts      rerun R/02_prep_alerts.R first (default: reuse the latest export)
#   --update-geobase     copy the latest OutbreakTools geobase before loading it
#   --no-export          do not write exports to SharePoint
#   --no-send            do not rsync app_data.rds to the server
#   --update-lab         rebuild the lab data (default: reuse it from the last app_data.rds)
# All flags except --update-alerts are passed to R/1_prep_data.R (see R/cli_args.R).

set -euo pipefail

# run from the repo root whatever the current directory
cd "$(dirname "$0")"

run_alerts=false
prep_args=()
for arg in "$@"; do
  case "$arg" in
    -h | --help)
      sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    --update-alerts) run_alerts=true ;;
    --update-lab | --update-geobase | --no-export | --no-send)
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
