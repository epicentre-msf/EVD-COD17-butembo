# ! Function to fetch and update the GEOBASE

# the latest geobase is pulled from the GIS master database, cleaned, and data checks by Patrick in https://github.com/epicentre-msf/outbreak-tools-geobase/blob/main/geo/COD_2026.R
# The geobase .xlsx and .rds are then stored in the Outbreak Tools geobase sharepoint: https://msfintl.sharepoint.com/sites/grp-epi-proj-outbreaktools/GeoBase/Forms/AllItems.aspx?id=%2Fsites%2Fgrp-epi-proj-outbreaktools%2FGeoBase%2FCOD
# This function
# 1. finds the requested geobase version for a country (ADM/HF), "latest" by default
# 2. copies the flat references (adm_reference_<country>.xlsx, hf_reference_<country>.xlsx)
#    into <butembo_geo_path>/<version>/
# 3. copies the sf objects (<country>_adm1.rds ... <country>_adm4.rds, <country>_hf.rds) into
#    <butembo_geo_path>/<version>/sf/
#
# Source layout (OutbreakTools - GeoBase):
#   <country>/<country>__ALL__<yyyymmdd>__<hhmm>H/
#     adm_reference_<country>.xlsx, hf_reference_<country>.xlsx
#     sf/<country>_adm1.rds ... <country>_adm4.rds, <country>_hf.rds

update_geobase <- function(
    country,
    butembo_geo_path,
    version = "latest",
    type = c("adm", "hf"),
    path_onedrive = Sys.getenv("SHAREPOINT_PATH"),
    overwrite = FALSE
) {
    type <- match.arg(type, several.ok = TRUE)

    #* locate the country geobase ----
    path_obtgeo <- fs::path(path_onedrive, "OutbreakTools - GeoBase")
    if (!fs::dir_exists(path_obtgeo)) {
        stop(
            "Directory 'OutbreakTools - GeoBase' not found within `path_onedrive`"
        )
    }
    path_country <- fs::path(path_obtgeo, country)
    if (!fs::dir_exists(path_country)) {
        stop("No geobase directory found for country '", country, "'")
    }

    #* resolve the version ----
    available_vers <- fs::path_file(fs::dir_ls(
        path_country,
        type = "directory"
    ))
    available_vers <- available_vers[startsWith(
        available_vers,
        paste0(country, "__")
    )]
    if (version == "latest") {
        # folder names are <country>__ALL__<yyyymmdd>__..., so max() sorts by date
        version <- max(available_vers)
    }
    path_version <- fs::path(path_country, version)
    if (!fs::dir_exists(path_version)) {
        stop(
            "Check argument `version`, directory not found:\n",
            path_version,
            "\nAvailable versions:\n",
            paste(available_vers, collapse = "\n")
        )
    }
    message("Using geobase version: ", version)

    #* list source files ----
    ref_files <- fs::path(
        path_version,
        sprintf("%s_reference_%s.xlsx", type, country)
    )

    path_sf <- fs::path(path_version, "sf")
    sf_pattern <- paste0(
        "^",
        country,
        "_(",
        paste(
            c(if ("adm" %in% type) "adm[1-4]", if ("hf" %in% type) "hf"),
            collapse = "|"
        ),
        ")\\.rds$"
    )
    sf_files <- fs::dir_ls(path_sf, type = "file")
    sf_files <- sf_files[grepl(sf_pattern, fs::path_file(sf_files))]

    missing <- ref_files[!fs::file_exists(ref_files)]
    if (length(missing) > 0) {
        stop("Reference file(s) not found:\n", paste(missing, collapse = "\n"))
    }
    if (length(sf_files) == 0) {
        stop("No sf objects found in:\n", path_sf)
    }

    #* copy to the Butembo geo folder ----
    dest_version <- fs::path(butembo_geo_path, version)
    dest_sf <- fs::path(dest_version, "sf")
    if (fs::dir_exists(dest_version) && !overwrite) {
        message(
            "Geobase already up to date, nothing copied ",
            "(use `overwrite = TRUE` to replace):\n",
            dest_version
        )
        # updated = FALSE: callers skip work tied to a new version (reference map)
        return(invisible(list(
            version = version,
            path = dest_version,
            updated = FALSE
        )))
    }
    fs::dir_create(dest_sf)

    fs::file_copy(ref_files, dest_version, overwrite = overwrite)
    fs::file_copy(sf_files, dest_sf, overwrite = overwrite)

    message(
        "Copied ",
        length(ref_files),
        " reference file(s) and ",
        length(sf_files),
        " sf object(s) to:\n",
        dest_version
    )

    invisible(list(
        version = version,
        path = dest_version,
        updated = TRUE,
        ref = fs::path(dest_version, fs::path_file(ref_files)),
        sf = fs::path(dest_sf, fs::path_file(sf_files))
    ))
}

# update_geobase("COD", butembo_geo_path = butembo_project_sf_data_path)
