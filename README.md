
<div align = "center">

# EVD-COD17-butembo

Project for epidemiological analysis of the Ebola 2026 outbreak in Bulembo, North Kivu, DRC.

</div>



## Getting started

This template requires the path to your OneDrive Sharepoint folder. 
Set it up in your `.Renviron` file located in your `HOME` or project directory. 

Setting the environment variables in your `HOME` directory will make it
available to all your R projects. On the other hand project level 
`.Renviron` make the environment variables only available to your specific project. 

- In the `.Renviron` file, add:

    ```r
    SHAREPOINT_PATH="ADD YOUR SHAREPOINT PATH HERE"
    ```
- Restart your R session so that the updated `.Renviron` is loaded
- The [`set_paths()`](R/setpaths.R) function allows you to quickly access
 your sharepoint path as well as all of the synced sub-folders in the sharepoint folder. 
*This function requires to have {janitor}, {stringr}, {cli} and {fs} packages installed.*

## Ignored folders

In every project, the following directories are gitignored by default,
you can add them to the project depending on your needs:

- `data` for local data storage.
- `local` for local output (ex. pdfs, html, png, excels files).
- `temp` for temporary/sensitive files.
- `renv` files and folders if you configured [`renv`](https://cran.r-project.org/web/packages/renv/index.html) for your project.

## Rmd template

You can create rmarkdown template using the [`epitemplates`](https://github.com/epicentre-msf/epitemplates) R package.

- Install the `epitemplates` R package

```r
# install.packages("remotes")
remotes::install_github("epicentre-msf/epitemplates")
```

- To create a new `Rmd` file in the project, run in the console

```r
epitemplates::epi_rmd(path = "Rmd/[newfile].Rmd")
```

where `[newfile]` is the new file to add. Start editing the `.Rmd` file.

Alternatively if you are using RStudio you can create a new Rmd file
by using the addins `Addins > Epitemplates > Rmd document`. 
You will be prompted to pick a file path.

## Quarto template

You can create quarto template using the [`epitemplates`](https://github.com/epicentre-msf/epitemplates) R package.

- Install the `epitemplates` quarto package

```r
# install.packages("remotes")
remotes::install_github("epicentre-msf/epitemplates")
```

- To create a new `Rmd` file in the project, run in the R console

```r
epitemplates::epi_qmd(path = "Qmd/[newfile].qmd")
```

where `[newfile]` is the new file to add. Start editing the `.qmd` file.

Alternatively if you are using RStudio you can create a new Rmd file
by using the addins 
`Addins > Epitemplates > Quarto document`. You will be prompted to pick a file path.

## Rendering documents

We usually render documents to a local folder called `local`. 
`quarto`does not allow to render a file to an output directory other 
than the one of the quarto document, [unless you have a project structure](https://github.com/quarto-dev/quarto-r/issues/81).
`epitemplates` has a wrapper function for rendering documents to your 
local folder.

```r
# render qmd to your local folder
epitemplates::epi_render(
    input = "Qmd/[yourfile].qmd",
    output_format = "all",
    output_folder = "local"
)

# render Rmd to your local folder
epitemplates::epi_render(
    input = "Rmd/[yourfile].Rmd",
    output_format = "all",
    output_folder = "local"
)
```

Depending on the setup of your local folder you may want to render to a
specific sub-folders of your `local` folder. For example, render all 
`Rmd/Qmd` files to a report sub-folder, or one specific `Rmd/Qmd` file
to a specific sub-folder. You can configure these options in the 
`YAML` [`_outputs.yml`](Rmd/_outputs.yml) in each `Rmd/Qmd` folder. 
There is a default one in the current project structure you can start with.
