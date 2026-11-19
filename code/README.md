# Workshop code

This folder holds the R scripts for the workshop "Sourcing Multivariate Climate Data".
The scripts download climate data, make figures and fit a simple model.

## The scripts

| Script | What it does | Manual |
|---|---|---|
| `01_download_gsod.R` | Downloads GSOD station data with the GSODR package. | |
| `02_download_era5.R` | Requests ERA5 data from the Copernicus Climate Data Store (CDS) with the ecmwfr package. | |
| `03_era5_nc_to_csv.R` | Changes an ERA5 NetCDF file into a CSV table. | |
| `04_example_data.R` | Prepares the data for the example: ERA5 for November 2023 and GSOD for the same month. | [Manual 04](manual/04_example_data.md) |
| `05_visualise.R` | Makes maps and graphs of the ERA5 and GSOD data with terra and ggplot2. | [Manual 05](manual/05_visualise.md) |
| `06_gam_model.R` | Fits a generalised additive model (GAM) that compares ERA5 with the stations. | [Manual 06](manual/06_gam_model.md) |
| `make_figures.R` | Runs scripts 05 and 06. It makes all the figures for the slides. | |

## How to run the example

1. Install the packages. Run `setup/install_packages.R` one time.
2. Put the ERA5 file `nov2023.zip` in the folder `data/raw/`.
3. Open RStudio. Set the working folder to the workshop folder (the folder that holds `code/` and `data/`).
4. Run `code/04_example_data.R`. This step needs an internet connection.
5. Run `code/make_figures.R`.

The figures go to `figures/`. The processed data goes to `data/processed/`.

You can also start a script from the `code/` folder. The script then moves to the workshop folder.

## Rules that apply to all scripts

- **Time.** All times are in UTC (Coordinated Universal Time). South African Standard Time (SAST) is UTC + 2 hours. A "day" is a UTC day, from 00:00 to 23:59 UTC. GSOD uses the same day.
- **Units.** Temperature is in degrees Celsius (°C). Precipitation is in millimetres (mm). Elevation is in metres (m).
- **Raw data.** Do not change the files in `data/raw/`. The scripts only read these files.

## About the manuals

The manuals follow the main writing rules of ASD-STE100 Simplified Technical English:

- Short sentences: not more than 20 words in an instruction, and not more than 25 words in a description.
- One instruction in each sentence.
- Active verbs.
- One term for one thing. For example, the manuals always use "grid cell" for one square of the ERA5 grid.

Each manual has the same parts: purpose, input and output, procedure, results, and problems.
