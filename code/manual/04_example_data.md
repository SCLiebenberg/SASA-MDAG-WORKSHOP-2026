# Manual 04: Prepare the example data

Script: `code/04_example_data.R`

## 1. Purpose

This script prepares the data for the example in scripts 05 and 06.
It does two tasks:

1. It unzips the ERA5 file for November 2023.
2. It downloads GSOD station data for the South African stations in the same area.

## 2. Input and output

| | File | Description |
|---|---|---|
| Input | `data/raw/nov2023.zip` | ERA5 data from the CDS website. |
| Output | `data/raw/era5_nov2023/` | Two NetCDF files from the zip file. |
| Output | `data/raw/gsod_sa_2023.csv` | GSOD daily data for 2023, one row for each station and day. |
| Output | `data/raw/download_log.txt` | The date of the download and the package version. |

### The ERA5 file

We requested the ERA5 file on the CDS website. The request had these values:

| Field | Value |
|---|---|
| Dataset | ERA5 hourly data on single levels (`reanalysis-era5-single-levels`) |
| Variables | 2m temperature, surface pressure, total precipitation, downward UV radiation at the surface |
| Period | 1 to 30 November 2023, all 24 hours |
| Area | North −24.25, West 14.25, South −35.5, East 33 |
| Format | NetCDF, unarchived |

The area includes South Africa south of 24.25° S. The grid has 46 × 76 grid points, 0.25° apart.
The file has 720 hourly time steps (30 days × 24 hours).

## 3. Before you start

- Install the R package `GSODR`.
- Put `nov2023.zip` in `data/raw/`.
- Make sure that the computer has an internet connection. GSOD comes from a NOAA server.

## 4. Procedure

### Step 1: Set the working folder

```r
if (basename(getwd()) == "code") setwd("..")
```

The script must run from the workshop folder.
If you start the script from `code/`, this line moves one folder up.

### Step 2: Unzip the ERA5 file

```r
era5_dir <- file.path(raw_dir, "era5_nov2023")
if (!dir.exists(era5_dir)) unzip(file.path(raw_dir, "nov2023.zip"), exdir = era5_dir)
```

The zip file holds two NetCDF files:

| File | Variables | Type |
|---|---|---|
| `data_stream-oper_stepType-instant.nc` | `t2m` (2m temperature), `sp` (surface pressure) | Instantaneous: the value at the time stamp. |
| `data_stream-oper_stepType-accum.nc` | `tp` (total precipitation), `uvb` (UV radiation) | Accumulated: the total for the hour before the time stamp. |

The CDS makes one file for each type.
That is why a "NetCDF" request can give a zip file.

### Step 3: Select the stations

```r
load(system.file("extdata", "isd_history.rda", package = "GSODR"))
stns <- isd[isd$CTRY == "SF" & ... , ]
```

The GSODR package contains the NOAA station list (`isd_history`).
The script keeps a station when all these conditions are true:

- The country code is `SF` (South Africa).
- The station is in the ERA5 area.
- The station record starts before 1 November 2023 (`BEGIN`).
- The station record continues after 30 November 2023 (`END`).

### Step 4: Download the GSOD data

```r
gsod <- get_GSOD(years = 2023, station = id)
```

The script downloads one station at a time.
If one station has no file for 2023, the script writes a message and continues.

`get_GSOD()` does three things:

1. It downloads the station file for the year from NOAA.
2. It changes the values to metric units (°C, mm, m/s, hPa, km).
3. It changes the GSOD missing-value codes (for example 9999.9) to `NA`.

The script saves the result in `data/raw/gsod_sa_2023.csv`.
If this file is already there, the script does not download again.

### Step 5: Write the log

The script writes the date of the download in `download_log.txt`.
Keep this file. It tells other users which version of the data you used.

## 5. Results

The GSOD file has one row for each station and day with a report.
These columns are used in scripts 05 and 06:

| Column | Meaning | Unit |
|---|---|---|
| `STNID` | Station identifier (USAF-WBAN number) | |
| `NAME` | Station name | |
| `LATITUDE`, `LONGITUDE` | Station position | degrees |
| `ELEVATION` | Station height above sea level | m |
| `YEARMODA` | Date (a UTC day) | |
| `TEMP` | Mean temperature of the day | °C |
| `MAX` | Maximum temperature of the day | °C |
| `PRCP` | Precipitation of the day | mm |

For November 2023, the file gives 157 stations.

**Important:** A day without reports has no row in GSOD.
The day is not in the file with `NA`. The row is not there.
Script 05 finds these missing days.

## 6. Problems

| Problem | Cause | What to do |
|---|---|---|
| `cannot open zip file` | `nov2023.zip` is not in `data/raw/`. | Put the file in `data/raw/`. Check the file name. |
| `there is no package called 'GSODR'` | The package is not installed. | Run `install.packages("GSODR")`. |
| A message for one station, for example "only provides data for years ..." | The station has no data for 2023. | No action. The script continues with the next station. |
| The download stops | The NOAA server is slow or the connection stopped. | Run the script again. The finished CSV file is not downloaded again. |
| You want new GSOD data | The script does not download again when the CSV file exists. | Delete `gsod_sa_2023.csv`. Then run the script again. |

## 7. Terms

| Term | Meaning |
|---|---|
| ERA5 | A reanalysis from ECMWF. A weather model combines old observations into complete hourly maps from 1940 to a few days ago. |
| GSOD | Global Surface Summary of the Day. Daily summaries from weather stations, from NOAA. |
| Grid cell | One square of the ERA5 grid, 0.25° × 0.25° (about 28 km × 25 km in South Africa). |
| NetCDF | A file format for data on a grid, with dimensions such as longitude, latitude and time. |
| UTC | Coordinated Universal Time. SAST = UTC + 2 hours. |
