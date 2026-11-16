# Before the workshop: preparation checklist

Sourcing Multivariate Climate Data, SASA MDAG Workshop


Please complete the steps below **before** the workshop. Registering on the Copernicus Climate Data Store can involve an email confirmation, and installing R packages can take a while on a slow connection, so neither is practical to do during the session.

Allow about 30 to 45 minutes in total.

## Checklist

- [ ] 1. Create an account on the Copernicus Climate Data Store (CDS)
- [ ] 2. Accept the CDS Terms of Use and the ERA5 licence
- [ ] 3. Copy your personal API token
- [ ] 4. Install R and RStudio
- [ ] 5. Install the required R packages
- [ ] 6. Store your API token in R
- [ ] 7. Download the workshop materials

---

## 1. Create a Copernicus Climate Data Store account

ERA5 is distributed through the Copernicus Climate Data Store (CDS), which is run by ECMWF on behalf of the Copernicus Climate Change Service. Accounts are free.

1. Go to <https://cds.climate.copernicus.eu>.
2. Click **Login - Register** (top right) and choose to register a new user. You are creating an ECMWF account; the same login is used across ECMWF and Copernicus services.
3. Fill in the form and confirm your email address using the link sent to you.
4. Log in to the CDS with your new account.

If you created a CDS account before September 2024, note that the old CDS logins no longer work. You will need an ECMWF account as described above.

## 2. Accept the Terms of Use and the ERA5 licence

The CDS will refuse download requests (from the website and from R) until you have accepted both of the following.

**CDS Terms of Use.** After your first login, open your user profile (click your name, top right) and accept the Terms of Use if prompted.

**ERA5 dataset licence.** Each dataset has its own licence that must be accepted once:

1. Open the ERA5 dataset page:
   [ERA5 hourly data on single levels from 1940 to present](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels)
2. Click the **Download** tab.
3. Scroll to the bottom of the form to the **Terms of use** section and click **Accept**.

## 3. Copy your API token

The API token lets R request data on your behalf.

1. While logged in, open your user profile page (<https://cds.climate.copernicus.eu/profile>).
2. Find the **API Token** section and copy the token. It is a long string of letters, numbers and hyphens.

Treat the token like a password. Do not paste it into scripts you share or upload to GitHub.

## 4. Install R and RStudio

1. Install R from <https://cran.r-project.org>.
2. Install RStudio Desktop from <https://posit.co/download/rstudio-desktop/>.

If you already have R installed, check your version by running `R.version.string` in the console. Please update if it is older than R version *4.4.2 (2024-10-31 ucrt)*.

## 5. Install the required R packages

Open RStudio and run the following in the console:

```r
source("https://raw.githubusercontent.com/TODO-USERNAME/TODO-REPO/main/setup/install_packages.R")
```

<!-- TODO: replace TODO-USERNAME/TODO-REPO once the repository exists -->

Alternatively, download the workshop materials first (step 7), open `setup/install_packages.R` in RStudio and click **Source**.

The script installs any missing packages and ends by printing a short report. If every line reads `OK`, you are set. If a package fails to install, please send me the error message before the workshop.

## 6. Store your API token in R

Run the following in the RStudio console, pasting in the token from step 3:

```r
ecmwfr::wf_set_key(key = "PASTE-YOUR-TOKEN-HERE")
```

This stores the token in your computer's secure keychain, so you only do it once and never need to type it into a script. To check that it was saved, run:

```r
ecmwfr::wf_get_key()
```

which should print your token.

<!-- TODO: decide whether participants should rather paste the token into the Shiny app on the day -->

## 7. Download the workshop materials

You do not need a GitHub account or any knowledge of Git.

1. Go to <https://github.com/TODO-USERNAME/TODO-REPO>.
2. Click the green **Code** button, then **Download ZIP**.
3. Unzip the folder somewhere easy to find (for example, your Documents folder).

The materials may be updated shortly before the workshop, so please download them again the day before.

---

## Troubleshooting

| Problem | Likely cause |
|---|---|
| R download request fails with a message about licences or "required licences not accepted" | ERA5 licence not accepted (step 2) |
| Authentication error or "401" from the CDS | Token copied incorrectly, or an old pre-2024 CDS key is stored. Repeat steps 3 and 6 |


