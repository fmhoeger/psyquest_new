# psyquest: PsychtestR Questionnaire Implementations

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://tidyverse.org/lifecycle/#maturing)
[![Travis build status](https://travis-ci.org/fmhoeger/psyquest.svg?branch=master)](https://travis-ci.org/fmhoeger/psyquest)
[![Coverage status](https://codecov.io/gh/fmhoeger/psyquest/branch/master/graph/badge.svg)](https://codecov.io/github/fmhoeger/psyquest?branch=master)
<!-- badges: end -->

This package contains a set of standard questionnaires as psychTestR models.

### Manual
For more detailed information about psyquest, an instruction, and examples see the "Introduction to psyquest": https://shiny.gold-msi.org/psyquest-documentation/

### Video tutorial
Watch our tutorial with an installation guide and an example: https://shiny.gold-msi.org/psyquest-tutorial/


## Citation

We also advise mentioning the software versions you used,
in particular the versions of the `psyquest`, and `psychTestR` packages.
You can find these version numbers from R by running the following commands:

``` r
library(psychTestR)
library(psyquest)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("psyquest", "psychTestR"), ]
```

## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install psyquest:

`devtools::install_github('fmhoeger/psyquest')`

## Usage

### Testing a participant

The `XYZ_standalone()` functions are  designed for real data collection.
In particular, the participant doesn't receive feedback during this version.

``` r
# Load the psyquest package
library(psyquest)

# Run the test as if for a participant, using default settings,
# saving data, and with a custom admin password
XYZ_standalone(admin_password = "put-your-password-here")
```
Replace 'XYZ' with one of the following three-letter acronyms:
* BMR (Barcelona Music Rewards Questionnaire)
* CCM (Concurrent Musical Activities)
* CMT (Competence focus in Music Teaching)
* DAC (Drama Activity)
* DEG (Demographics)
* GMS (Goldsmith Musical Sophistication Index)
* GRT (Short Grit Scale)
* HOP (Children’s Hope Scale)
* HSP (High Sensitivtiy Personality Scale)
* HUM (Healthy-Unhealthy Music Scale)
* MHE (Musical Home Environment)
* PAC (Physical Activity)
* PMS (Profile of Mood Scale)
* SDQ (Strengths and Difficulties Questionnaire (mental health))
* SEM (School Engagement Measurement)
* SES (Socio-economic Status)
* SOS (Student Opinion Scale)
* TOI (Theory of Intelligence)
* TOM (Theory of Musicality)
* TPI (Ten Item Personality Inventory)

You will need to enter a participant ID for each participant which  will be stored along with the participants' results.

Each time you test a new participant, rerun the `XYZ_standalone()` function,
and a new participation session will begin.

You can retrieve your data by starting up a participation session,
entering the admin panel using your admin password, and downloading your data.
For more details on the psychTestR interface, see http://psychtestr.com/.

`psyquest` currently supports English (EN) and German (DE).
You can select one of these languages by passing a language code as 
an argument to `XYZ_standalone()`, e.g. `XYZ_standalone(languages = "de")`,
or alternatively by passing it to the browser as a URL parameter, eg. http://127.0.0.1:4412/?language=DE (note that the `p_id` argument must be empty).

## Installation instructions (Shiny Server)

1. Complete the installation instructions for 'local use' described above.

2. If not already installed, install Open Source Shiny Server from

https://www.rstudio.com/products/shiny/download-server/

3. Navigate to the Shiny Server app directory.

`cd /srv/shiny-server`

4. Create a directory which will contain your new Shiny app:

`sudo mkdir psyquest`

5. Create a text file in this directory called `app.R` which specifies the R code to run the app.

- Open the `app.R` in a text editor.

`sudo nano psyquest/app.R`

Paste below code into it replacing 'XYZ' with the three-letter acronym of the corresponding questionnaire.

``` r
library(psyquest)
XYZ_standalone(admin_password = "put-your-password-here")
```

- Save the file (CTRL-O).

6. Change the permissions of your app directory to allow `psyquest`
to write its temporary files.

`sudo chown -R shiny psyquest`

where `shiny` is the username for the Shiny process user
(this is the usual default).

7. Navigate to your new shiny app, with a URL similar to
http://my-web-page.org:3838/psyquest

## Usage notes

- The psyquest participation sessions run in your web browser.
