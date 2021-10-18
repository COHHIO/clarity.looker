# clarity.looker
Retrieve the HUD Export & other data from the Clarity Looker API

## Installation
`remotes::install_github("COHHIO/clarity.looker")`

## Setup
Connecting to the Looker API requires a Client ID and Client Secret which must be requested from Clarity support. Once received, `setup_looker` takes these as input and creates an ini file to store these credentials. See `?setup_looker` for additional details.
```r
setup_looker(
  client_id = [YOUR_CLIENT_ID],
  client_secret = [YOUR_CLIENT_SECRET]
)
```

## Usage
The `clarity_api` [R6](https://adv-r.hadley.nz/r6.html) object instantiates a connection to the Looker account associated with the credentials provided by Clarity by passing it the path to the *ini* file created in Setup. 
  - `export_folder` takes a folder name or Look ID corresponding to the name of the folder setup in Looker containing Looks for each of the Export CSV items. Each Look should be named according to it's corresponding Export item name
  - `daily_folder` takes a folder name or Look ID corresponding to the name of the folder setup in Looker containing Looks for each of the Export CSV Items filtered for data added or modified in the past 24 hours (12a - 12p)
  - `look_folders` takes arbitrary folder names or Look IDs for folders for which Look retrieval methods should be dynamically created. These will be made available via the method corresponding to the title of the look nested in a list with the folder name. IE if the folder name is `"Extras"`, and it has a Look entitled `"All"`, this will be called as follows: `[INITIALIZED R6 OBJECT NAME]$Extras$All()`
When instantiating, default directories for specific file types can be provided with the `dirs` argument. 
The  recommended directory structure is as follows:
```r
list(export = "data/export", # All HUD CSV Exports
     public = "data/public", # All public data previously housed in COHHIO_HMIS/public_data
     spm = "data/spm", # All System performance measure data previously housed in COHHIO_HMIS/SPM_data
     extras = "data/extras") # All "Extras" previously housed in Rmisc
```
This directory structure will be used by **default** unless another is specified.

To instantiate the Looker connection with default options:

```r
clarity_api = clarity.looker::clarity_api$new([PATH TO LOOKER.INI])
```

Individual HUD Export CSVs can be called via their named method. For details on usage see `?call_data`

For example, to call the `Clients` Export CSV
```r
clarity_api$Clients()
```

All Export items can be retrieved like so:
```r
clarity_api$get_export()
```
_Note_: `.write` defaults to `TRUE` for this method such that all Export CSVs are saved to the default directory specified by `dirs$export`, or any value passed to `path` explicitly.


### `Client_filter`
The `Client_filter` function allows for specific PersonalIDs in Clarity to be filtered from all endpoint data. This is useful when particular client profiles on the live Clarity instance are used for training and demonstration purposes. To enable client filtering, use `options` with a character vector of PersonalIDs. These options can be set permanently by adding the call to `.Rprofile`. See `usethis::edit_r_profile`. 

```r
options(clients_to_filter = c("335","1"))
```
