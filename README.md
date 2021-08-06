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
When instantiating, default directories for specific file types can be provided with the `dirs` argument. 
The  recommended directory structure is as follows:
```r
list(export = "data/export", # All HUD CSV Exports
     public = "data/public", # All public data previously housed in COHHIO_HMIS/public_data
     spm = "data/spm", # All System performance measure data previously housed in COHHIO_HMIS/SPM_data
     extras = "data/extras") # All "Extras" previously housed in Rmisc
```
This directory structure will be used by **default** unless another is specified.

To instantiate the Looker connection:

```r
clarity_api = clarity.looker::clarity_api$new([PATH TO LOOKER.INI])
```

Individual HUD Export CSVs, and other Looks can be called using the similarly named method which takes the following arguments:
  - `look_type` `(character)` indicating the type of Look to be called. 
    - `"disk"` this will return the data already on disk, or it will fetch `"since2019"` from the API if that data does not exist.
    - `"since2019"` returns data since `2019-01-01` 
    - `"year2"` returns data for the last two complete years (not available for extras & spms)
    - `"daily"` returns data that has been added or modified in the last full midnight - midnight period
    - Additional `look_type`s can be created. More on this to come in future releases.
  - `path` `(character)` a path can be provided to save the data to a specific directory other than the default.
  - `.write` `(logical)` indicating whether to write the data to disk **Default** FALSE. **Note** that previous data will be overwritten if it already exists on the path provided.

  
```r
clarity_api$Clients()
clarity_api$Client_extras()
```

All Export items can be retrieved like so
```r
clarity_api$get_export()
```
