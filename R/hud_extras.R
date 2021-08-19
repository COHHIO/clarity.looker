.hud_extras <-
  list(
    Client_extras = c(since2019 = 66252L),
    Enrollment_extras = c(since2019 = 66254L),
    Project_extras = c(since2019 = 66520L),
    User_extras = c(since2019 = 66523L),
    Client_COVID_extras = c(since2019 = 66546L),
    Client_Offer_extras = c(since2019 = 66719L),
    Services_extras = c(since2019 = 66721L),
    CE_Referrals_extras = c(since2019 = 66781L),
    Client_Doses_extras = c(since2019 = 66782L),
    Client_SPDAT_old_extras = c(since2019 = 66925L),
    Client_SPDAT_extras = c(since2019 = 66926L),
    Contact_extras = c(since2019 = 67774L)
  )
reconstruct_hud_extras <- function(configFile, look_folder = "HUD Extras", look_type = "since2019") {
  ca <- clarity.looker::clarity_api$new(configFile = configFile)
  search <- ca$api$folders("search", name = look_folder)
  purrr::map(search[[1]]$looks, ~.x[c("id", "title")]) |>
    {\(x) {setNames(x, purrr::map_chr(x, "title"))}}() |>
    purrr::map(~do.call(c,rlang::list2(!!look_type := .x$id)))
}


# Replaced by CurrentLivingSituation
#Contacts = 4,
# Removed as per card-63683974
# CaseManagers = 5,
# Offers = 7,
# Raw_services = 8,
# Services_funds = 9,
# Referrals = 10,
# Scores = 12,
# Removed as per https://github.com/orgs/COHHIO/projects/16#card-63683974
#Interims = 20
