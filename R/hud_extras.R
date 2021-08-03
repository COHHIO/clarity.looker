.hud_extras <- list(
  Project_extras = list(look = c(since2019 = 66520)),
  Enrollment_extras = list(look = c(since2019 = 66254)),
  Client_COVID_extras = list(look = c(since2019 = 66252, daily = 66546)),
  User_extras = list(look = NULL),
  # Replaced by CurrentLivingSituation
  #Contacts = 4,
  # Removed as per card-63683974
  # CaseManagers = 5,
  Offers = 7,
  Raw_services = 8,
  Services_funds = 9,
  Referrals = 10,
  Scores = 12,
  # Removed as per https://github.com/orgs/COHHIO/projects/16#card-63683974
  #Interims = 20
  CE_Referrals_extras = list(look = c(since2019 = 66781)),
  Client_Doses_extras = list(look = c(since2019 = 66782)),
  Client_extras = list(look = c(since2019 = 66252)),
  Client_Offer_extras = list(look = c(since2019 = 66719)),
  Services_extras = list(look = c(since2019 = 66721))
)


# purrr::imap_chr(Rmisc2, ~{
#   paste0(" - [ ] Rmisc Sheet ",.x,": ",.y)
# }) %>% cat(sep = "\n")
