.hud_extras <- c(
  # Formerly Provider_extras, provider_geo, provider_tel, provider_services
  Project_extras = list(
    look = c(s2019 = 66520)
  ),
  # Formerly Counties
  Enrollment_extras = list(
    look = c(s2019 = 66254)
  ),
  # Formerly VeteranCE
  Client_COVID_extras = list(
    look = c(s2019 = 66252,
             daily = 66546)
  ),
  User_extras = list(
    look = c(s2019 = )
  ),
  # Replaced by CurrentLivingSituation
  #Contacts = 4,
  # Removed as per card-63683974
  # CaseManagers = 5,
  Covid19 = 6,
  Offers = 7,
  Raw_services = 8,
  Services_funds = 9,
  Referrals = 10,
  Scores = 12,
  # Removed as per https://github.com/orgs/COHHIO/projects/16#card-63683974
  #Interims = 20,
  Doses = 21
)

# purrr::imap_chr(Rmisc2, ~{
#   paste0(" - [ ] Rmisc Sheet ",.x,": ",.y)
# }) %>% cat(sep = "\n")
