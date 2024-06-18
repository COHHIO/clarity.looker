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

CE_Referrals_extras <- c("PersonalID", "UniqueID", "ReferringEnrollmentID", "ReferralID",
                        "ReferralConnectedEnrollmentsID", "ExitUpdatedTime", "LastUpdated",
                        "ReferralEndDate", "ReferringAgency", "ReferringProjectID",
                        "ReferredProjectID", "ReferringProjectName", "ReferredProjectName",
                        "ReferringPTC", "ReferredPTC", "ReferralConnectedProjectName",
                        "ReferralConnectedProjectType", "ReferringHouseholdID",
                        "ReferralConnectedHouseholdID", "ReferredDate", "ReferralAcceptedDate",
                        "ReferralCurrentlyOnQueue",
                        "WasReferral", "ExitDestination", "ExitHoused", "ReferralConnectedMoveInDate",
                        "DaysInQueue", "RemovedReason","RemovedSubReason", "Reassigned",
                        "ReassignedDate", "ReferralDaysElapsed", "DeniedInfo",
                        "DeniedType", "DeniedReason", "ExitAuto", "ActiveInProject",
                        "IsLastEnrollment", "IsLastReferral", "ReferralResult")

CE_Referrals_new_extras <- c("PersonalID", "UniqueID", "EnrollmentID", "ReferralID",
                             "ExitUpdatedTime", "LastUpdated", "ReferralEndDate",
                             "ReferringAgency", "ReferringProjectID", "ReferredProjectID",
                             "ReferringProjectName", "ReferredProjectName", "ReferringPTC",
                             "ReferredPTC", "ReferringHouseholdID", "DateReferred",
                             "DateReferralAccepted", "IsCurrentlyOnQueue", "ExitDestination",
                             "ExitHoused", "DaysInQueue", "RemovedReason", "RemovedSubReason",
                             "Reassigned", "ReassignedDate", "ReferralDaysElapsed",
                             "DeniedInfo", "DeniedType", "DeniedReason", "ExitAuto", "ActiveInProject",
                             "IsLastEnrollment", "IsLastReferral", "ReferralResult")

Client_COVID_extras <- c("PersonalID", "UniqueID", "Deleted", "C19AssessmentDate",
                         "C19ScreeningDate", "C19Tested", "C19TestDate", "C19TestResults",
                         "C19UnderInvestigation", "C19InvestigationDate", "C19ContactWithIll",
                         "C19ContactWithIllDate", "C19ContactWithConfirmed", "C19ContactwithConfirmedDate",
                         "C19HealthNotes", "HROver65", "HRSmoke", "HRImmunocompromised",
                         "HRHistoryofRespiratoryIllness", "HRChronicIllness", "HRKidneyDisease",
                         "Symptom2Fever", "Symptom1Cough", "Symptom2SoarThroat",
                         "Symptom1BreathingDifficult", "Congestion", "Chills", "Nausea",
                         "Weak", "MusclePain", "Diarrhea", "LostTasteSmell", "Headache")

Client_Doses_extras <- c("PersonalID", "UniqueID", "EnrollmentID", "C19AssessmentDate",
                         "C19VaccineConsent", "C19VaccineConcerns",
                         "C19VaccineManufacturer", "Documentation", "Deleted")

Client_extras <- c("UniqueID", "PersonalID", "EnrollmentID", "DateVeteranIdentified",
                   "PHTrack", "ExpectedPHDate", "HOMESID", "ListStatus", "VAEligible",
                   "SSVFIneligible", "ConsentToVaccine", "VaccineConcerns")

Client_MentalHealth_extras <- c("UniqueID", "PersonalID", "ScoreDate", "Deleted",
                                "MentalHealth", "MentalHealthLongTerm", "MentalHealthServices",
                                "SubstanceAbuseServices", "SubstanceAbuse", "SubstanceAbuseLongTerm")

Client_Offer_extras <- c("UniqueID", "PersonalID","AcceptDeclineDate", "OfferAccepted",
                         "PHTypeOffered", "OfferDate")

Client_SPDAT_extras <- c("UniqueID", "PersonalID", "ScoreDate", "Score", "CustomScore", "Deleted", "Assessment", "Total")

Client_UniqueID_extras <- c("UniqueID", "PersonalID")

Contact_extras <- c("UniqueID", "PersonalID", "EnrollmentID", "CurrentLivingSituation",
                    "ProgramName", "ContactDate", "LocationDetails")

Enrollment_extras <- c("PersonalID", "EnrollmentID", "UserCreating", "CountyServed",
                       "CountyPrior", "LastPermanentAddress")

Program_lookup_extras <- c("ProgramID", "ProgramName", "ProgramActive", "ProjectType",
                           "ParticipationStatus", "AgencyID", "AgencyName", "AgencyActive",
                           "PropertyManager", "StartDate", "EndDate", "LastUpdatedDate")

Project_extras <- c("ProjectID", "ProjectName", "ProjectTypeCode", "Website",
                    "Phone", "Hours", "APCountiesGeneral", "APCountiesVeteran",
                    "APCountiesYouth", "CoCCompDocsReceived", "CoCCompChronicPrioritization",
                    "CoCCompCostPerExit", "CoCCompHousingFirst", "CoCCompOnTrackSpending",
                    "CoCCompUnspentFunds", "Geocode", "Address", "Address2", "City",
                    "ZIP", "FundingSourceCode", "NonFederalFundingSourceCode",
                    "OrganizationName", "FundingSourceID", "ProgramCoC")

Services_extras <- c("UniqueID", "PersonalID", "ServiceID", "ServiceItemID",
                    "HouseholdID", "EnrollmentID", "ServiceStartDate",
                    "ServiceEndDate", "ServiceItemName", "FundName", "ServiceAmount",
                    "FundingSourceID")

UserNamesIDs_extras <- c("UserCreated", "UserCreatedText")

User_extras <- c("UserID", "Deleted", "ProjectID", "ProjectName")




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
