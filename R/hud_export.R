
# hud_export object ----
# Mon Jul 19 16:05:14 2021
hud_export <-
  list(
    Affiliation = list(
      look = c(
        year2 = 65501,
        s2020 = 65869,
        daily = 65914
      ),
      col_types = c(
        ProjectID = "n",
        ResProjectID = "c",
        UserID = "n",
        DateCreated = "T",
        DateUpdated = "T",
        DateDeleted = "l"
      )
    ),
    Client = list(
      look = c(
        year2 = 65504,
        s2020 = 65870,
        daily = 65915
      ),
      col_types = c(
        AfghanistanOEF = "n",
        AmIndAKNative = "n",
        Asian = "n",
        BlackAfAmerican = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        DesertStorm = "n",
        DischargeStatus = "n",
        DOB = "D",
        DOBDataQuality = "n",
        Ethnicity = "n",
        FirstName = "c",
        Gender = "n",
        IraqOIF = "n",
        IraqOND = "n",
        KoreanWar = "n",
        LastName = "c",
        MiddleName = "c",
        MilitaryBranch = "n",
        NameDataQuality = "n",
        NameSuffix = "c",
        NativeHIOtherPacific = "n",
        OtherTheater = "n",
        PersonalID = "n",
        RaceNone = "l",
        SSN = "c",
        SSNDataQuality = "n",
        UserID = "n",
        VeteranStatus = "n",
        VietnamWar = "n",
        White = "n",
        WorldWarII = "n",
        YearEnteredService = "n",
        YearSeparated = "n"
      )
    ),
    CurrentLivingSituation = list(
      look = c(
        year2 = 65506,
        s2020 = 65871,
        daily = 65918
      ),
      api_nm = "Current Living Situation",
      col_types = c(
        CurrentLivingSitID = "n",
        CurrentLivingSituation = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        EnrollmentID = "n",
        InformationDate = "D",
        LeaseOwn60Day = "l",
        LeaveSituation14Days = "l",
        LocationDetails = "c",
        MovedTwoOrMore = "l",
        PersonalID = "n",
        ResourcesToObtain = "l",
        SubsequentResidence = "l",
        UserID = "n",
        VerifiedBy = "l"
      )
    ),
    Disabilities = list(
      look = c(
        year2 = 65508,
        s2020 = 65872,
        daily = 65919
      ),
      col_types = c(
        DataCollectionStage = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        DisabilitiesID = "c",
        DisabilityResponse = "n",
        DisabilityType = "n",
        EnrollmentID = "n",
        IndefiniteAndImpairs = "n",
        InformationDate = "D",
        PersonalID = "n",
        TCellCount = "l",
        TCellCountAvailable = "l",
        TCellSource = "l",
        ViralLoadAvailable = "l",
        ViralLoad = "l",
        ViralLoadSource = "l",
        UserID = "n"
      )
    ),
    EmploymentEducation = list(
      look = c(
        year2 = 65509,
        s2020 = 65873,
        daily = 65920
      ),
      api_nm = "Employment Education",
      col_types = c(
        UserID = "n",
        SchoolStatus = "l",
        PersonalID = "n",
        NotEmployedReason = "n",
        LastGradeCompleted = "n",
        InformationDate = "D",
        EnrollmentID = "n",
        EmploymentType = "l",
        EmploymentEducationID = "c",
        Employed = "n",
        DateUpdated = "T",
        DateDeleted = "l",
        DateCreated = "T",
        DataCollectionStage = "n"
      )
    ),
    Enrollment = list(
      look = c(
        year2 = 65514,
        s2020 = 65875,
        daily = 65921
      ),
      col_types = c(
        AddressDataQuality = "n",
        AlcoholDrugAbuseFam = "l",
        AnnualPercentAMI = "n",
        ChildWelfareMonths = "l",
        ChildWelfareYears = "l",
        ClientEnrolledInPATH = "n",
        CountOutreachReferralApproaches = "l",
        ZeroIncome = "n",
        WorstHousingSituation = "l",
        VAMCStation = "n",
        UserID = "n",
        UrgentReferral = "n",
        UnemploymentFam = "l",
        TimeToHousingLoss = "n",
        TimesHomelessPastThreeYears = "n",
        ThresholdScore = "n",
        SubsidyAtRisk = "n",
        SingleParent = "n",
        SexualOrientationOther = "l",
        SexualOrientation = "n",
        SexOffender = "n",
        RunawayYouth = "l",
        `Reporting Period Start Date` = "D",
        `Reporting Period End Date` = "D",
        RelationshipToHoH = "n",
        ReferralSource = "l",
        ReasonNotEnrolled = "n",
        ReasonNoServices = "l",
        ProjectID = "n",
        PreviousStreetESSH = "n",
        PhysicalDisabilityFam = "l",
        PersonalID = "n",
        PercentAMI = "n",
        MoveInDate = "D",
        LOSUnderThreshold = "n",
        LivingSituation = "n",
        LiteralHomelessHistory = "n",
        MonthsHomelessPastThreeYears = "n",
        MentalHealthIssuesFam = "l",
        LengthOfStay = "n",
        LastPermanentZIP = "n",
        LastPermanentState = "l",
        LastPermanentCity = "c",
        LastPermanentStreet = "c",
        JuvenileJusticeYears = "l",
        JuvenileJusticeMonths = "l",
        IraqAfghanistan = "n",
        InsufficientIncome = "l",
        IncarceratedParent = "l",
        HPScreeningScore = "n",
        HouseholdID = "c",
        HouseholdChange = "n",
        HH5Plus = "n",
        FormerWardJuvenileJustice = "l",
        FormerWardChildWelfare = "l",
        FemVet = "n",
        FinancialChange = "n",
        EvictionHistory = "n",
        EntryDate = "D",
        DisablingCondition = "n",
        EligibleForRHY = "l",
        DisabledHoH = "n",
        DependentUnder6 = "n",
        DateUpdated = "T",
        DateToStreetESSH = "D",
        DateOfPATHStatus = "D",
        DateOfEngagement = "D",
        DateOfBCPStatus = "l",
        CriminalRecord = "n",
        DateCreated = "T",
        DateDeleted = "l",
        EnrollmentID = "n"
      )
    ),
    EnrollmentCoC = list(
      look = c(
        year2 = 65515,
        s2020 = 65876,
        daily = 65922
      ),
      api_nm = "Enrollment CoC",
      col_types = c(
        CoCCode = "c",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        EnrollmentCoCID = "c",
        EnrollmentID = "n",
        HouseholdID = "c",
        InformationDate = "D",
        PersonalID = "n",
        ProjectID = "n",
        UserID = "n"
      )
    ),
    Event = list(
      look = c(
        year2 = 65516,
        s2020 = 65877,
        daily = 65923
      ),
      col_types = c(
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        EnrollmentID = "n",
        Event = "n",
        EventDate = "D",
        EventID = "n",
        LocationCrisisOrPHHousing = "l",
        PersonalID = "n",
        ProbSolDivRRResult = "n",
        ReferralCaseManageAfter = "n",
        ReferralResult = "n",
        ResultDate = "D",
        UserID = "n"
      )
    ),
    Exit = list(
      look = c(
        year2 = 65512,
        s2020 = 65874,
        daily = 65924
      ),
      col_types = c(
        AftercareDate = "l",
        AftercareProvided = "l",
        AskedOrForcedToExchangeForSex = "l",
        AskedOrForcedToExchangeForSexPastThreeMonths = "l",
        CMExitReason = "l",
        CoercedToContinueWork = "l",
        CounselingReceived = "l",
        CountOfExchangeForSex = "l",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        Destination = "n",
        DestinationSafeClient = "l",
        DestinationSafeWorker = "l",
        EarlyExitReason = "l",
        EmailSocialMedia = "l",
        EnrollmentID = "n",
        ExchangeForSex = "l",
        ExchangeForSexPastThreeMonths = "l",
        ExitDate = "D",
        ExitID = "n",
        FamilyCounseling = "l",
        GroupCounseling = "l",
        HousingAssessment = "n",
        IndividualCounseling = "l",
        InPersonGroup = "l",
        InPersonIndividual = "l",
        LaborExploitPastThreeMonths = "l",
        OtherDestination = "c",
        PersonalID = "n",
        PosAdultConnections = "l",
        PosCommunityConnections = "l",
        PosPeerConnections = "l",
        PostExitCounselingPlan = "l",
        ProjectCompletionStatus = "l",
        SessionCountAtExit = "l",
        SessionsInPlan = "l",
        SubsidyInformation = "n",
        Telephone = "l",
        UserID = "n",
        WorkPlacePromiseDifference = "l",
        WorkPlaceViolenceThreats = "l"
      )
    ),
    Export = list(look = c(
      year2 = 65961,
      s2020 = 65962,
      daily = 65963
    )),
    Funder = list(
      look = c(
        year2 = 65949,
        s2020 = 65948,
        daily = 65925
      ),
      col_types = c(
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        EndDate = "D",
        Funder = "n",
        FunderID = "n",
        GrantID = "c",
        OtherFunder = "c",
        ProjectID = "n",
        StartDate = "D",
        UserID = "n"
      )
    ),
    HealthAndDV = list(
      look = c(
        year2 = 65517,
        s2020 = 65878,
        daily = 65926
      ),
      api_nm = "Health DV",
      col_types = c(
        CurrentlyFleeing = "n",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        DentalHealthStatus = "l",
        DomesticViolenceVictim = "n",
        DueDate = "l",
        EnrollmentID = "n",
        GeneralHealthStatus = "l",
        HealthAndDVID = "c",
        InformationDate = "D",
        MentalHealthStatus = "l",
        PersonalID = "n",
        PregnancyStatus = "n",
        UserID = "n",
        WhenOccurred = "n"
      )
    ),
    IncomeBenefits = list(
      look = c(
        year2 = 65518,
        s2020 = 65879,
        daily = 65927
      ),
      api_nm = "Income Benefits",
      col_types = c(
        ADAP = "l",
        Alimony = "n",
        AlimonyAmount = "l",
        BenefitsFromAnySource = "n",
        ChildSupport = "n",
        ChildSupportAmount = "n",
        COBRA = "n",
        ConnectionWithSOAR = "n",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        Earned = "n",
        EarnedAmount = "n",
        EmployerProvided = "n",
        EnrollmentID = "n",
        GA = "n",
        GAAmount = "n",
        HIVAIDSAssistance = "l",
        IncomeBenefitsID = "c",
        IncomeFromAnySource = "n",
        IndianHealthServices = "n",
        InformationDate = "D",
        InsuranceFromAnySource = "n",
        Medicaid = "n",
        Medicare = "n",
        NoCOBRAReason = "n",
        NoEmployerProvidedReason = "n",
        NoHIVAIDSAssistanceReason = "l",
        NoMedicaidReason = "n",
        NoIndianHealthServicesReason = "n",
        NoADAPReason = "l",
        NoMedicareReason = "n",
        NoPrivatePayReason = "n",
        NoSCHIPReason = "n",
        NoStateHealthInsReason = "n",
        NoVAMedReason = "n",
        OtherBenefitsSource = "n",
        OtherBenefitsSourceIdentify = "c",
        OtherIncomeAmount = "n",
        OtherIncomeSource = "n",
        OtherIncomeSourceIdentify = "c",
        OtherInsurance = "n",
        OtherInsuranceIdentify = "l",
        OtherTANF = "n",
        Pension = "n",
        PensionAmount = "n",
        PersonalID = "n",
        PrivateDisability = "n",
        PrivateDisabilityAmount = "l",
        PrivatePay = "n",
        SCHIP = "n",
        SNAP = "n",
        SocSecRetirement = "n",
        SocSecRetirementAmount = "n",
        SSDI = "n",
        SSDIAmount = "n",
        SSI = "n",
        SSIAmount = "n",
        StateHealthIns = "n",
        TANF = "n",
        TANFAmount = "n",
        TANFChildCare = "n",
        TANFTransportation = "n",
        TotalMonthlyIncome = "n",
        Unemployment = "n",
        UnemploymentAmount = "n",
        UserID = "n",
        VADisabilityNonService = "n",
        VADisabilityNonServiceAmount = "n",
        VADisabilityService = "n",
        VADisabilityServiceAmount = "n",
        VAMedicalServices = "n",
        WIC = "n",
        WorkersComp = "n",
        WorkersCompAmount = "l"
      )
    ),
    Inventory = list(
      look = c(
        year2 = 65519,
        s2020 = 65880,
        daily = 65928
      ),
      col_types = c(
        Availability = "n",
        BedInventory = "n",
        CHBedInventory = "n",
        CHVetBedInventory = "n",
        CHYouthBedInventory = "n",
        CoCCode = "c",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        ESBedType = "n",
        HouseholdType = "n",
        InventoryEndDate = "D",
        InventoryID = "n",
        InventoryStartDate = "D",
        OtherBedInventory = "n",
        ProjectID = "n",
        UnitInventory = "n",
        UserID = "n",
        VetBedInventory = "n",
        YouthBedInventory = "n",
        YouthVetBedInventory = "n"
      )
    ),
    Organization = list(
      look = c(
        year2 = 65525,
        s2020 = 65881,
        daily = 65930
      ),
      col_types = c(
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        OrganizationCommonName = "c",
        OrganizationID = "n",
        OrganizationName = "c",
        UserID = "c",
        VictimServicesProvider = "n"
      )
    ),
    Project = list(
      look = c(
        year2 = 65526,
        s2020 = 65882,
        daily = 65931
      ),
      col_types = c(
        ContinuumProject = "n",
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        HMISParticipatingProject = "n",
        HousingType = "n",
        OperatingEndDate = "D",
        OperatingStartDate = "D",
        OrganizationID = "n",
        PITCount = "l",
        ProjectCommonName = "c",
        ProjectID = "n",
        ProjectName = "c",
        ProjectType = "n",
        ResidentialAffiliation = "n",
        TargetPopulation = "n",
        TrackingMethod = "n",
        UserID = "n"
      )
    ),
    ProjectCoC = list(
      look = c(
        year2 = "65527",
        s2020 = "65883",
        daily = "65932",
        api_nm = "Project CoC"
      ),
      col_types = c(
        ZIP = "n",
        UserID = "n",
        State = "c",
        ProjectID = "n",
        ProjectCoCID = "n",
        GeographyType = "n",
        Geocode = "n",
        DateUpdated = "T",
        DateDeleted = "l",
        CoCCode = "c",
        DateCreated = "T",
        City = "c",
        Address2 = "l",
        Address1 = "c"
      )
    ),
    Services = list(
      look = c(
        year2 = 65528,
        s2020 = 65884,
        daily = 65933
      ),
      col_types = character(0)
    ),
    User = list(
      look = c(
        year2 = 65529,
        s2020 = 65885,
        daily = 65934
      ),
      col_types = c(
        DateCreated = "T",
        DateDeleted = "l",
        DateUpdated = "T",
        UserEmail = "c",
        UserExtension = "l",
        UserFirstName = "c",
        UserID = "n",
        UserLastName = "c",
        UserPhone = "l"
      )
    )
  )

# Supporting functions ----
# Mon Jul 19 16:05:32 2021


hud_rename <- function(x, .nm) {
  if (is.null(x)) return(NULL)
  x %>%
    dplyr::rename_with(.fn = ~{
      # All column names are prefixed with the HUD CSV Export BETA report name from Looker - with spaces between capitalized words. This is removed
      out <- trimws(stringr::str_remove(.x, stringr::fixed(paste0(.nm, " ")))) %>%
        stringr::str_replace_all("(?<!a)[Ii][Dd]$", "ID") %>%
        stringr::str_remove("^Enrollment ") %>%
        stringr::str_replace_all("[Cc][Oo][Cc]", "CoC") %>%
        stringr::str_replace_all("^[Zz][Ii][Pp]$", "ZIP") %>%
        stringr::str_replace_all("(?<=rk)p(?=lace)", "P")

      if (all(is.na(out)))
        out <- .x
      out
    })
}

call_csv <- function(look_type = "year2", write = FALSE) {
  fetch(deparse(match.call()[[1]][[3]]), look_type, write, self$.__enclos_env__)
}


#' @title Call HUD Export Items from the Clarity Looker API
#' @description Calls the Clarity Looker HUD CSV Export  (BETA) API to return to the HUD Export Items on various time ranges via pre-constructed Looks.
#' @export
hud_export <- R6::R6Class(
  "hud_export",
  public = rlang::exec(
    rlang::list2,
    !!!purrr::map(hud_export, ~ call_csv),
    #' @description initialize the Looker API connection given the path to the ini configuration file.
    #' @param configFile \code{(character)} Path to the Looker *.ini* configuration file. Only the directory path is needed if the file is entitled *Looker.ini*
    initialize = function(configFile) {
      self$sdk <- lookr::LookerSDK$new(configFile = ifelse(
        stringr::str_detect(configFile, "ini$"),
        file.path(configFile),
        file.path(configFile, "Looker.ini")
      ))
    },
    #' @description Close the Looker API Connection
    close = function() {
      self$sdk$on_connection_closed()
    }
  ),
  lock_objects = FALSE,
  private = list(item = hud_export),
)


#' @title Retrieve data from disk or the API
#' @description Determines the appropriate location from which to retrieve HUD Export data
#' @param x \code{(character)} The HUD Export item to retrieve. One of:
#' \itemize{
#'   \item{Affiliation}
#'   \item{Client}
#'   \item{CurrentLivingSituation}
#'   \item{Disabilities}
#'   \item{EmploymentEducation}
#'   \item{Enrollment}
#'   \item{EnrollmentCoC}
#'   \item{Event}
#'   \item{Exit}
#'   \item{Export}
#'   \item{Funder}
#'   \item{HealthAndDV}
#'   \item{IncomeBenefits}
#'   \item{Inventory}
#'   \item{Organization}
#'   \item{Project}
#'   \item{ProjectCoC}
#'   \item{Services}
#'   \item{User}
#' }
#' @param look_type \code{(character)} The look type to retrieve. One of:
#' \itemize{
#'   \item{year2}{Two Complete Years}
#'   \item{s2020}{Since the beginning of 2020}
#'   \item{daily}{Created or updated in the last complete day}
#' }
#' @param write \code{(logical)} Whether to write the raw data from the API and the renamed data to the data/API folder
#' @return \code{(tibble)} The HUD Export item requested.

fetch <- function(x,
                  look_type = "year2",
                  write = FALSE,
                  ee
) {
  .y <- x
  .x <- ee$private$item[[x]]
  .nm <- .x$api_nm %||% .y

  if (look_type != "daily")
    data <- .x[[look_type]] %||% try(feather::read_feather(file.path("data", "API", paste0(.y, ".feather"))))

  if ((inherits(data, "try-error")) || look_type == "daily") {
    message(.y, ": fetching data")
    if (is.null(.x$look[look_type])) return(NULL)
    data <- ee$self$sdk$runLook(.x$look[look_type], "csv", as = "parsed", col_types = .x$col_types)
    if (write) {
      feather::write_feather(data, file.path("data", "API", paste0(.y, "_raw.feather")))
    }
    if (nrow(data) %in% c(0, 500)) stop(.y, " row count is", nrow(data))
  }

  if (any(stringr::str_detect(names(data), paste0("^",.nm,"\\s")))) {
    data <- hud_rename(data, .y)
  }

  if (write) {
    feather::write_feather(data, file.path("data", "API", paste0(.y, ".feather")))
  }

  return(data)
}

#' @title Write object to the *data* directory
#' @description Writes a \code{tibble/data.frame} as a feather file to the *data* directory using the name of the object as the file name.
#' @param x \code{(tibble/data.frame)} The object to write to feather
#' @param path \code{(character vector)} A character vector of the directory path to be passed to \link[base](file.path)
#' @return A success message at the console
#' @export
to_feather <- function(x, path = "data") {
  fn <- rlang::exec(file.path, !!!path, !!!ifelse(stringr::str_detect(path, "feather$"), path, paste0(deparse(rlang::enexpr(x)), ".feather")))
  feather::write_feather(x, fn)
  cli::cli_alert_success(paste0(fn, " saved"))
}
