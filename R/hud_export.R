


# hud_export object ----
# Mon Jul 19 16:05:14 2021
#' @title HUD Export saved values
#' @description The Look Ids for various filter criteria and the column specifications
#' @export

.hud_export <-
  list(
    Affiliation = list(
      look = c(
        year2 = 65501,
        s2020 = 65869,
        daily = 65914
      ),
      col_types = c(
        AffiliationID = "n",
        ProjectID = "n",
        ResProjectID = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Client = list(
      look = c(
        year2 = 65504,
        s2020 = 65870,
        daily = 65915
      ),
      col_types = c(
        PersonalID = "n",
        FirstName = "c",
        MiddleName = "c",
        LastName = "c",
        NameSuffix = "c",
        NameDataQuality = "n",
        SSN = "c",
        SSNDataQuality = "n",
        DOB = "D",
        DOBDataQuality = "n",
        AmIndAKNative = "n",
        Asian = "n",
        BlackAfAmerican = "n",
        NativeHIOtherPacific = "n",
        White = "n",
        RaceNone = "n",
        Ethnicity = "n",
        Gender = "n",
        VeteranStatus = "n",
        YearEnteredService = "n",
        YearSeparated = "n",
        WorldWarII = "n",
        KoreanWar = "n",
        VietnamWar = "n",
        DesertStorm = "n",
        AfghanistanOEF = "n",
        IraqOIF = "n",
        IraqOND = "n",
        OtherTheater = "n",
        MilitaryBranch = "n",
        DischargeStatus = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "c",
        DateDeleted = "T",
        ExportID = "n"
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
        EnrollmentID = "n",
        PersonalID = "n",
        InformationDate = "T",
        CurrentLivingSituation = "n",
        VerifiedBy = "c",
        LeaveSituation14Days = "n",
        SubsequentResidence = "n",
        ResourcesToObtain = "n",
        LeaseOwn60Day = "n",
        MovedTwoOrMore = "n",
        LocationDetails = "c",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "c",
        DateDeleted = "T",
        ExportID = "c"
      )
    ),
    Disabilities = list(
      look = c(
        year2 = 65508,
        s2020 = 65872,
        daily = 65919
      ),
      col_types = c(
        DisabilitiesID = "c",
        EnrollmentID = "n",
        PersonalID = "n",
        InformationDate = "D",
        DisabilityType = "n",
        DisabilityResponse = "n",
        IndefiniteAndImpairs = "n",
        TCellCountAvailable = "n",
        TCellCount = "n",
        TCellSource = "n",
        ViralLoadAvailable = "n",
        ViralLoad = "n",
        ViralLoadSource = "n",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
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
        EmploymentEducationID = "c",
        EnrollmentID = "n",
        PersonalID = "n",
        InformationDate = "D",
        LastGradeCompleted = "n",
        SchoolStatus = "n",
        Employed = "n",
        EmploymentType = "n",
        NotEmployedReason = "n",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Enrollment = list(
      look = c(
        year2 = 65514,
        s2020 = 65875,
        daily = 65921
      ),
      col_types = c(
        EnrollmentID = "n",
        PersonalID = "n",
        ProjectID = "n",
        EntryDate = "D",
        HouseholdID = "c",
        RelationshipToHoH = "n",
        LivingSituation = "n",
        LengthOfStay = "n",
        LOSUnderThreshold = "l",
        PreviousStreetESSH = "n",
        DateToStreetESSH = "D",
        TimesHomelessPastThreeYears = "n",
        MonthsHomelessPastThreeYears = "n",
        DisablingCondition = "n",
        DateOfEngagement = "D",
        MoveInDate = "D",
        DateOfPATHStatus = "D",
        ClientEnrolledInPATH = "n",
        ReasonNotEnrolled = "n",
        WorstHousingSituation = "n",
        PercentAMI = "n",
        LastPermanentStreet = "c",
        LastPermanentCity = "c",
        LastPermanentState = "c",
        LastPermanentZIP = "n",
        AddressDataQuality = "n",
        DateOfBCPStatus = "D",
        EligibleForRHY = "n",
        ReasonNoServices = "n",
        RunawayYouth = "n",
        SexualOrientation = "n",
        SexualOrientationOther = "c",
        FormerWardChildWelfare = "n",
        ChildWelfareYears = "n",
        ChildWelfareMonths = "n",
        FormerWardJuvenileJustice = "n",
        JuvenileJusticeYears = "n",
        JuvenileJusticeMonths = "n",
        UnemploymentFam = "n",
        MentalHealthIssuesFam = "n",
        PhysicalDisabilityFam = "n",
        AlcoholDrugAbuseFam = "n",
        InsufficientIncome = "n",
        IncarceratedParent = "n",
        ReferralSource = "n",
        CountOutreachReferralApproaches = "n",
        UrgentReferral = "n",
        TimeToHousingLoss = "n",
        ZeroIncome = "n",
        AnnualPercentAMI = "n",
        FinancialChange = "n",
        HouseholdChange = "n",
        EvictionHistory = "n",
        SubsidyAtRisk = "n",
        LiteralHomelessHistory = "n",
        DisabledHoH = "n",
        CriminalRecord = "n",
        SexOffender = "n",
        DependentUnder6 = "n",
        SingleParent = "n",
        HH5Plus = "n",
        IraqAfghanistan = "n",
        FemVet = "n",
        HPScreeningScore = "n",
        ThresholdScore = "n",
        VAMCStation = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
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
        EnrollmentCoCID = "c",
        EnrollmentID = "n",
        HouseholdID = "c",
        ProjectID = "n",
        PersonalID = "n",
        InformationDate = "D",
        CoCCode = "c",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Event = list(
      look = c(
        year2 = 65516,
        s2020 = 65877,
        daily = 65923
      ),
      col_types = c(
        EventID = "n",
        EnrollmentID = "n",
        PersonalID = "n",
        EventDate = "D",
        Event = "n",
        ProbSolDivRRResult = "n",
        ReferralCaseManageAfter = "n",
        LocationCrisisOrPHHousing = "c",
        ReferralResult = "D",
        ResultDate = "T",
        DateCreated = "T",
        DateUpdated = "c",
        UserID = "T",
        DateDeleted = "c",
        ExportID = "c"
      )
    ),
    Exit = list(
      look = c(
        year2 = 65512,
        s2020 = 65874,
        daily = 65924
      ),
      col_types = c(
        ExitID = "n",
        EnrollmentID = "n",
        PersonalID = "n",
        ExitDate = "D",
        Destination = "n",
        OtherDestination = "c",
        HousingAssessment = "n",
        SubsidyInformation = "n",
        ProjectCompletionStatus = "n",
        EarlyExitReason = "n",
        ExchangeForSex = "n",
        ExchangeForSexPastThreeMonths = "n",
        CountOfExchangeForSex = "n",
        AskedOrForcedToExchangeForSex = "n",
        AskedOrForcedToExchangeForSexPastThreeMonths = "n",
        WorkPlaceViolenceThreats = "n",
        WorkPlacePromiseDifference = "n",
        CoercedToContinueWork = "n",
        LaborExploitPastThreeMonths = "n",
        CounselingReceived = "n",
        IndividualCounseling = "n",
        FamilyCounseling = "n",
        GroupCounseling = "n",
        SessionCountAtExit = "n",
        PostExitCounselingPlan = "n",
        SessionsInPlan = "n",
        DestinationSafeClient = "n",
        DestinationSafeWorker = "n",
        PosAdultConnections = "n",
        PosPeerConnections = "n",
        PosCommunityConnections = "n",
        AftercareDate = "D",
        AftercareProvided = "n",
        EmailSocialMedia = "n",
        Telephone = "n",
        InPersonIndividual = "n",
        InPersonGroup = "n",
        CMExitReason = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Export = list(
      look = c(
        year2 = 65961,
        s2020 = 65962,
        daily = 65963
      ),
      col_types = c(
        ExportID = "n",
        SourceType = "n",
        SourceID = "n",
        SourceName = "c",
        SourceContactFirst = "c",
        SourceContactLast = "c",
        SourceContactPhone = "c",
        SourceContactExtension = "n",
        SourceContactEmail = "c",
        ExportDate = "T",
        ExportStartDate = "D",
        ExportEndDate = "D",
        SoftwareName = "c",
        SoftwareVersion = "c",
        ExportPeriodType = "n",
        ExportDirective = "n",
        HashStatus = "n"
      )
    ),
    Funder = list(
      look = c(
        year2 = 65949,
        s2020 = 65948,
        daily = 65925
      ),
      col_types = c(
        FunderID = "n",
        ProjectID = "n",
        Funder = "n",
        OtherFunder = "c",
        GrantID = "c",
        StartDate = "D",
        EndDate = "D",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "c",
        DateDeleted = "T",
        ExportID = "n"
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
        HealthAndDVID = "c",
        EnrollmentID = "n",
        PersonalID = "n",
        InformationDate = "D",
        DomesticViolenceVictim = "n",
        WhenOccurred = "n",
        CurrentlyFleeing = "n",
        GeneralHealthStatus = "n",
        DentalHealthStatus = "n",
        MentalHealthStatus = "n",
        PregnancyStatus = "n",
        DueDate = "D",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
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
        IncomeBenefitsID = "c",
        EnrollmentID = "n",
        PersonalID = "n",
        InformationDate = "D",
        IncomeFromAnySource = "n",
        TotalMonthlyIncome = "n",
        Earned = "n",
        EarnedAmount = "n",
        Unemployment = "n",
        UnemploymentAmount = "n",
        SSI = "n",
        SSIAmount = "n",
        SSDI = "n",
        SSDIAmount = "n",
        VADisabilityService = "n",
        VADisabilityServiceAmount = "n",
        VADisabilityNonService = "n",
        VADisabilityNonServiceAmount = "n",
        PrivateDisability = "n",
        PrivateDisabilityAmount = "n",
        WorkersComp = "n",
        WorkersCompAmount = "n",
        TANF = "n",
        TANFAmount = "n",
        GA = "n",
        GAAmount = "n",
        SocSecRetirement = "n",
        SocSecRetirementAmount = "n",
        Pension = "n",
        PensionAmount = "n",
        ChildSupport = "n",
        ChildSupportAmount = "n",
        Alimony = "n",
        AlimonyAmount = "n",
        OtherIncomeSource = "n",
        OtherIncomeAmount = "n",
        OtherIncomeSourceIdentify = "c",
        BenefitsFromAnySource = "n",
        SNAP = "n",
        WIC = "n",
        TANFChildCare = "n",
        TANFTransportation = "n",
        OtherTANF = "n",
        OtherBenefitsSource = "n",
        OtherBenefitsSourceIdentify = "c",
        InsuranceFromAnySource = "n",
        Medicaid = "n",
        NoMedicaidReason = "n",
        Medicare = "n",
        NoMedicareReason = "n",
        SCHIP = "n",
        NoSCHIPReason = "n",
        VAMedicalServices = "n",
        NoVAMedReason = "n",
        EmployerProvided = "n",
        NoEmployerProvidedReason = "n",
        COBRA = "n",
        NoCOBRAReason = "n",
        PrivatePay = "n",
        NoPrivatePayReason = "n",
        StateHealthIns = "n",
        NoStateHealthInsReason = "n",
        IndianHealthServices = "n",
        NoIndianHealthServicesReason = "n",
        OtherInsurance = "n",
        OtherInsuranceIdentify = "c",
        HIVAIDSAssistance = "n",
        NoHIVAIDSAssistanceReason = "n",
        ADAP = "n",
        NoADAPReason = "n",
        ConnectionWithSOAR = "n",
        DataCollectionStage = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Inventory = list(
      look = c(
        year2 = 65519,
        s2020 = 65880,
        daily = 65928
      ),
      col_types = c(
        InventoryID = "n",
        ProjectID = "n",
        CoCCode = "c",
        HouseholdType = "n",
        Availability = "n",
        UnitInventory = "n",
        BedInventory = "n",
        CHVetBedInventory = "n",
        YouthVetBedInventory = "n",
        VetBedInventory = "n",
        CHYouthBedInventory = "n",
        YouthBedInventory = "n",
        CHBedInventory = "n",
        OtherBedInventory = "n",
        ESBedType = "n",
        InventoryStartDate = "D",
        InventoryEndDate = "D",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "c",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Organization = list(
      look = c(
        year2 = 65525,
        s2020 = 65881,
        daily = 65930
      ),
      col_types = c(
        OrganizationID = "n",
        OrganizationName = "c",
        VictimServicesProvider = "n",
        OrganizationCommonName = "c",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "n",
        DateDeleted = "T",
        ExportID = "n"
      )
    ),
    Project = list(
      look = c(
        year2 = 65526,
        s2020 = 65882,
        daily = 65931
      ),
      col_types = c(
        ProjectID = "n",
        OrganizationID = "n",
        ProjectName = "c",
        ProjectCommonName = "c",
        OperatingStartDate = "D",
        OperatingEndDate = "D",
        ContinuumProject = "n",
        ProjectType = "n",
        HousingType = "n",
        ResidentialAffiliation = "n",
        TrackingMethod = "n",
        HMISParticipatingProject = "n",
        TargetPopulation = "n",
        PITCount = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "c",
        DateDeleted = "T",
        ExportID = "n"
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
        ProjectCoCID = "n",
        ProjectID = "n",
        CoCCode = "c",
        Geocode = "n",
        Address1 = "c",
        Address2 = "c",
        City = "c",
        State = "c",
        ZIP = "n",
        GeographyType = "n",
        DateCreated = "T",
        DateUpdated = "T",
        UserID = "c",
        DateDeleted = "T",
        ExportID = "n"
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
        DateDeleted = "T",
        DateUpdated = "T",
        UserEmail = "c",
        UserExtension = "n",
        UserFirstName = "c",
        UserID = "n",
        UserLastName = "c",
        UserPhone = "c"
      )
    )
  )

# Supporting functions ----
# Mon Jul 19 16:05:32 2021


hud_rename <- function(x, .nm) {
  if (is.null(x))
    return(NULL)
  x %>%
    dplyr::rename_with(.fn = ~ {
      # All column names are prefixed with the HUD CSV Export BETA report name from Looker - with spaces between capitalized words. This is removed
      out <-
        trimws(stringr::str_remove(.x, stringr::fixed(paste0(.nm, " ")))) %>%
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
  fetch(deparse(match.call()[[1]][[3]]),
        look_type,
        write,
        self$.__enclos_env__)
}


#' @title Call HUD Export Items from the Clarity Looker API
#' @description Calls the Clarity Looker HUD CSV Export  (BETA) API to return to the HUD Export Items on various time ranges via pre-constructed Looks.
#' @export
hud_export <- R6::R6Class(
  "hud_export",
  public = rlang::exec(
    rlang::list2,!!!purrr::map(.hud_export, ~ call_csv),
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
  private = list(item = .hud_export),
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
                  ee) {
  .y <- x
  .x <- ee$private$item[[x]]
  .nm <- .x$api_nm %||% .y

  if (look_type != "daily")
    data <-
    .x[[look_type]] %||% try(feather::read_feather(file.path("data", "API", paste0(.y, ".feather"))))

  if ((inherits(data, "try-error")) || look_type == "daily") {
    message(.y, ": fetching data")
    if (is.null(.x$look[look_type]))
      return(NULL)
    # Rename col_types to match the way they appear coming from the API
    names(.x$col_types) <- paste0(.nm, " ", names(.x$col_types))
    data <-
      ee$self$sdk$runLook(.x$look[look_type],
                          "csv",
                          as = "parsed",
                          col_types = .x$col_types)
    message(.y, ": data retrieved")
    if (write) {
      message(.y, "writing raw to feather")
      feather::write_feather(data, file.path("data", "API", paste0(.y, "_raw.feather")))
    }
    if (nrow(data) %in% c(0, 500))
      stop(.y, " row count is", nrow(data))
  }

  if (any(stringr::str_detect(names(data), paste0("^", .nm, "\\s")))) {
    data <- hud_rename(data, .nm)
  }

  if (write) {
    feather::write_feather(data, file.path("data", "API", paste0(.y, ".feather")))
  }

  return(data)
}

#' @title Write object to the *data* directory
#' @description Writes a \code{tibble/data.frame} as a feather file to the *data* directory using the name of the object as the file name.
#' @param x \code{(tibble/data.frame)} The object to write to feather
#' @param path \code{(character vector)} A character vector of the directory path to be passed to \link[base]{file.path}
#' @return A success message at the console
#' @export
to_feather <- function(x, path = "data") {
  fn <-
    rlang::exec(file.path,
                !!!path,
                !!!ifelse(
                  stringr::str_detect(path, "feather$"),
                  path,
                  paste0(deparse(rlang::enexpr(x)), ".feather")
                ))
  feather::write_feather(x, fn)
  cli::cli_alert_success(paste0(fn, " saved"))
}
