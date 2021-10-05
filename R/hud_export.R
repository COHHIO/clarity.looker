
.hud_category <- list(Export = "Export",
                 Project = c("Organization", "User", "Project", "Funder", "ProjectCoC","Inventory", "Affiliation"),
                 Client = "Client",
                 Enrollment = c("Enrollment", "EnrollmentCoC", "Exit", "IncomeBenefits", "HealthAndDV", "EmploymentEducation", "Disabilities", "Services", "CurrentLivingSituation", "Assessment", "AssessmentQuestions",  "AssessmentResults", "Event", "YouthEducationStatus"))


# hud_export object ----
# Mon Jul 19 16:05:14 2021
#' @title HUD Export saved values
#' @description The Look Ids for various filter criteria and the column specifications
#' @export

.hud_export <- source(paste0("R/hud_export_",2022,".R"))$value
