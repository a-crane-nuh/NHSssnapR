#' row wise numerator and denominator for pathway based key indicators
#'
#' @param df As the import dataset. This dataset should be created from the
#'    import_and_cleanse fun
#'
#' @returns a dataframe of per row numerator and denominator observations
#'    with primary keys to support additional analysis
#' @export
#'
#' @examples
#' \dontrun{
#' derive_num_denom_flags(df)
#' }
derive_num_denom_flags <- function(df) {

  if (is.null(df)) {
    stop("Oops, did you forget to add a df", call. = FALSE)
  }
  if (!is.data.frame(df)) {
    stop("input must be a data.frame", call. = FALSE)
  }
  # suspect dataframe. this breaks testthat. think of alternative
  # if (ncol(df) != 417) {
  #   stop("data.frame not in the expected SSNAP format")
  #   }
# ---- base data ----
output <- with(df, {
  SiteCode
  SiteName
  S01PatientId
  S01ClockStartTime
  S07DepartureDate
  d <- 1
  dc <- as.integer(!is.na(S07DepartureDate))

  data.frame(SiteCode
             ,SiteName
             ,S01PatientId
             ,S01ClockStartTime
             ,S07DepartureDate
             ,d
             ,dc)
  })

## Domain 1 ----
DomainOne <- with(df, {
  StartTime <- S01ClockStartTime
  KI_1_1EndTime <- S02BrainImagingDateTime
  KI_1_1Duration <- difftime(KI_1_1EndTime, StartTime, units = "secs")
  DuringSleep <- as.integer(S01OnsetDateType == "DS")
  NotKnown <- as.integer(S01OnsetTimeType == "NK")
  DsOrNk <- as.integer(rowSums(cbind(DuringSleep, NotKnown), na.rm = TRUE) >0)
  CtaCtpOrMri <- as.integer(rowSums(
    cbind(
        S02BrainImagingCTIntercranial
        ,S02BrainImagingCTPerfusion
        ,S02BrainImagingPlainMRI
        )
      ,na.rm = TRUE) > 0)
  KI_1_5EndTime <- S03FirstClinicanAssessment
  KI_1_5Duration <- difftime(KI_1_5EndTime, StartTime, units = "secs")
  KI_1_6EndTime <- S03StrokeNurseAssessedDateTime
  KI_1_6Duration <- difftime(KI_1_6EndTime, StartTime, units = "secs")
  Eligible <- as.integer(!S02SwallowScreening4HrsNotPerformedReason %in% c("PU", "PR"))
  Assessed <- as.integer(S02SwallowScreening4HrsNotPerformed == 0)
  EligibleAssessed <- as.integer(rowSums(cbind(Eligible, Assessed), na.rm = TRUE) >1)

  data.frame(KI_1_1n = as.integer(KI_1_1Duration < 1201)
             ,KI_1_2n = as.numeric(KI_1_1Duration)
             ,KI_1_3n = S02BrainImagingCTIntercranial
             ,KI_1_4d = DsOrNk
             ,KI_1_4n = as.integer(rowSums(
               cbind(
                 DsOrNk
                 ,CtaCtpOrMri
                 )
               , na.rm = TRUE) > 1)
             ,KI_1_5n = as.integer(KI_1_5Duration < 3601)
             ,KI_1_5act = as.numeric(KI_1_5Duration)
             ,KI_1_6n = as.integer(KI_1_6Duration < 14401)
             ,KI_1_6act = as.numeric(KI_1_6Duration)
             ,KI_1_7d = Eligible
             ,KI_1_7n = EligibleAssessed
             )
  })

output[colnames(DomainOne)] <- DomainOne

## Domain 2 ----
DomainTwo <- with(df, {
  dc = as.integer(!is.na(S07DepartureDate))
  ICH = as.integer(S01FirstWard == "ICH")
  MT = as.integer(S02IAI == 1)
  Referral = as.integer(S02IAIReferral %in% c("YTT", "YAT", "YD"))
  Exclude = as.integer(rowSums(
    cbind(ICH
          , MT, Referral)
    , na.rm = TRUE) >0)
  StartTime = S01ClockStartTime
  KI_2_1EndTime = S01FirstStrokeUnitArrivalDateTime
  KI_2_1Duration <- ifelse(Exclude == 0,
                           difftime(KI_2_1EndTime, StartTime, units = "secs"),
                           NA)
  EndTime = S07DepartureDate
  Duration = ifelse(Exclude == 0, as.numeric(difftime(EndTime, StartTime, units = "secs")), NA)
  StartTime2 =  S01FirstStrokeUnitArrivalDateTime
  EndTime2 = S07StrokeUnitDischargeDateTime
  Duration2 = ifelse(Exclude == 0, difftime(EndTime2, StartTime2, units = "secs"), NA)
  SpellDuration90 = as.integer(Duration2 / Duration > .899)
  ESD = as.integer(S07DischargedESD %in% c("SNS", "NS"))
  ESDmt = as.integer(S07DischargedEsdmt %in% c("SNS", "NS"))
  Mcrt = as.integer(S07DischargedMcrt %in% c("SNS", "NS"))
  EsdCrt = as.integer(rowSums(cbind(ESD, ESDmt, Mcrt), na.rm = TRUE) > 0)
  EsdCrtd = as.integer(!S07DischargeType %in% c("D", "T", "TN"))

  data.frame(KI_2_1n = as.integer(KI_2_1Duration < 14401)
             ,KI_2_1d = as.integer(Exclude == 0)
             ,KI_2_2 = KI_2_1Duration
             ,KI_2_3n = ifelse(Exclude == 1, NA, ifelse(dc == 1, SpellDuration90, NA))
             ,KI_2_3d = ifelse(Exclude == 1, NA, dc)
             ,KI_2_4n = ifelse(dc == 1, EsdCrt, NA)
             ,KI_2_4d = ifelse(dc == 1, EsdCrtd, NA)
             )
  })

output[colnames(DomainTwo)] <- DomainTwo

## Domain 3 ----

DomainThree <- with(df, {
  KI_3_1n = as.integer(S02Thrombolysis == "Y")
  StartTime = S01ClockStartTime
  EndTime = ifelse(S01OnsetInHospital == 1, NA, S01FirstArrivalDateTime)
  Duration = as.numeric(difftime(EndTime, StartTime, units = "secs"))
  DateTimeType = as.integer(S01OnsetDateType %in% c("P", "BE") &
                              S01OnsetTimeType %in% c("P", "BE"))
  OnsetToArrival = as.numeric(difftime(StartTime, S01OnsetDateTime, units = "secs"))
  Within4Hours = ifelse(DateTimeType == 1 &
                          OnsetToArrival < 14401, 1, 0)
  between_4_8half_hours = as.integer(Duration > 14399 &
                                       Duration < 30661)
  ExtendedHours = as.integer(rowSums(cbind(DateTimeType, between_4_8half_hours), na.rm = TRUE) >
                               1)
  NotKnownSleep = as.integer(S01OnsetDateType == "DS" |
                               S01OnsetTimeType == "NK")
  ExtendedOrSleep = as.integer(ExtendedHours > 0 | NotKnownSleep > 0)
  KI_3_2n = as.integer(rowSums(cbind(KI_3_1n, ExtendedOrSleep), na.rm = TRUE) >
                         1)
  DtnEndTime = S02ThrombolysisDateTime
  DtnDuration = as.numeric(difftime(DtnEndTime, StartTime, units = "secs"))
  KI_3_5n = as.integer(S02IAI > 0)
  DiDoEndTime = S02IAIReferralAmbulanceDepartDateTime
  DiDoDuration = as.numeric(difftime(DiDoEndTime, StartTime, units = "secs"))
  DtpStartTime = S04ArrivalDateTime
  DtpEndTime = S02IAIArterialPunctureDateTime
  ## unusually, negative times happen. source data checked. most common in old dataset. Raise with SSNAP?
  ## review for other sites... only a HUB site can calculate this (data n/a unless SSNAP PID available)
  DtpDuration = ifelse(as.numeric(difftime(DtpEndTime, DtpStartTime, units = "secs")) <
                         0, NA
                       , as.numeric(difftime(DtpEndTime, DtpStartTime, units = "secs")))

  data.frame(KI_3_1n = KI_3_1n
             ,KI_3_OnsetToArrival = Duration
             ,KI_3_2n = KI_3_2n
             ,KI_3_2d = ExtendedOrSleep
             ,KI_3_3n = as.integer(rowSums(
               cbind(
                 KI_3_1n
                 , Within4Hours
                 ), na.rm = TRUE
               ) > 1)
             ,KI_3_3d = Within4Hours
             ,KI_3_4n = DtnDuration
             ,KI_3_5n
             ,KI_3_6n = DiDoDuration
             ,KI_3_7n = DtpDuration
             )
  })

output[colnames(DomainThree)] <- DomainThree

## Domain 4 ----

DomainFour <- with(df, {
  StartTime = S01ClockStartTime
  KI_4_1_EndTime = S03StrokeConsultantFirstContactDateTime
  KI_4_1_Duration = as.numeric(difftime(KI_4_1_EndTime, StartTime, units ="secs"))
  KI_4_2_Applicable = as.integer(!S03SpLangTherapistSwallow72HrsNotAssessedReason %in% c("PR", "PU", "PS"))
  KI_4_2_EndTime = pmin(
    S03SpLangTherapistSwallow72HrsDateTime,
    S06SpLangTherapistSwallowByDischargeDateTime,
    na.rm = TRUE
  )
  KI_4_2_Duration = as.numeric(difftime(KI_4_2_EndTime, StartTime, units = "secs"))
  KI_4_3_Applicable = as.integer(!S03OccTherapist72HrsNotAssessedReason %in%
                                   c("PR", "PU", "ND"))
  KI_4_3_EndTime = pmin(S03OccTherapist72HrsDateTime,
                        S06OccTherapistByDischargeDateTime,
                        na.rm = TRUE)
  KI_4_3_Duration = as.numeric(difftime(KI_4_3_EndTime, StartTime, units =  "secs"))
  KI_4_4_Applicable = as.integer(!S03Physio72HrsNotAssessedReason %in% c("PR", "PU", "ND"))
  KI_4_4_EndTime = pmin(S03Physio72HrsDateTime,
                        S06PhysioByDischargeDateTime,
                        na.rm = TRUE)
  KI_4_4_Duration = as.numeric(difftime(KI_4_4_EndTime, StartTime, units = "secs"))
  KI_4_5_Applicable = as.integer(!S03SpLangTherapistComm72HrsNotAssessedReason %in%
                                   c("PR", "PU", "ND"))
  KI_4_5_EndTime = pmin(
    S03SpLangTherapistComm72HrsDateTime,
    S06SpLangTherapistCommByDischargeDateTime,
    na.rm = TRUE
  )
  KI_4_5_Duration = as.numeric(difftime(KI_4_5_EndTime, StartTime, units = "secs"))

  data.frame(
    S01ClockStartTime
    ,KI_4_1n = as.integer(KI_4_1_Duration < 50401)
    ,KI_4_2n = as.integer(rowSums(
      cbind(KI_4_2_Applicable
            , as.integer(KI_4_2_Duration < 86401)
            )
      , na.rm = TRUE) > 1)
    ,KI_4_2d = KI_4_2_Applicable
    ,KI_4_3n = as.integer(rowSums(
      cbind(KI_4_3_Applicable
            , as.integer(KI_4_3_Duration < 86401)
            )
      , na.rm = TRUE) > 1)
    ,KI_4_3d = KI_4_3_Applicable
    ,KI_4_4n = as.integer(rowSums(
      cbind(KI_4_4_Applicable
            , as.integer(KI_4_4_Duration < 86401)
            )
      , na.rm = TRUE) > 1)
    ,KI_4_4d = KI_4_4_Applicable
    ,KI_4_5n = as.integer(rowSums(
      cbind(KI_4_5_Applicable
            , as.integer(KI_4_5_Duration < 259201)
            )
      , na.rm = TRUE) > 1)
    ,KI_4_5d = KI_4_5_Applicable
    )
  })

output[colnames(DomainFour)] <- DomainFour

## Domain 5 ----
DomainFive <- with(df, {
  dc = as.integer(!is.na(S07DepartureDate))
  # KI_5_1 is n/a for this study. Overly convoluted and n/a as a team metric
  TotalTherapyMinutes = rowSums(
    cbind(
      S04CommsSwallowMinutes,
      S04MotorMinutes,
      S04PsychoFunctionMinutes,
      S04OtherMinutes
    ),
    na.rm = TRUE
  )
  TherapyWindow = ceiling(pmax(as.numeric(
    difftime(
      pmin(
        as.Date(S07EndRehabDate, tz = "Europe/London"),
        as.Date(S07DepartureDate, tz = "Europe/London"),
        na.rm = TRUE
      ),
      as.Date(S01ClockStartTime, tz = "Europe/London"),
      unit = "days"
    )
  ) - 1, 1))
  KI_5_2act = as.numeric(round(TotalTherapyMinutes / TherapyWindow, digits = 0))
  HadTherapy = as.integer(TotalTherapyMinutes > 0)## nb. documentation states no exclusions. Validation exercises suggest otherwise.

  data.frame(KI_5_1n = NA
             ,KI_5_2n = as.integer(HadTherapy == 1 & KI_5_2act > 127)
             ,KI_5_2d = dc#HadTherapy# 07/26 review SSNAP Jan-Mar, reverted to SSNAPs documentation of no exclusions
             ,KI_5_3n = ifelse(HadTherapy == 1,KI_5_2act * 60,NA)#KI_5_2act * 60# 07/26 review SSNAP Jan-Mar, reverted to SSNAPs documentation of HAS exclusions
             )
  })

output[colnames(DomainFive)] <- DomainFive

## Domain 6 ----
DomainSix <- with(df, {

  KI_6_1act = as.numeric(S04MotorMinutes / S04MotorDays)
  KI_6_3act = as.numeric(S04PsychoFunctionMinutes / S04PsychoFunctionDays)
  KI_6_5act = as.numeric(S04CommsSwallowMinutes / S04CommsSwallowDays)
  TherapyWindow = ceiling(pmax(as.numeric(
    difftime(
      pmin(
        as.Date(S07EndRehabDate, tz = "Europe/London"),
        as.Date(S07DepartureDate, tz = "Europe/London"),
        na.rm = TRUE
      ),
      as.Date(S01ClockStartTime, tz = "Europe/London"),
      unit = "days"
    )
  ) - 1, 1))

  data.frame(KI_6_1d = S04ConsideredMotor
             ,KI_6_1n = as.integer(KI_6_1act > 179)
             ,KI_6_1act
             ,KI_6_2n = S04MotorDays / TherapyWindow
             ,KI_6_3d = S04PsychoFunction
             ,KI_6_3n = as.integer(KI_6_3act > 44)
             ,KI_6_3act
             ,KI_6_4n = S04PsychoFunctionDays / TherapyWindow
             ,KI_6_5d = S04CommsSwallow
             ,KI_6_5n = as.integer(KI_6_5act > 44)
             ,KI_6_5act
             ,KI_6_6n = S04CommsSwallowDays / TherapyWindow
             )
  })

output[colnames(DomainSix)] <- DomainSix

## Domain 7 ----
Domainseven <- with(df, {

  MalnutritionScreening = ifelse(S06MalnutritionScreening == "NS", 0,
                                 ifelse(is.na(S06MalnutritionScreeningDietitianDate), 0, 1))
  HighRisk = ifelse(S06MalnutritionScreening == "Y", 1, 0)
  Infection7Days = ifelse(S05UrinaryTractInfection7Days == "Y", 1, 0)
  Antibiotics = ifelse(S05PneumoniaAntibiotics7Days == "Y", 1, 0)
  dc = as.integer(!is.na(S07DepartureDate))
  death = ifelse(S07DischargeType == "D", 1, 0)
  dcalive = ifelse(dc == 1 & death == 0, 1, 0)
  NamedContact = ifelse(dc == 1 & S07DischargeNamedContact == 1, 1, 0)
  DcNamedContact = as.integer(rowSums(
    cbind(dcalive
          , NamedContact
          )
    , na.rm = TRUE) >1)

  data.frame(KI_7_1n = MalnutritionScreening
             ,KI_7_1d = HighRisk
             ,KI_7_2n = as.integer(rowSums(
               cbind(
                 Infection7Days
                 , Antibiotics
                 )
               ,na.rm = TRUE) > 0)
             ,KI_7_3d = ifelse(S06MoodScreeningNoScreeningReason %in% c("PR", "MU"), 0, 1)
             ,KI_7_3n = ifelse(is.na(S06MoodScreeningDate), 0, 1)
             ,KI_7_4n = ifelse(is.na(S06CognitionScreeningDate), 0, 1)
             ,KI_7_4d = ifelse(S06CognitionScreeningNoScreeningReason %in% c("PR", "MU"),0,1)
             ,KI_7_5n = ifelse(is.na(S06PsychologistByDischargeDateTime), 0, 1)
             ,KI_7_5d = ifelse(S06PsychologistByDischargeNotAssessedReason %in% c("PR", "MU", "PD"),0,1)
             ,KI_7_6n = ifelse(is.na(S06VisualScreenTool), 0, 1)
             ,KI_7_6d = ifelse(S06NotScreenedReason %in% c("PR", "PU"), 0, 1)
             ,KI_7_7n = ifelse(!is.na(S06Orthoptist),1,ifelse(S06OrthoptistNAReason == "SA", 1, 0))
             ,KI_7_7d = ifelse(S06OrthoptistNAReason %in% c("PR", "PU", "PD"), 0, 1)
             ,KI_7_8n = DcNamedContact
             ,KI_7_8d = dcalive
             )
  })

output[colnames(Domainseven)] <- Domainseven

# end ----
output
}
