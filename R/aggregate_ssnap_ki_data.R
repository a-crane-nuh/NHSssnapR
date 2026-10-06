#' aggregate imported and parsed SSNAP data into core key indicators (ki)
#'
#' @param df a data.frame object which has created from derive_num_denom_flags.R
#' @param Interval Non optional, non defaulted value. week, month, quarter, year
#'    recommend not to use day due to low population for single sites
#'
#' @importFrom stats median
#'
#' @returns a data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' aggregate_ssnap_ki_data(df)
#' }
aggregate_ssnap_ki_data <- function(df, Interval) {

  KeyIndicatorReference <- load_metadata("SSNAP_Key_Indicator_Reference.csv")

  SSNAP_Time_Score_Matrix <- load_metadata("SSNAP_Time_Score_Matrix.csv")

  DomainReference <- load_metadata("SSNAP_Domain_Levels.csv")

  all_teams <- merge(load_metadata("Inpatient-Teams-Contact-List-(August-2025-update).csv")
                     ,load_metadata("Site_specific_Samuel_Target.csv", skip = 1)
                     ,by.x = "TeamName"
                     ,by.y = "Site name")|>
    subset(select = c("TeamCode"
                      ,"TeamName"
                      ,"Bespoke thrombolysis site-specific target for patients arriving within 4 hours of onset (%)"
                      )
  )

  names(all_teams)[3]<-"KI.3.3.Site.Target"

  SiteTarget <- all_teams[all_teams$TeamCode == unique(df$SiteCode), ]

  SiteTarget <- as.numeric(SiteTarget$KI.3.3.Site.Target)/100 # source data conversion

  aggregation_group = as.Date(cut(df$S01ClockStartTime, breaks = Interval))

  max_date_filter = if(Interval == "weeks") {
    max(as.Date(cut(df$S01ClockStartTime, breaks = Interval)))+6 # aligns discharge metrics to admission metrics
  } else { max(as.Date(cut(df$S01ClockStartTime, breaks = Interval))) }

  date = as.Date(rownames(rowsum(subset(df)$d, aggregation_group, na.rm = TRUE)))
  d = as.integer(rowsum(subset(df)$d, aggregation_group, na.rm = TRUE))

  aggregated <- data.frame(
    Interval
    ,SiteCode = unique(df$SiteCode)
    ,SiteName = unique(df$SiteName)
    ,date
    ,d
  )

  for( k in grep("^KI_[0-9]+_[0-9]+$", KeyIndicatorReference$IndicatorColumnId, value = TRUE) ){
    # start of aggregation loop ----
    ## local data ----

    keys = KeyIndicatorReference[KeyIndicatorReference[["IndicatorColumnId"]] == k, , drop = FALSE]
    n = keys$IndicatorNumerator
    d = keys$IndicatorDenominator

    ## filter any indicators with all NA for numerator values ----
    if ( all(is.na(df[[keys$IndicatorNumerator]])) ){
      if ( keys$IndicatorDenominator=="score_matrix" ) {
        aggregated[[k]] = NA
        aggregated[[paste0(k,"score")]] <- NA
      } else { aggregated[[k]] = NA }
    } else {
      # split admission/discharge aggregation type ----
      # admission metrics ----
      if (keys$IndicatorAggregationGroup=="S01ClockStartTime") {
        ## admission aggregation group ----
        aggregation_group = as.Date(cut(df$S01ClockStartTime, breaks = Interval))
        ## admission measure type ----
        if (keys$IndicatorValue=="percentage" & keys$BespokeTargets==TRUE) {
          ## admission besoke SAMueL 2 target ----
          metric = tapply(subset(df)[[n]], aggregation_group, sum, na.rm = TRUE)
          metric = data.frame(
            date = as.Date(names(metric))
            ,value = unlist(metric)
            ,row.names = NULL
            )
          metric$dn <- tapply(subset(df)[[d]], aggregation_group, sum, na.rm = TRUE)
          metric$t <- SiteTarget
          metric[[paste0(k,"act")]] <- (metric$value/metric$dn)
          metric[[k]] <- pmin((metric$value/metric$dn)/SiteTarget,1)
          metric <- subset(metric, select = c("date","KI_3_3","KI_3_3act")) # this should be the only explicit
          ### append data - SAMueL 2 target ----
          aggregated <- merge(aggregated,metric, by = "date", all = TRUE)
        }
        if (keys$IndicatorValue=="percentage" & keys$BespokeTargets==FALSE) { # og this works with 3.3 overwritten in the following step
          ## admisison percentile metrics ----
          metric = tapply(subset(df)[[n]], aggregation_group, sum, na.rm = TRUE)
          metric = data.frame(
            date = as.Date(names(metric))
            ,value = unlist(metric)
            ,row.names = NULL
          )
          metric[[k]] = (metric$value/tapply(subset(df)[[d]], aggregation_group, sum, na.rm = TRUE))
          ### append data - percentiles ----
          aggregated <- merge(aggregated,subset(metric, select = c("date",k)), by = "date", all = TRUE)
        }
        if (keys$IndicatorValue=="time" ) {
          # admission time metrics ----
          metric = tapply(subset(df)[[n]], aggregation_group, median, na.rm = TRUE)
          metric = data.frame(
            date = as.Date(names(metric))
            ,value = unlist(metric)
            ,row.names = NULL
          )
          names(metric)[names(metric) == "value"] <- k

          subset_timematrix <- SSNAP_Time_Score_Matrix[
            SSNAP_Time_Score_Matrix$KeyIndicatorId == k,
            c("KeyIndicatorValue", "KeyIndicatorScore")
          ]

          timematrix <- data.frame(
            KeyIndicatorValue = c(0, subset_timematrix$KeyIndicatorValue * 60),
            KeyIndicatorScore = c(1, subset_timematrix$KeyIndicatorScore)
          )

          metric[[paste0(k,"score")]] <- timematrix$KeyIndicatorScore[findInterval(metric[[k]], timematrix$KeyIndicatorValue, all.inside = TRUE)]
          # append metric and score, the score will only be used for domain level aggregation
          aggregated <- merge(aggregated,metric, by = "date", all = TRUE)
        }
        # admission end ----
      } else {
        # ensures the discharge metrics do not go beyond the admission metrics
        dc_df <- df[as.Date(df$S07DepartureDate) < max_date_filter |
                      is.na(df$S07DepartureDate),]

        ## discharge aggregation group ----
        aggregation_group = as.Date(cut(dc_df$S07DepartureDate, breaks = Interval))
        ## discharge measure type ----
        ### append data - percentiles ----
        if (keys$IndicatorValue=="percentage" ) {
          ## discharge median days as percentiles ----
          if(is.na(keys$IndicatorDenominator)) {
            metric = tapply(subset(dc_df)[[n]], aggregation_group, median, na.rm = TRUE)
            metric = data.frame(
              date = as.Date(names(metric))
              ,value = unlist(metric)
              ,row.names = NULL
            )
            names(metric)[names(metric) == "value"] <- k
            aggregated <- merge(aggregated,subset(metric, select = c("date",k)), by = "date", all = TRUE)
          }
          if(!is.na(keys$IndicatorDenominator) & keys$IndicatorDenominator != "dc"){
            ## discharge percentile metrics ----
            metric = tapply(subset(dc_df)[[n]], aggregation_group, sum, na.rm = TRUE)
            metric = data.frame(
              date = as.Date(names(metric))
              ,value = unlist(metric)
              ,row.names = NULL
            )
            metric[[k]] = (metric$value/tapply(subset(dc_df)[[d]], aggregation_group, sum, na.rm = TRUE))
            ### append data - percentiles ----
            aggregated <- merge(aggregated,subset(metric, select = c("date",k)), by = "date", all = TRUE)
          }
          if(!is.na(keys$IndicatorDenominator) & keys$IndicatorDenominator=="dc") {
            metric = tapply(subset(dc_df)[[n]], aggregation_group, sum, na.rm = TRUE)
            metric = data.frame(
              date = as.Date(names(metric))
              ,value = unlist(metric)
              ,row.names = NULL
            )

            metric[[k]] = (metric$value/tapply(subset(dc_df)[[d]], aggregation_group, sum, na.rm = TRUE))
            metric = subset(metric, select = c("date","KI_7_2"))

            subset_scorematrix <- SSNAP_Time_Score_Matrix[
              SSNAP_Time_Score_Matrix$KeyIndicatorId == k,
              c("KeyIndicatorValue", "KeyIndicatorScore")
            ]
            scorematrix <- data.frame(
              KeyIndicatorValue = c(0, subset_scorematrix$KeyIndicatorValue)
              ,KeyIndicatorScore = c(1, subset_scorematrix$KeyIndicatorScore)
            )
            metric[[paste0(k,"score")]] <- scorematrix$KeyIndicatorScore[findInterval(metric[[k]], scorematrix$KeyIndicatorValue, all.inside = TRUE)]
            ### append data - scorematrix ----
            aggregated <- merge(aggregated,metric, by = "date", all = TRUE)
          }
        } else {
          # discharge time metrics (5.3only)----
          metric = tapply(subset(dc_df)[[n]], aggregation_group, median, na.rm = TRUE)
          metric = data.frame(
            date = as.Date(names(metric))
            ,value = unlist(metric)
            ,row.names = NULL
          )
          names(metric)[names(metric) == "value"] <- k

          subset_timematrix <- SSNAP_Time_Score_Matrix[
            SSNAP_Time_Score_Matrix$KeyIndicatorId == k
            ,c("KeyIndicatorValue", "KeyIndicatorScore")
            ]

          timematrix <- data.frame(
            KeyIndicatorValue = c(0, subset_timematrix$KeyIndicatorValue * 60)
            ,KeyIndicatorScore = c(1, subset_timematrix$KeyIndicatorScore)
            )
          metric[[paste0(k,"score")]] <- timematrix$KeyIndicatorScore[findInterval(metric[[k]]
                                                                                   , timematrix$KeyIndicatorValue
                                                                                   , all.inside = TRUE)]
          ### append data - time ----
          aggregated <- merge(aggregated,metric, by = "date", all = TRUE)
        }
      }
    }
  }

  # end of key indicator aggregation loop ----
  # domain aggregations ----
  # domain 1 ----
  Domain_One <-
    with(aggregated, {

      v1 <- pmin(as.numeric(KI_1_1)*2.5,1)
      v2 <- KI_1_2score
      v3 <- pmin(as.numeric(KI_1_3)*4,1)
      v4 <- pmin(as.numeric(KI_1_4)*1.25,1)
      v5 <- as.numeric(KI_1_5)
      v6 <- as.numeric(KI_1_6)
      v7 <- as.numeric(KI_1_7)

      mat <- cbind(v1, v2, v3, v4, v5, v6, v7)
      row_sum <- rowSums(mat, na.rm = TRUE)
      row_count <- rowSums(!is.na(mat))
      Domain_One <-round(row_sum / row_count, 2)

      data.frame(Domain_One)
    })

  aggregated[colnames(Domain_One)] <- Domain_One
  # domain 2 ----
  Domain_Two <- with(aggregated, {

    v1 <- as.numeric(KI_2_1)
    v2 <- as.numeric(KI_2_2score)
    v3 <- as.numeric(KI_2_3)
    v4 <- pmin(as.numeric(KI_2_4)*1.25,100)

    mat <- cbind(v1, v2, v3, v4)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    Domain_Two <- round(row_sum / row_count, 2)

    data.frame(Domain_Two)
  })

  aggregated[colnames(Domain_Two)] <- Domain_Two
  # domain 3 ----
  Domain_Three <- with(aggregated, {

    v1 <- pmin(as.numeric(KI_3_1)*5, 1)
    v2 <- pmin(as.numeric(KI_3_2)*5, 1)
    v3 <- pmin(as.numeric(KI_3_3)*SiteTarget, 1)
    v4 <- as.numeric(KI_3_4score)
    v5 <- pmin(as.numeric(KI_3_5)*10,100)
    v6 <- if ("KI_3_6score" %in% names(aggregated)) as.numeric(KI_3_6score) else rep(NA_real_, nrow(aggregated))
    v7 <- as.numeric(KI_3_7score)

    mat <- cbind(v1, v2, v3, v4, v5, v6, v7)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    Domain_Three <- round(row_sum / row_count, 2)

    data.frame(Domain_Three)
  })

  aggregated[colnames(Domain_Three)] <- Domain_Three
  # domain 4 ----
  Domain_Four <- with(aggregated, {

    v1 <- as.numeric(KI_4_1)
    v2 <- as.numeric(KI_4_2)
    v3 <- as.numeric(KI_4_3)
    v4 <- as.numeric(KI_4_4)
    v5 <- as.numeric(KI_4_5)

    mat <- cbind(v1, v2, v3, v4, v5)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    Domain_Four <- round(row_sum / row_count, 2)

    data.frame(Domain_Four)
  })

  aggregated[colnames(Domain_Four)] <- Domain_Four
  # domain 5 ----
  Domain_Five  <- with(aggregated, {

    v1 <- as.numeric(KI_5_1)
    v2 <- pmin(as.numeric(KI_5_2)/2.2,100)
    v3 <- as.numeric(KI_5_3score)

    mat <- cbind(v1, v2, v3)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    Domain_Five <- round(row_sum / row_count, 2)

    data.frame(Domain_Five)
  })

  aggregated[colnames(Domain_Five)] <- Domain_Five
  # domain 6 ----
  Domain_Six <- with(aggregated, {

    v1 <- pmin(as.numeric(KI_6_1)*1.43,100)
    v2 <- as.numeric(KI_6_2)
    v3 <- as.numeric(KI_6_3)
    v4 <- as.numeric(KI_6_4)
    v5 <- as.numeric(KI_6_5)
    v6 <- as.numeric(KI_6_6)

    mat <- cbind(v1, v2, v3, v4, v5, v6)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    Domain_Six <- round(row_sum / row_count, 2)

    data.frame(Domain_Six)
  })

  aggregated[colnames(Domain_Six)] <- Domain_Six
  # domain 7 ----
  Domain_Seven <- with(aggregated, {
    # Apply multipliers and caps
    v1 <- as.numeric(KI_7_1)
    v2 <- as.numeric(KI_7_2score)
    v3 <- as.numeric(KI_7_3)
    v4 <- as.numeric(KI_7_4)
    v5 <- as.numeric(KI_7_5)
    v6 <- as.numeric(KI_7_6)
    v7 <- as.numeric(KI_7_7)
    v8 <- as.numeric(KI_7_8)

    mat <- cbind(v1, v2, v3, v4, v5, v6, v7, v8)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    Domain_Seven <- round(row_sum / row_count, 2)

    data.frame(Domain_Seven)
  })

  aggregated[colnames(Domain_Seven)] <- Domain_Seven
  # end of domain aggregates ----

  for(domain in unique(grep("^Domain[A-z]+$",DomainReference$DomainCode, value = TRUE)) ) {

    subset_domain_level <- DomainReference[
      DomainReference$DomainCode == domain
      ,c("MinScorePercent","DomainScore")
    ]

    domain_level <- data.frame(
      MinScorePercent = c(subset_domain_level$MinScorePercent,1)
      ,DomainScore = c(0,subset_domain_level$DomainScore)
    )


    metric <- aggregated[[domain]]
    metric_score <- domain_level$DomainScore[findInterval(c(metric), c(domain_level$MinScorePercent))]
    aggregated[[paste0(domain,"score")]] = metric_score
  }

  ##"TotalKeyIndicatorAvgScore"
  aggregated[("TotalKeyIndicatorAvgScore")] <- with(aggregated, {
    # Apply multipliers and caps
    v1 <- as.numeric(aggregated$Domain_Onescore)
    v2 <- as.numeric(aggregated$Domain_Twoscore)
    v3 <- as.numeric(aggregated$Domain_Threescore)
    v4 <- as.numeric(aggregated$Domain_Fourscore)
    v5 <- as.numeric(aggregated$Domain_Fivescore)
    v6 <- as.numeric(aggregated$Domain_Sixscore)
    v7 <- as.numeric(aggregated$Domain_Sevenscore)

    mat <- cbind(v1, v2, v3, v4, v5, v6, v7)
    row_sum <- rowSums(mat, na.rm = TRUE)
    row_count <- rowSums(!is.na(mat))
    round(row_sum / row_count, 2)
  })

  aggregated <-
    subset(
      aggregated
      , select =- c(Domain_Onescore
                    ,Domain_Twoscore
                    ,Domain_Threescore
                    ,Domain_Fourscore
                    ,Domain_Fivescore
                    ,Domain_Sixscore
                    ,Domain_Sevenscore)
      )

  output_dir <- file.path(getwd(), "out", "data")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (Interval == "months") {
    aggregated <- aggregated[aggregated$date < as.Date(cut(Sys.Date(), breaks = "months")), ]
  }
  if (Interval == "weeks") {
    aggregated <- aggregated[aggregated$date < as.Date(cut(Sys.Date(), breaks = "weeks", start.on.monday = TRUE)), ]
  } else { aggregated }

  saveRDS(
    aggregated
    ,file.path(output_dir,"aggregated.rds")
  )

  aggregated
}
