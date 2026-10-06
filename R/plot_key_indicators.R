#' Title
#'
#' @param df an aggregated database created aggregate_ssnap_ki_data
#' @param rebase_date optional date fo data to be rebased
#'
#' @importFrom svglite svglite
#'
#' @returns plots saved to users working directory then out/plots/key_indicators
#' @export
#'
#' @examples
#' \dontrun{
#' plot_key_indicators(df)
#' }
plot_key_indicators <- function(df, rebase_date = NA ) {
  # stop breaks ----
  if (is.null(df)) {
    stop("Oops, did you forget to add a file", call. = FALSE)
  }
  if (!is.data.frame(df)) {
    stop("input must be a data.frame", call. = FALSE)
  }

  if( unique(stats::na.omit(df$Interval)) == "months") {
    df <- df[df$date  < as.Date(cut(Sys.Date(), breaks = "months")), ]
  } else { df }

  rebase_fact <- if (!is.na(rebase_date)) {
  NHSRplotthedots::ptd_rebase(max(df$date[df$date < rebase_date], na.rm = TRUE))
  } else {
    NHSRplotthedots::ptd_rebase()
    }
  # meta data ----
  ## ki_meta_data ----
  Key_Indicator_Reference <- load_metadata("SSNAP_Key_Indicator_Reference.csv")

  Key_Indicator_Reference$IndicatorName <- gsub("_", ".", Key_Indicator_Reference$IndicatorColumnId)

  Domain_Reference <- load_metadata("SSNAP_Domain_Levels.csv")
  ### index ki_meta_data----

  data.table::setDT(Key_Indicator_Reference)

  data.table::setkey(Key_Indicator_Reference, IndicatorColumnId)

  data.table::setDT(Domain_Reference)

  data.table::setkey(Domain_Reference, DomainCode)

  ## ki_score_matrix ----
  SSNAP_Time_Score_Matrix <- load_metadata("SSNAP_Time_Score_Matrix.csv")
  ### index ki_score_matrix----

  data.table::setDT(SSNAP_Time_Score_Matrix)

  data.table::setkey(SSNAP_Time_Score_Matrix, KeyIndicatorId)

  Interval <- unique(stats::na.omit(df$Interval))

  SiteName <- unique(stats::na.omit(df$SiteName))

  SiteCode <- unique(stats::na.omit(df$SiteCode))

  # plot save location  ----
  # using working directory as root. sub dir for plots/plot types. e.g.
  # key_indicators, domain_scores, distribution_charts etc.

  output_dir <- file.path(getwd(), "out", "plots","key_indicators")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_dir_ds <- file.path(getwd(), "out", "plots","domain_scores")

  if (!dir.exists(output_dir_ds)) {
    dir.create(output_dir_ds, recursive = TRUE, showWarnings = FALSE)
  }

  output_dir_tki <- file.path(getwd(), "out", "plots","total_key_indicator")#base_dir_ds

  if (!dir.exists(output_dir_tki)) {
    dir.create(output_dir_tki, recursive = TRUE, showWarnings = FALSE)
  }
  # loop start ----
  for ( ki in grep("^KI_[0-9]+_[0-9]+$", Key_Indicator_Reference$IndicatorColumnId, value = TRUE)) {
    #ki="KI_1_1"
    ## in loop indicator skip ----
    if(!(ki %in% names(df))) {
      message("skipping missing key indicator input: ", ki)
      next
    }
    if(all(is.na(df[[ki]]))) {
      message("Skipping all NA Key indicator: ", ki)
      next
    }
    if(all(is.nan(df[[ki]]))) { # does NA already cover this?
      message("Skipping all NAN Key indicator: ", ki)
      next
    }
    ## in loop meta data ----

    ki_meta_data <- Key_Indicator_Reference[Key_Indicator_Reference[["IndicatorColumnId"]] == ki,]

    ki_score_matrix <- SSNAP_Time_Score_Matrix[SSNAP_Time_Score_Matrix[["KeyIndicatorId"]] == ki,]

    time_breaks <- c(0,as.numeric(ki_score_matrix$KeyIndicatorValue)*60)

    time_breaks[time_breaks==max(time_breaks)] <- round(max(time_breaks)*1.15,digits = 0)

    ## in loop indicator data
    ki_aggregated_data <- stats::na.omit(df[,c("date",ki)])# "Interval","SiteCode","SiteName" optional? would you use it?

    ### Create spc----
    # change to create spc_dat to be called upon separately
    spc_dat <-
      eval(
        substitute(
          NHSRplotthedots::ptd_spc(
            ki_aggregated_data
            ,value_field = valid_column
            ,date_field = date
            ,improvement_direction = ki_meta_data$IndicatorDirection
            ,rebase=rebase_fact  # a global option at this time
          )
          ,list(valid_column = as.name(ki))
        )
      )
      # create the aesthetic levels.
    spc<-
      spc_dat|>
      NHSRplotthedots::ptd_create_ggplot(
        point_size = 2.2  ## default 2.2 allow use to override
        ,icons_position = "top left" ## default top left, allow use to override. default to "top left" <- aesthetic choice
        #, x_axis_breaks = "1 months" ## base this on input Interval?? default to 3 months
        , label_limits = TRUE
        )+
      ggplot2::scale_x_date( ## adding this to expand x axis beyond max data range.
        date_breaks = "1 months"
        ,expand = ggplot2::expansion(mult = c(.05,.15))
      )+
      ### over to meta data to add in individual chart elements ----
      ggplot2::labs(
        title = ki_meta_data$IndicatorDescriptionShort
        ,subtitle = paste0(ki_meta_data$IndicatorName
                           ," with an improvement direction of "
                           ,ki_meta_data$IndicatorDirection)
        ,y = ki_meta_data$IndicatorName
        ,x = paste0(ifelse(ki_meta_data$IndicatorAggregationGroup=="S07DepartureDate" , "Discharge","Admission")," by ",Interval)
        ,alt = ki_meta_data$IndicatorDescriptionFull
        ,caption = paste("Data produced from the beta version of the NHSssnapR package for: ",SiteName)
      )+
        if (ki_meta_data$IndicatorValue == "percentage" ) {

          list(
            ggplot2::scale_y_continuous(
              labels = scales::label_percent()
              ,breaks = seq(0,1,.1)
              ,position = "right" ## consider user alteration?
            )
            , ggplot2::coord_cartesian(
              ylim = c(0,1)
              ,clip = "off"
            )
          )
        } else {
          list(
            ggplot2::scale_y_continuous(
              transform = "hms"
              ,limits = c(0,NA)
              ,breaks = c(time_breaks, max(time_breaks))
              ,position = "right" ## consider user alteration?
              )
            , ggplot2::coord_cartesian(
              ylim = c(0, max(time_breaks))
              ,clip = "off"
              )
            )
        }
      spc <-
        spc+if (ki_meta_data$IndicatorValue == "percentage" ) {
          ggplot2::geom_text(
            ggplot2::aes(x = x
                ,y = y
                ,label = scales::percent(y, accuracy = 1)
                ,hjust = ifelse(y >= .9, 1.5, -0.5))
            ,check_overlap = TRUE
            ,colour = "#1c1e20"
            ,size = 3
            ,angle = 90)
        }else{
          ggplot2::geom_text(
            ggplot2::aes(x =x
                ,y=y
                ,label=hms::as_hms(y)
                ,vjust=.5
                ,hjust =-.5)
            ,check_overlap=T
            ,colour="#1c1e20"
            ,size=3
            , angle=90)
        }

      last_point <- spc_dat[which.max(spc_dat$x), ]

      spc <-
        spc+
        ggplot2::geom_text(
          data = last_point
          , ggplot2::aes(
            x = x
            ,y = lpl
            , label = if (ki_meta_data$IndicatorValue == "percentage")
            {scales::percent(lpl, accuracy = 1)
            }else {
              hms::as_hms(trunc(lpl))
            }
          )
          ,hjust = -1
          ,colour = "#1c1e20"
          ,size = 2.5
        )+
        ggplot2::geom_text(
          data = last_point
          , ggplot2::aes(
            x = x
            ,y = mean_col
            , label = if (ki_meta_data$IndicatorValue == "percentage")
            {scales::percent(mean_col, accuracy = 1)
            }else {
              hms::as_hms(trunc(mean_col))
            }
          )
          ,hjust = -1
          ,colour = "#1c1e20"
          ,size = 2.5
        )+
        ggplot2::geom_text(
          data = last_point
          , ggplot2::aes(
            x = x
            ,y = upl
            , label = if (ki_meta_data$IndicatorValue == "percentage")
            {scales::percent(upl, accuracy = 1)
            }else {
              hms::as_hms(trunc(upl))
            }
            )
          ,hjust = -1
          ,colour = "#1c1e20"
          ,size = 2.5
        )+
        ggplot2::theme( # consider handling via own function.
          # 1. Outer padding
          plot.margin = ggplot2::margin(t = 24, r = 32, b = 24, l = 24, unit = "pt")
          # 2. Title spacing
          ,plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 4))
          ,plot.subtitle = ggplot2::element_text(color = "grey30", size = 8, margin = ggplot2::margin(b = 20))
          ,plot.caption = ggplot2::element_text(color = "grey50", size = 6, margin = ggplot2::margin(t = 16))
          # 3. Axis styling
          ,axis.text.x = ggplot2::element_text(size = 8, colour = "grey30")
          ,axis.title.x = ggplot2::element_text(size = 7, colour = "grey30", margin = ggplot2::margin(t = 8))
          ,axis.text.y = ggplot2::element_text(size = 8, colour = "grey30")
          ,axis.title.y = ggplot2::element_text(size = 7, colour = "grey30", margin = ggplot2::margin(r = 8))
          # 4. Horizontal only
          ,panel.grid.minor = ggplot2::element_blank()
          ,panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.5)
          # 5. Background colour
          ,plot.background = ggplot2::element_rect(fill = "#FCFCFC", colour = NA)
        )
      ##print(spc)

      file_path_spc_name <- file.path(output_dir, paste0(ki,".svg"))

      ggplot2::ggsave(
        filename = file_path_spc_name
        , plot = spc
        , width = 16
        , height = 9
        , units = "in"
        , bg = "transparent"
        )
  }
  # key indicator fun end ----
  # domain indicators fun start ----

  for (ds in grep("^Domain[A-z]+$",unique(Domain_Reference$DomainCode), value = TRUE) ){

    ## in loop indicator skip ----
    if(!(ds %in% names(df))) {
      message("skipping missing key indicator input: ", ds)
      next
    }
    if(all(is.na(df[[ds]]))) {
      message("Skipping all NA Key indicator: ", ds)
      next
    }

    # domain meta
    ds_meta_data <- Key_Indicator_Reference[Key_Indicator_Reference[["IndicatorColumnId"]] == ds,]
    # y axis
    pct_breaks <- Domain_Reference[Domain_Reference[["DomainCode"]] == ds,]$MinScorePercent
    # grade
    grades <-  Domain_Reference[Domain_Reference[["DomainCode"]] == ds,]
    # geom_rect
    grade_a <- grades[grades[["DomainGrade"]] == "A",]
    grade_b <- grades[grades[["DomainGrade"]] == "B",]
    grade_c <- grades[grades[["DomainGrade"]] == "C",]
    grade_d <- grades[grades[["DomainGrade"]] == "D",]
    grade_e <- grades[grades[["DomainGrade"]] == "E",]

    ds_aggregated_data <- stats::na.omit(df[,c("date",ds)])

    ds_spc_dat <-
      eval(
        substitute(
          NHSRplotthedots::ptd_spc(
            ds_aggregated_data
            ,value_field = valid_column
            ,date_field = date
            ,improvement_direction = ds_meta_data$IndicatorDirection
            ,rebase=rebase_fact  # a global option at this time
          )
          ,list(valid_column = as.name(ds))
        )
      )

    ds_spc <-
      ds_spc_dat|>
        NHSRplotthedots::ptd_create_ggplot(
          point_size = 2.2  ## default 2.2 allow use to override
          ,icons_position = "top left" ## default top left, allow use to override. default to "top left" <- aesthetic choice
          #, x_axis_breaks = "1 months" ## base this on input Interval?? default to 3 months
          , label_limits = TRUE
        )+
        ggplot2::geom_text(
          ggplot2::aes(x = x,y = y,label = scales::percent(y, accuracy = 1),hjust = ifelse(y >= .9, 1.5, -0.5))
          ,check_overlap = TRUE
          ,colour = "#1c1e20"
          ,size = 3
          ,angle = 90)+
        ggplot2::geom_rect(
          data = grade_a
          , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
          , inherit.aes = FALSE
          , fill = grade_a$GradeHexCode, alpha = .111)+
        ggplot2::geom_rect(
          data = grade_b
          , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
          , inherit.aes = FALSE
          , fill = grade_b$GradeHexCode, alpha = .111)+
        ggplot2::geom_rect(
          data = grade_c
          , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
          , inherit.aes = FALSE
          , fill = grade_c$GradeHexCode, alpha = .111)+
        ggplot2::geom_rect(
          data = grade_d
          , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
          , inherit.aes = FALSE
          , fill = grade_d$GradeHexCode, alpha = .111)+
        ggplot2::geom_rect(
          data = grade_e
          , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
          , inherit.aes = FALSE
          , fill = grade_e$GradeHexCode, alpha = .111)+
        ggplot2::scale_x_date( ## adding this to expand x axis beyond max data range.
          date_breaks = "1 months"
          ,expand = ggplot2::expansion(mult = c(.05,.15))
        )+
        ggplot2::scale_y_continuous(
          labels = scales::label_percent()
          ,breaks = c(pct_breaks) # based on5 bands graded A-E
          ,position = "right" ## consider user alteration?
        )+
        ggplot2::labs(
          title = ds_meta_data$IndicatorDomain
          ,subtitle = paste0(ds_meta_data$IndicatorDescriptionFull
                             ," with an improvement direction of "
                             ,ds_meta_data$IndicatorDirection)
          ,y = ds_meta_data$IndicatorColumnId
          ,x = paste0("by ",Interval)
          ,alt = ds_meta_data$IndicatorDescriptionFull
          ,caption = paste("Data produced from the beta version of the NHSssnapR package for: ",SiteName)
        )

    ds_last_point <- ds_spc_dat[which.max(ds_spc_dat$x), ]

    ds_spc <-
      ds_spc+
      ggplot2::geom_text(
        data = ds_last_point
        , ggplot2::aes(
          x = x
          ,y = lpl
          , label = scales::percent(lpl, accuracy = 1)
          )
        ,hjust = -1
        ,colour = "#1c1e20"
        ,size = 2.5
        )+
      ggplot2::geom_text(
        data = ds_last_point
        , ggplot2::aes(
          x = x
          ,y = mean_col
          , label = scales::percent(mean_col, accuracy = 1)
        )
        ,hjust = -1
        ,colour = "#1c1e20"
        ,size = 2.5
        )+
      ggplot2::geom_text(
        data = ds_last_point
        , ggplot2::aes(
          x = x
          ,y = upl
          , label = scales::percent(upl, accuracy = 1)
        )
        ,hjust = -1
        ,colour = "#1c1e20"
        ,size = 2.5
        )+
      ggplot2::theme( # consider handling via own function.
        # 1. Outer padding
        plot.margin = ggplot2::margin(t = 24, r = 32, b = 24, l = 24, unit = "pt")
        # 2. Title spacing
        ,plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 4))
        ,plot.subtitle = ggplot2::element_text(color = "grey30", size = 8, margin = ggplot2::margin(b = 20))
        ,plot.caption = ggplot2::element_text(color = "grey50", size = 6, margin = ggplot2::margin(t = 16))
        # 3. Axis styling
        ,axis.text.x = ggplot2::element_text(size = 8, colour = "grey30")
        ,axis.title.x = ggplot2::element_text(size = 7, colour = "grey30", margin = ggplot2::margin(t = 8))
        ,axis.text.y = ggplot2::element_text(size = 8, colour = "grey30")
        ,axis.title.y = ggplot2::element_text(size = 7, colour = "grey30", margin = ggplot2::margin(r = 8))
        # 4. Horizontal only
        ,panel.grid.minor = ggplot2::element_blank()
        ,panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.5)
        # 5. Background colour
        ,plot.background = ggplot2::element_rect(fill = "#FCFCFC", colour = NA)
      )

    #print(ds_spc)

    file_path_spc_name_ds <- file.path(output_dir_ds, paste0(ds,".svg"))

    ggplot2::ggsave(
      filename = file_path_spc_name_ds
      , plot = ds_spc
      , width = 16
      , height = 9
      , units = "in"
      , bg = "transparent"
     )
  }
  # domain indicators fun end ----

  # total key indicator (tki) start ----

  if (!("TotalKeyIndicatorAvgScore" %in% names(df))) {
    message("Skipping Total Key Indicator plot: column missing")
    return(invisible())
  }

  if (all(is.na(df[["TotalKeyIndicatorAvgScore"]]))) {
    message("Skipping Total Key Indicator plot: all values are NA")
    return(invisible())
  }

  tki_data <- stats::na.omit(df[,c("date","TotalKeyIndicatorAvgScore")])

  tki_reference <- Domain_Reference[Domain_Reference[["DomainCode"]]=="TotalKeyIndicatorAvgScore",]

  pct_breaks <- c(Domain_Reference[Domain_Reference[["DomainCode"]] == "TotalKeyIndicatorAvgScore",]$MinScorePercent,1)

  tki_spc_dat <-
    NHSRplotthedots::ptd_spc(
      tki_data
      ,value_field = TotalKeyIndicatorAvgScore
      ,date_field = date
      ,improvement_direction = "increase"
      ,rebase=rebase_fact  # a global option at this time
      )

  tki_spc <-
    tki_spc_dat|>
    NHSRplotthedots::ptd_create_ggplot(
      point_size = 2.2  ## default 2.2 allow use to override
      ,icons_position = "top left" ## default top left, allow use to override. default to "top left" <- aesthetic choice
      #, x_axis_breaks = "1 months" ## base this on input Interval?? default to 3 months
      #, label_limits = TRUE
    )+
  ggplot2::geom_text(
    ggplot2::aes(x = x,y = y,label = scales::percent(y, accuracy = 1),hjust = ifelse(y >= .9, 1.5, -0.5))
    ,check_overlap = TRUE
    ,colour = "#1c1e20"
    ,size = 3
    ,angle = 90)+
  ggplot2::geom_rect(
    data = tki_reference[tki_reference[["DomainGrade"]]=="A",]
    , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
    , inherit.aes = FALSE
    , fill = "#A5D46A"#as.character(tki_reference[tki_reference[["DomainGrade"]]=="A"][["GradeHexCode"]])
    , alpha = .111)+
  ggplot2::geom_rect(
    data = tki_reference[tki_reference[["DomainGrade"]]=="B",]
    , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
    , inherit.aes = FALSE
    , fill = "#FFFF80"#as.character(tki_reference[tki_reference[["DomainGrade"]]=="B"][["GradeHexCode"]])
    , alpha = .111)+
  ggplot2::geom_rect(
    data = tki_reference[tki_reference[["DomainGrade"]]=="C",]
    , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
    , inherit.aes = FALSE
    , fill = "#FFDF80"#as.character(tki_reference[tki_reference[["DomainGrade"]]=="C"][["GradeHexCode"]])
    , alpha = .111)+
  ggplot2::geom_rect(
    data = tki_reference[tki_reference[["DomainGrade"]]=="D",]
    , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
    , inherit.aes = FALSE
    , fill = "#FFC080"#as.character(tki_reference[tki_reference[["DomainGrade"]]=="D"][["GradeHexCode"]])
    , alpha = .111)+
  ggplot2::geom_rect(
    data = tki_reference[tki_reference[["DomainGrade"]]=="E",]
    , ggplot2::aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin =MinScorePercent, ymax = MaxScorePercent  )
    , inherit.aes = FALSE
    , fill = "#FFA080"#as.character(tki_reference[tki_reference[["DomainGrade"]]=="E"][["GradeHexCode"]])
    , alpha = .111)+
  ggplot2::scale_x_date( ## adding this to expand x axis beyond max data range.
    date_breaks = "1 months"
    ,expand = ggplot2::expansion(mult = c(.05,.15))
  )+
  ggplot2::scale_y_continuous(
    labels = scales::label_percent()
    ,breaks = c(pct_breaks) # based on5 bands graded A-E
    ,position = "right" ## consider user alteration?
  )+
    ggplot2::labs(
      title = "Total Key Indicator level"
      ,subtitle = "with an improvement direction of increase"
      ,y = "% Score"
      ,x = paste0("by ",Interval)
      ,caption = paste("Data produced from the beta version of the NHSssnapR package for: ",SiteName)
    )

  tki_last_point <- tki_spc_dat[which.max(tki_spc_dat$x), ]

  tki_spc <-
    tki_spc+
  ggplot2::geom_text(
    data = tki_last_point
    , ggplot2::aes(
      x = x
      ,y = lpl
      , label = scales::percent(lpl, accuracy = 1)
      )
    ,hjust = -1
    ,colour = "#1c1e20"
    ,size = 2.5
    )+
  ggplot2::geom_text(
    data = tki_last_point
    , ggplot2::aes(
      x = x
      ,y = mean_col
      , label = scales::percent(mean_col, accuracy = 1)
    )
    ,hjust = -1
    ,colour = "#1c1e20"
    ,size = 2.5
    )+
  ggplot2::geom_text(
    data = tki_last_point
    , ggplot2::aes(
      x = x
      ,y = upl
      , label = scales::percent(upl, accuracy = 1)
    )
    ,hjust = -1
    ,colour = "#1c1e20"
    ,size = 2.5
    )+
      ggplot2::theme( # consider handling via own function.
        # 1. Outer padding
        plot.margin = ggplot2::margin(t = 24, r = 32, b = 24, l = 24, unit = "pt")
        # 2. Title spacing
        ,plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 4))
        ,plot.subtitle = ggplot2::element_text(color = "grey30", size = 8, margin = ggplot2::margin(b = 20))
        ,plot.caption = ggplot2::element_text(color = "grey50", size = 6, margin = ggplot2::margin(t = 16))
        # 3. Axis styling
        ,axis.text.x = ggplot2::element_text(size = 8, colour = "grey30")
        ,axis.title.x = ggplot2::element_text(size = 7, colour = "grey30", margin = ggplot2::margin(t = 8))
        ,axis.text.y = ggplot2::element_text(size = 8, colour = "grey30")
        ,axis.title.y = ggplot2::element_text(size = 7, colour = "grey30", margin = ggplot2::margin(r = 8))
        # 4. Horizontal only
        ,panel.grid.minor = ggplot2::element_blank()
        ,panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.5)
        # 5. Background colour
        ,plot.background = ggplot2::element_rect(fill = "#FCFCFC", colour = NA)
      )
  # print(tki_spc)
  file_path_spc_name_tki <- file.path(output_dir_tki, "TotalKeyIndicator.svg")

  ggplot2::ggsave(
    filename = file_path_spc_name_tki
    , plot = tki_spc
    , width = 16
    , height = 9
    , units = "in"
    , bg = "transparent"
  )
  # total key indicator end ----
}
#fun end----
