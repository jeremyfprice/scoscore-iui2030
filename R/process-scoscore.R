## ----load-libraries, message = FALSE, warning = FALSE----------------------------------
library(readr)
library(here)
library(openxlsx)
library(glue)
library(tidyr)
library(dplyr)
library(stringr)
library(curl)
library(showtext)
library(DT)




## ----set-constants, message=FALSE, warning=FALSE---------------------------------------
critical_threshold <- 1.000
focus_year <- 2023
round_threshold <- 0.970
state_3rd_proficiency <- 0.816
state_6th_proficiency <- 0.341
state_gpc <- 0.864


## ----function-prune-frame, message=FALSE, warning=FALSE--------------------------------
prune_frame <- function(working_frame) {
  working_frame |>
    # Remove NA's
    na.omit() |>
    # Keep only public schools
    filter(corp_id %in% crosswalk_frame$corp_id)
}


## ----function-set-numeric-ids, message=FALSE, warning=FALSE----------------------------
set_numeric_ids <- function(working_frame) {
  working_frame |>
    # Set the School Corporation ID column to numeric
    mutate(
      corp_id = as.numeric(corp_id)
    ) |>
    # Set the School ID column to numeric
    mutate(
      school_id = as.numeric(school_id)
    )
}


## ----federal-idoe-crosswalk, message=FALSE, warning=FALSE------------------------------
#   ____________________________________________________________________________
#   Create Federal-IDOE Crosswalk                                           ####
##  ............................................................................
##  Load Locale File                                                        ####
##  The Locale File is directly from a Federal DOE database and includes
##  the Federal LEA ID codes, rather than the Indiana DOE School
##  Corporation codes. lea_name is renamed to corp_name for consistency.
locale_frame <- read_csv(
  here(
    "data",
    glue("{focus_year}-in-school-corp-locale.csv")
  )
) |>
  rename(corp_name = lea_name)

##  ............................................................................
##  Load IREAD File                                                         ####
##  The IREAD File contains, on the otherhand, the Indiana DOE School
##  Corporation codes, but not the Federal DOE LEA codes.
iread_frame <- read_csv(
  here(
    "data",
    glue("{focus_year}-in-school-iread3.csv")
  )
)

##  ............................................................................
##  Create Crosswalk Frame                                                  ####
##  The two dataframes are joined together by matching the School
##  Corporation name so that the IDOE and USDOE codes match. This can be
##  used to allow different sources of data sets to align with one another.
crosswalk_frame <- iread_frame |>
  select(-iread_rate) |>
  full_join(locale_frame, by = "corp_name") |>
  na.omit() |>
  select(corp_id, school_id, leaid)


## ----read-school-corp-locale, message=FALSE, warning=FALSE-----------------------------
#   ____________________________________________________________________________
#   Process Locale Frame                                                    ####
#   Processing on the locale frame is completed by adding the IDOE
#   identifiers on to the frame and ensuring the School ID and School
#   Corporation ID columns are numeric for consistency.
locale_frame <- locale_frame |>
  full_join(crosswalk_frame, by = "leaid") |>
  select(-leaid) |>
  set_numeric_ids()


## ----read-demographics, message = FALSE, warning = FALSE-------------------------------
#   ____________________________________________________________________________
#   Load and Process Demographics File                                      ####
##  ............................................................................
##  Load Demographics File                                                  ####
##  The Demographics File is loaded and any blank or "omitted" ("***")
##  values are replaced with NA for easier processing.
read_demographics_frame <-
  read_csv(here("data", glue("{focus_year}-in-school-demographics.csv")),
    col_names = TRUE,
    na = c("", "***")
  ) |>
  ##  The Crosswalk Frame is used to only retain the school corporations that
  ##  are of interest (excluding private, charter, etc., schools) and that
  ##  consistently report data and are fully accountable to the communities
  ##  they serve.
  filter(corp_id %in% crosswalk_frame$corp_id) |>
  ##  Any NA is then replaced with 0 in the demographics columns.
  mutate(across(
    .cols = aina:nhpi,
    .fns = ~ if_else(is.na(.x) == TRUE, 0, .x)
  ))

##  ............................................................................
##  Process Demographics Frame                                              ####
##  An "underrepresented racial minority" column is created by calculating
##  the proportion of non-white students in the school. Similarly, a Free
##  and Reduced Lunch proportion is also calculated.
process_demographics_frame <- read_demographics_frame |>
  mutate(
    urm_percent =
      (aina +
        asian +
        black +
        latine +
        multiracial +
        nhpi) /
        enrollment
  ) |>
  mutate(
    frl_percent = frl / enrollment
  )

##  ............................................................................
##  Finalize Demographics Frame                                             ####
##  Keep only the necessary columns in the dataframe and convert all ID
##  columns to numeric for consistency.
demographics_frame <- process_demographics_frame |>
  select(
    corp_id,
    corp_name,
    school_id,
    school_name,
    urm_percent,
    frl_percent
  ) |>
  prune_frame() |>
  set_numeric_ids()

##  ............................................................................
##  Calculate Demographic Means                                             ####
##  Means for the demographic categories-underrepresented racial minorities and
##  free and reduced lunch-are calculated for future use in the counterfactual
##  analysis.
urm_percent_mean <- round(mean(demographics_frame$urm_percent), 3)
frl_percent_mean <- round(mean(demographics_frame$frl_percent), 3)


## ----read-iread, message = FALSE, warning = FALSE--------------------------------------

#   ____________________________________________________________________________
#   Load and Process IREAD File                                             ####
##  ............................................................................
##  Load IREAD File                                                         ####
## 
read_iread_frame <- read_csv(
  here(
    "data",
    glue("{focus_year}-in-school-iread3.csv")
  ),
  na = c("", "***")
) |>
  prune_frame() |>
  set_numeric_ids()

##  ............................................................................
##  Calculate IREAD Rates                                                   ####
iread_frame <- read_iread_frame |>
  mutate(iread_rate = as.numeric(str_sub(iread_rate, end = -2)) / 100)

##  ............................................................................
##  Calculate IREAD Mean                                                    ####
iread_mean <- round(mean(iread_frame$iread_rate), 3)


## ----read-ilearn, message = FALSE, warning = FALSE-------------------------------------

#   ____________________________________________________________________________
#   Load and Process ILEARN File                                            ####
##  ............................................................................
##  Load ILEARN File                                                        ####
read_ilearn_frame <- read_csv(
  here(
    "data",
    glue("{focus_year}-in-school-ilearn6.csv")
  ),
  na = c("", "***")
) |>
  prune_frame() |>
  set_numeric_ids()

##  ............................................................................
##  Calculate ILEARN Rates                                                  ####
ilearn_frame <- read_ilearn_frame |>
  mutate(ilearn_rate = as.numeric(str_sub(ilearn_rate, end = -2)) / 100)

##  ............................................................................
##  Calculate ILEARN Mean                                                   ####
ilearn_mean <- round(mean(ilearn_frame$ilearn_rate), 3)


## ----read-grad, message = FALSE, warning = FALSE---------------------------------------
#   ____________________________________________________________________________
#   Load and Process Graduation Pathways Completion File                    ####
##  ............................................................................
##  Load Graduation Pathways Completion File                                ####
read_grad_frame <- read_csv(
  here(
    "data",
    glue("{focus_year}-in-school-gcr.csv")
  ),
  na = c("", "***")
) |>
  prune_frame() |>
  set_numeric_ids()

##  ............................................................................
##  Process Graduation Completion Rates File                                ####
grad_frame <- read_grad_frame |>
  mutate(grad_rate = as.numeric(str_sub(grad_rate, end = -2)) / 100)

##  ............................................................................
##  Calculate Graduate Pathways Completion Rate Mean                        ####
grad_mean <- round(mean(grad_frame$grad_rate), 3)


## ----calculate-scoscore, message=FALSE, warning=FALSE----------------------------------
scoscore_demographics <- locale_frame |>
  full_join(
    demographics_frame,
    by = c("corp_id", "school_id"),
    suffix=c("",".y")) |>
  select(-ends_with(".y"))

scoscore_iread <- scoscore_demographics |>
  full_join(
    iread_frame,
    by = c("corp_id", "school_id"),
    suffix=c("",".y")) |>
  select(-ends_with(".y"))

scoscore_ilearn <- scoscore_iread |>
  full_join(
    ilearn_frame,
    by = c("corp_id", "school_id"),
    suffix=c("",".y")) |>
  mutate(corp_name = if_else(
    is.na(corp_name),
    corp_name.y,
    corp_name
  )) |>
  mutate(school_name = if_else(
    is.na(school_name),
    school_name.y,
    school_name
  )) |>
  select(-ends_with(".y"))

scoscore_grad <- scoscore_ilearn |>
  full_join(
    grad_frame,
    by = c("corp_id", "school_id"),
    suffix=c("",".y")) |>
  mutate(corp_name = if_else(
    is.na(corp_name),
    corp_name.y,
    corp_name
  )) |>
  mutate(school_name = if_else(
    is.na(school_name),
    school_name.y,
    school_name
  )) |>
  select(-ends_with(".y"))

scoscore_frame <- scoscore_grad |>
  filter(!is.na(corp_name)) |>
  filter(!is.na(urm_percent)) |>
  filter(!is.na(frl_percent)) |>
  mutate(iread_rate_adj = as.numeric(if_else(
    iread_rate < state_3rd_proficiency,
    (state_3rd_proficiency - iread_rate + 1),
    0
  ))) |>
  mutate(ilearn_rate_adj = as.numeric(if_else(
    ilearn_rate < state_6th_proficiency,
    (state_6th_proficiency - ilearn_rate + 1),
    0
  ))) |>
  mutate(grad_rate_adj = as.numeric(if_else(
    grad_rate < state_gpc,
    (state_gpc - grad_rate + 1),
    0
  ))) |>
  rowwise() |>
  mutate(
    adj_academic = round(
      mean(
        c(iread_rate_adj, ilearn_rate_adj, grad_rate_adj),
        na.rm = TRUE),
    3)) |>
  ungroup() |>
  select(
    -contains(c(
      "iread",
      "ilearn",
      "grad"))) |>
  mutate(
    scoScore = (
      ((urm_percent * 1.5) + (frl_percent * 1.5) + (adj_academic)) / 3
    )
  ) |>
  mutate(rounded_up = if_else(
    scoScore < critical_threshold & scoScore >= round_threshold,
    TRUE,
    FALSE
  )) |>
  mutate(scoScore = if_else(
    scoScore < critical_threshold & scoScore >= round_threshold,
    1,
    scoScore
  )) |>
  mutate(counter_factual = FALSE)


## ----counterfactual-analysis, message=FALSE, warning=FALSE-----------------------------

adj_ac_mean <- round(
  mean(scoscore_frame$adj_academic),
  3)
adj_ac_sd <- round(
  sd(scoscore_frame$adj_academic),
  3)
adj_ac_cutoff <- adj_ac_mean + adj_ac_sd

whatif_frame <- scoscore_frame |>
  filter(adj_academic >= adj_ac_cutoff) |>
  filter(scoScore < 1) |>
  select(-scoScore) |>
  mutate(urm_percent = ifelse(
    urm_percent < urm_percent_mean,
    urm_percent_mean,
    urm_percent
  )) |>
  mutate(frl_percent = ifelse(
    frl_percent < frl_percent_mean,
    frl_percent_mean,
    frl_percent
  )) |>
  mutate(
    scoScore = (
      ((urm_percent * 1.5) + (frl_percent * 1.5) + (adj_academic)) / 3
    )
  ) |>
    mutate(
    scoScore = round(scoScore, 3)
  ) |>
    mutate(rounded_up = if_else(
    scoScore < critical_threshold & scoScore >= round_threshold,
    TRUE,
    FALSE
  )) |>
  mutate(scoScore = if_else(
    scoScore < 1 & scoScore >= 0.970,
    1,
    scoScore
  )) |>
     filter(
    scoScore >= 1
  ) |>
  mutate(
    counter_factual = TRUE
  ) |>
  select(
    corp_id,
    school_id,
    scoScore,
    rounded_up,
    counter_factual
  )

scoscore_frame <- scoscore_frame |>
  left_join(
    whatif_frame,
    by = c("corp_id", "school_id")
    ) |>
  mutate(counter_factual = if_else(!is.na(counter_factual.y),
                                   counter_factual.y,
                                   counter_factual.x)) |>
  mutate(rounded_up = if_else(!is.na(rounded_up.y),
                                   rounded_up.y,
                                   rounded_up.x)) |>
  mutate(scoScore = if_else(!is.na(scoScore.y),
                                   scoScore.y,
                                   scoScore.x)) |>
  select(-(ends_with(c(".x", ".y"))))


## ----output-file, message=FALSE, warning=FALSE-----------------------------------------

scoscore_frame <- scoscore_frame |>
  select(
    corp_id,
    school_id,
    corp_name,
    school_name,
    urban_centric_locale,
    urm_percent,
    frl_percent,
    adj_academic,
    scoScore,
    rounded_up,
    counter_factual
  ) |>
  arrange(
    corp_id,
    school_id
  )

write.xlsx(
  scoscore_frame,
  here(
    "output",
    glue("{focus_year}-in-scoscore-schools.xlsx")
  ),
  colNames = TRUE
)

