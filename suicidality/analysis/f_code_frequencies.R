# f_code_frequencies.R
# Generate frequency table of all ICD-10 F-codes (psychiatric diagnoses)
# before follow-up for the study cohort.
#
# Note: This script requires raw_diagnoses_cohort.rds because main_12wks_28.rds
# only contains binary diagnosis indicators, not the raw ICD-10 codes.

library(dplyr)
library(here)
here::i_am("suicidality/analysis/f_code_frequencies.R")

source(here("suicidality", "analysis", "common.R"))

# Load cohort for follow-up start dates (complete-case)
data <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))
n <- nrow(data)

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("F-CODE FREQUENCY TABLE\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")
cat("Total N:", format(n, big.mark = ","), "\n\n")

# Load raw diagnoses and join with cohort
raw_diagnoses <- read_rds_file("raw_diagnoses_cohort.rds")
fu_start_data <- data %>% dplyr::select(lopnr, fu_start)

f_codes <- fu_start_data %>%
  left_join(raw_diagnoses %>% dplyr::select(lopnr, dia, date = diagn_date),
            by = "lopnr", relationship = "many-to-many") %>%
  filter(!is.na(dia), date < fu_start) %>%
  filter(substr(dia, 1, 1) == "F") %>%
  distinct(lopnr, dia) %>%
  count(dia, name = "n_individuals") %>%
  mutate(pct = 100 * n_individuals / n) %>%
  arrange(desc(n_individuals))

rm(raw_diagnoses); invisible(gc())

# ICD-10 F-code descriptions (comprehensive)
f_code_labels <- c(
  # Range codes (Swedish registry artifacts)
  "F00-F09" = "Organic mental disorders (range)",
  "F00-F999" = "Mental disorders (range)",
  "F40-F48" = "Neurotic/stress-related disorders (range)",
  "F50-F59" = "Behavioural syndromes (range)",
  "F60-F69" = "Personality disorders (range)",
  "F19-P" = "Multiple substance use (provisional)",

  # F10: Alcohol use disorders
  "F100" = "Alcohol: acute intoxication",
  "F101" = "Alcohol: harmful use",
  "F102" = "Alcohol: dependence syndrome",
  "F102A" = "Alcohol: dependence, early remission",
  "F102B" = "Alcohol: dependence, partial remission",
  "F102X" = "Alcohol: dependence, unspecified",
  "F103" = "Alcohol: withdrawal state",
  "F104" = "Alcohol: withdrawal with delirium",
  "F105" = "Alcohol: psychotic disorder",
  "F106" = "Alcohol: amnesic syndrome",
  "F107" = "Alcohol: residual disorder",
  "F107W" = "Alcohol: residual disorder, other",
  "F108" = "Alcohol: other mental disorder",
  "F109" = "Alcohol: unspecified mental disorder",

  # F11: Opioid use disorders
  "F110" = "Opioid: acute intoxication",
  "F111" = "Opioid: harmful use",
  "F112" = "Opioid: dependence syndrome",
  "F113" = "Opioid: withdrawal state",
  "F1130" = "Opioid: withdrawal, uncomplicated",
  "F114" = "Opioid: withdrawal with delirium",
  "F115" = "Opioid: psychotic disorder",
  "F116" = "Opioid: amnesic syndrome",
  "F117" = "Opioid: residual disorder",
  "F118" = "Opioid: other mental disorder",
  "F119" = "Opioid: unspecified mental disorder",

  # F12: Cannabis use disorders
  "F120" = "Cannabis: acute intoxication",
  "F121" = "Cannabis: harmful use",
  "F122" = "Cannabis: dependence syndrome",
  "F123" = "Cannabis: withdrawal state",
  "F124" = "Cannabis: withdrawal with delirium",
  "F125" = "Cannabis: psychotic disorder",
  "F126" = "Cannabis: amnesic syndrome",
  "F127" = "Cannabis: residual disorder",
  "F128" = "Cannabis: other mental disorder",
  "F129" = "Cannabis: unspecified mental disorder",

  # F13: Sedative/hypnotic use disorders
  "F130" = "Sedatives: acute intoxication",
  "F131" = "Sedatives: harmful use",
  "F132" = "Sedatives: dependence syndrome",
  "F133" = "Sedatives: withdrawal state",
  "F134" = "Sedatives: withdrawal with delirium",
  "F135" = "Sedatives: psychotic disorder",
  "F137" = "Sedatives: residual disorder",
  "F138" = "Sedatives: other mental disorder",
  "F139" = "Sedatives: unspecified mental disorder",

  # F14: Cocaine use disorders
  "F140" = "Cocaine: acute intoxication",
  "F141" = "Cocaine: harmful use",
  "F142" = "Cocaine: dependence syndrome",
  "F143" = "Cocaine: withdrawal state",
  "F144" = "Cocaine: withdrawal with delirium",
  "F145" = "Cocaine: psychotic disorder",
  "F147" = "Cocaine: residual disorder",
  "F148" = "Cocaine: other mental disorder",
  "F149" = "Cocaine: unspecified mental disorder",

  # F15: Stimulant use disorders
  "F150" = "Stimulants: acute intoxication",
  "F151" = "Stimulants: harmful use",
  "F152" = "Stimulants: dependence syndrome",
  "F153" = "Stimulants: withdrawal state",
  "F154" = "Stimulants: withdrawal with delirium",
  "F155" = "Stimulants: psychotic disorder",
  "F156" = "Stimulants: amnesic syndrome",
  "F157" = "Stimulants: residual disorder",
  "F158" = "Stimulants: other mental disorder",
  "F159" = "Stimulants: unspecified mental disorder",

  # F16: Hallucinogen use disorders
  "F160" = "Hallucinogens: acute intoxication",
  "F161" = "Hallucinogens: harmful use",
  "F162" = "Hallucinogens: dependence syndrome",
  "F163" = "Hallucinogens: withdrawal state",
  "F164" = "Hallucinogens: withdrawal with delirium",
  "F165" = "Hallucinogens: psychotic disorder",
  "F166" = "Hallucinogens: amnesic syndrome",
  "F167" = "Hallucinogens: residual disorder",
  "F168" = "Hallucinogens: other mental disorder",
  "F169" = "Hallucinogens: unspecified mental disorder",

  # F17: Tobacco use disorders
  "F17" = "Tobacco use disorder",
  "F170" = "Tobacco: acute intoxication",
  "F171" = "Tobacco: harmful use",
  "F172" = "Tobacco: dependence syndrome",
  "F173" = "Tobacco: withdrawal state",
  "F178" = "Tobacco: other mental disorder",
  "F179" = "Tobacco: unspecified mental disorder",

  # F18: Volatile solvent use disorders
  "F180" = "Solvents: acute intoxication",
  "F181" = "Solvents: harmful use",
  "F182" = "Solvents: dependence syndrome",
  "F183" = "Solvents: withdrawal state",
  "F184" = "Solvents: withdrawal with delirium",
  "F185" = "Solvents: psychotic disorder",
  "F188" = "Solvents: other mental disorder",
  "F189" = "Solvents: unspecified mental disorder",

  # F19: Multiple drug use disorders
  "F19" = "Multiple substance use",
  "F190" = "Multiple drugs: acute intoxication",
  "F191" = "Multiple drugs: harmful use",
  "F192" = "Multiple drugs: dependence syndrome",
  "F193" = "Multiple drugs: withdrawal state",
  "F194" = "Multiple drugs: withdrawal with delirium",
  "F195" = "Multiple drugs: psychotic disorder",
  "F196" = "Multiple drugs: amnesic syndrome",
  "F197" = "Multiple drugs: residual disorder",
  "F198" = "Multiple drugs: other mental disorder",
  "F199" = "Multiple drugs: unspecified mental disorder",
  "F1999" = "Multiple drugs: unspecified (detailed)",

  # F20-F29: Schizophrenia and psychotic disorders
  "F20" = "Schizophrenia",
  "F21" = "Schizotypal disorder",
  "F22" = "Persistent delusional disorders",
  "F23" = "Acute and transient psychotic disorders",
  "F24" = "Induced delusional disorder",
  "F25" = "Schizoaffective disorders",
  "F250" = "Schizoaffective disorder, manic type",
  "F28" = "Other nonorganic psychotic disorders",
  "F29" = "Unspecified nonorganic psychosis",

  # F30: Manic episode
  "F30" = "Manic episode",
  "F300" = "Hypomania",

  # F31: Bipolar affective disorder
  "F31" = "Bipolar affective disorder",
  "F31-" = "Bipolar disorder (unspecified)",
  "F310" = "Bipolar: current episode hypomanic",
  "F311" = "Bipolar: current episode manic without psychosis",
  "F312" = "Bipolar: current episode manic with psychosis",
  "F313" = "Bipolar: current episode mild/moderate depression",
  "F314" = "Bipolar: current episode severe depression",
  "F315" = "Bipolar: current episode severe depression with psychosis",
  "F316" = "Bipolar: current episode mixed",
  "F317" = "Bipolar: currently in remission",
  "F318" = "Bipolar: other episodes",
  "F318A" = "Bipolar II disorder",
  "F318B" = "Bipolar: rapid cycling",
  "F318C" = "Bipolar: other specified",
  "F318D" = "Bipolar: other specified",
  "F318E" = "Bipolar: other specified",
  "F318F" = "Bipolar: other specified",
  "F318W" = "Bipolar: other specified",
  "F319" = "Bipolar: unspecified",

  # F32: Depressive episode
  "F32" = "Depressive episode",
  "F32-" = "Depressive episode (unspecified)",
  "F320" = "Mild depressive episode",
  "F321" = "Moderate depressive episode",
  "F3212" = "Moderate depressive episode (detailed)",
  "F321P" = "Moderate depressive episode (provisional)",
  "F322" = "Severe depressive episode without psychosis",
  "F323" = "Severe depressive episode with psychosis",
  "F323A" = "Severe depression with mood-congruent psychosis",
  "F323W" = "Severe depression with psychosis, other",
  "F324" = "Depressive episode in partial remission",
  "F328" = "Other depressive episodes",
  "F329" = "Depressive episode, unspecified",

  # F33: Recurrent depressive disorder
  "F33" = "Recurrent depressive disorder",
  "F33-" = "Recurrent depression (unspecified)",
  "F330" = "Recurrent depression: current episode mild",
  "F331" = "Recurrent depression: current episode moderate",
  "F332" = "Recurrent depression: current episode severe",
  "F333" = "Recurrent depression: severe with psychosis",
  "F334" = "Recurrent depression: currently in remission",
  "F338" = "Other recurrent depressive disorders",
  "F339" = "Recurrent depressive disorder, unspecified",

  # F34: Persistent mood disorders
  "F340" = "Cyclothymia",
  "F341" = "Dysthymia",
  "F348" = "Other persistent mood disorders",
  "F349" = "Persistent mood disorder, unspecified",

  # F38-F39: Other mood disorders
  "F38" = "Other mood disorders",
  "F39" = "Unspecified mood disorder",

  # F40: Phobic anxiety disorders
  "F400" = "Agoraphobia",
  "F4000" = "Agoraphobia without panic disorder",
  "F4001" = "Agoraphobia with panic disorder",
  "F401" = "Social phobias",
  "F401P" = "Social phobia (provisional)",
  "F402" = "Specific (isolated) phobias",
  "F402B" = "Specific phobia: blood/injection/injury",
  "F402F" = "Specific phobia: animal type",
  "F402G" = "Specific phobia: natural environment",
  "F402W" = "Specific phobia: other",

  # F41: Other anxiety disorders
  "F410" = "Panic disorder",
  "F411" = "Generalized anxiety disorder",

  # F42: Obsessive-compulsive disorder
  "F42" = "Obsessive-compulsive disorder",
  "F428" = "Other obsessive-compulsive disorders",
  "F428A" = "OCD: primarily obsessional",
  "F428W" = "OCD: other specified",

  # F43: Reaction to severe stress and adjustment disorders
  "F43" = "Reaction to severe stress/adjustment",
  "F430" = "Acute stress reaction",
  "F431" = "Post-traumatic stress disorder",
  "F432" = "Adjustment disorders",
  "F4320" = "Adjustment: brief depressive reaction",
  "F4322" = "Adjustment: mixed anxiety and depression",
  "F4324" = "Adjustment: with disturbance of emotions",
  "F4325" = "Adjustment: with disturbance of conduct",
  "F4328" = "Adjustment: other specified",
  "F438" = "Other reactions to severe stress",
  "F438A" = "Complex PTSD",
  "F438W" = "Other stress reactions",
  "F439" = "Reaction to severe stress, unspecified",
  "F439P" = "Stress reaction (provisional)",

  # F45: Somatoform disorders
  "F452" = "Hypochondriacal disorder",
  "F452A" = "Hypochondriacal disorder, specified",
  "F452B" = "Body dysmorphic disorder",
  "F452C" = "Hypochondriacal disorder, other",
  "F452X" = "Hypochondriacal disorder, unspecified",

  # F50: Eating disorders
  "F50" = "Eating disorders",
  "F500" = "Anorexia nervosa",
  "F5000" = "Anorexia nervosa, restricting type",
  "F5002" = "Anorexia nervosa, binge-eating/purging type",
  "F501" = "Atypical anorexia nervosa",
  "F502" = "Bulimia nervosa",
  "F503" = "Atypical bulimia nervosa",
  "F504" = "Overeating associated with psychological factors",
  "F505" = "Vomiting associated with psychological factors",
  "F508" = "Other eating disorders",
  "F509" = "Eating disorder, unspecified",
  "F509P" = "Eating disorder (provisional)",

  # F51: Sleep disorders
  "F51" = "Nonorganic sleep disorders",

  # F60: Specific personality disorders
  "F600" = "Paranoid personality disorder",
  "F601" = "Schizoid personality disorder",
  "F602" = "Dissocial personality disorder",
  "F603" = "Emotionally unstable personality disorder",
  "F6031" = "Emotionally unstable: borderline type",
  "F604" = "Histrionic personality disorder",
  "F605" = "Anankastic personality disorder",
  "F606" = "Anxious (avoidant) personality disorder",
  "F607" = "Dependent personality disorder",
  "F608" = "Other specific personality disorders",
  "F609" = "Personality disorder, unspecified",

  # F63: Habit and impulse disorders
  "F630" = "Pathological gambling",
  "F633" = "Trichotillomania",

  # F64: Gender identity disorders
  "F64" = "Gender identity disorders",

  # F70-F79: Intellectual disability
  "F70" = "Mild intellectual disability",
  "F71" = "Moderate intellectual disability",
  "F72" = "Severe intellectual disability",
  "F73" = "Profound intellectual disability",
  "F78" = "Other intellectual disability",
  "F79" = "Unspecified intellectual disability",

  # F84: Pervasive developmental disorders (Autism spectrum)
  "F840" = "Childhood autism",
  "F841" = "Atypical autism",
  "F842" = "Rett syndrome",
  "F843" = "Other childhood disintegrative disorder",
  "F844" = "Overactive disorder with intellectual disability",
  "F845" = "Asperger syndrome",
  "F848" = "Other pervasive developmental disorders",
  "F849" = "Pervasive developmental disorder, unspecified",

  # F90: Hyperkinetic disorders (ADHD)
  "F90" = "Hyperkinetic disorders (ADHD)",
  "F90-" = "ADHD (unspecified)",
  "F900" = "Disturbance of activity and attention",
  "F900A" = "ADHD: predominantly inattentive",
  "F900B" = "ADHD: combined type",
  "F900BP" = "ADHD: combined type (provisional)",
  "F900C" = "ADHD: predominantly hyperactive-impulsive",
  "F900X" = "ADHD: other specified",
  "F901" = "Hyperkinetic conduct disorder",
  "F908" = "Other hyperkinetic disorders",
  "F909" = "Hyperkinetic disorder, unspecified",

  # F91-F98: Other childhood disorders
  "F91" = "Conduct disorders",
  "F92" = "Mixed disorders of conduct and emotions",
  "F93" = "Emotional disorders with childhood onset",
  "F94" = "Disorders of social functioning in childhood",
  "F95" = "Tic disorders",
  "F950" = "Transient tic disorder",
  "F951" = "Chronic motor or vocal tic disorder",
  "F952" = "Tourette syndrome",
  "F958" = "Other tic disorders",
  "F959" = "Tic disorder, unspecified",
  "F98" = "Other behavioural/emotional disorders of childhood"
)

cat("ALL F-CODES (ICD-10 psychiatric diagnoses before follow-up)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

cat(sprintf("  %-8s %8s  %6s  %s\n", "Code", "N", "%", "Description"))
cat(sprintf("  %-8s %8s  %6s  %s\n", "--------", "------", "----", paste(rep("-", 50), collapse="")))

for (i in seq_len(nrow(f_codes))) {
  code <- f_codes$dia[i]
  n_ind <- f_codes$n_individuals[i]
  pct <- f_codes$pct[i]
  desc <- if (code %in% names(f_code_labels)) f_code_labels[code] else ""
  cat(sprintf("  %-8s %8s  (%5.1f%%)  %s\n",
              code, format(n_ind, big.mark = ","), pct, desc))
}
cat("\n")

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("END OF F-CODE FREQUENCY TABLE\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
