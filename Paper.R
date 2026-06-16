# ==============================================================================
# ANALYSIS v6 — CLEAN STRUCTURED PIPELINE
# ==============================================================================
# Data requirements:
#   data/compustat_full.csv, data/ccm_link.csv, data/KPSS_2024.csv,
#   data/Match_patent_permco_permno_2024.csv,
#   data/PatentSimilarityImportanceBreakthrough_forPost2022.csv,
#   data/g_cpc_current.tsv, data/g_patent.tsv, data/g_application.tsv,
#   data/patex_application_data.csv, data/bds2023_sec.csv,
#   data/bds2023_vcn4_fac.csv, data/2022-NAICS-to-SIC-Crosswalk.xlsx,
#   markup_estimation.R
# ==============================================================================
rm(list = ls()); gc()
library(tidyverse); library(data.table); library(fixest)
library(lubridate); library(patchwork); library(readxl)
select <- dplyr::select; filter <- dplyr::filter

setwd("/Users/bedavischer/Desktop/Idea")
dir.create("figures", showWarnings = FALSE)

# --- Helpers ---
read_csv_lower <- function(path, ...) { dt <- fread(path, ...); names(dt) <- tolower(names(dt)); dt }
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs, na.rm = TRUE); x[x < q[1]] <- q[1]; x[x > q[2]] <- q[2]; x
}
z90 <- 1.645
theme_econ <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.3),
          axis.ticks = element_line(color = "black", linewidth = 0.3),
          legend.position = "bottom", legend.title = element_blank(),
          plot.title = element_text(size = rel(1.05), face = "bold"))
}
plot_irf <- function(tbl, ylab, title, color = "#2166AC") {
  tbl |> ggplot(aes(x = h, y = b)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = color, alpha = 0.15) +
    geom_line(linewidth = 0.8, color = color) +
    geom_point(size = 2, color = color) +
    labs(x = "Horizon (years)", y = ylab, title = title) +
    scale_x_continuous(breaks = 0:8) + theme_econ()
}
xwalk <- read_xlsx("data/2022-NAICS-to-SIC-Crosswalk.xlsx")
H <- 8


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  SECTION A: DATA LOADING & PANEL CONSTRUCTION                           ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ======================================================================
# A1: COMPUSTAT
# ======================================================================
cat("=== A1: Compustat ===\n")
comp <- read_csv_lower("data/compustat_full.csv") |>
  select(any_of(c("gvkey","fyear","sic","naics","sale","cogs","xrd","xsga",
                  "at","ppegt","ppent","emp","capx"))) |>
  filter(!is.na(sale), sale > 0, !is.na(cogs), cogs > 0,
         !(between(as.numeric(sic), 6000, 6999)),
         !(between(as.numeric(sic), 4900, 4999))) |>
  mutate(markup = sale / cogs, log_sale = log(sale),
         rd_intensity = ifelse(is.na(xrd), 0, xrd) / sale,
         sic4 = as.character(sic), sic3 = substr(sic, 1, 3), sic2 = substr(sic, 1, 2),
         naics3 = if ("naics" %in% names(cur_data())) substr(naics, 1, 3) else NA_character_)
cat("  Compustat:", nrow(comp), "firm-years\n")

source("markup_estimation.R")
markup_estimates <- estimate_markups_translog(comp)

ccm <- read_csv_lower("data/ccm_link.csv") |>
  filter(linktype %in% c("LC","LU"), linkprim %in% c("P","C")) |>
  select(gvkey, lpermno, linkdt, linkenddt) |>
  mutate(linkdt = ymd(linkdt),
         linkenddt = ymd(ifelse(is.na(linkenddt) | linkenddt == "", "2099-12-31", linkenddt))) |>
  mutate(linkenddt = replace_na(linkenddt, ymd("2099-12-31")))

# ======================================================================
# A2: PATENT DATA
# ======================================================================
cat("\n=== A2: Patent data ===\n")
kpss <- fread("data/KPSS_2024.csv", select = c("patent_num","filing_date","xi_real","cites")) |>
  mutate(patent_id = as.character(patent_num)) |> select(-patent_num)
names(kpss) <- tolower(names(kpss))

kpss_match <- fread("data/Match_patent_permco_permno_2024.csv", select = c("patent_num","permno")) |>
  mutate(patent_id = as.character(patent_num)) |> select(-patent_num)
names(kpss_match) <- tolower(names(kpss_match))

kpst_bt <- fread("data/PatentSimilarityImportanceBreakthrough_forPost2022.csv",
                 select = c("patent_num","bk_p90_alqsim05","lqsim05","issue_year")) |>
  mutate(patent_id = as.character(patent_num)) |> filter(patent_num >= 2000000) |>
  select(patent_id, breakthrough = bk_p90_alqsim05, importance = lqsim05)

pv_cpc <- fread("data/g_cpc_current.tsv", sep = "\t",
                select = c("patent_id","cpc_section","cpc_group","cpc_type")) |>
  mutate(patent_id = as.character(patent_id)) |> filter(cpc_type == "inventional")
pv_patent <- if (file.exists("data/g_patent.tsv")) {
  fread("data/g_patent.tsv", sep = "\t", select = c("patent_id","num_claims")) |>
    mutate(patent_id = as.character(patent_id))
} else NULL
pv_app <- if (file.exists("data/g_application.tsv")) {
  fread("data/g_application.tsv", sep = "\t", select = c("patent_id","patent_application_type")) |>
    mutate(patent_id = as.character(patent_id),
           is_continuation = as.integer(grepl("continuation|divisional",
                                              tolower(patent_application_type), perl = TRUE)))
} else NULL

bds_sec <- read_csv_lower("data/bds2023_sec.csv") |> select(sector, year, firms, estabs_entry_rate, estabs_exit_rate)
bds_ind <- read_csv_lower("data/bds2023_vcn4_fac.csv")

# --- Build patent-firm panel ---
cat("  Building patent-firm panel...\n")
patent_firm <- kpss_match |> select(patent_id, permno) |>
  inner_join(kpss |> select(patent_id, filing_date, xi_real, cites), by = "patent_id") |>
  inner_join(ccm |> rename(permno = lpermno), by = "permno") |>
  mutate(filing_dt = ymd(filing_date)) |>
  filter(!is.na(filing_dt), filing_dt >= linkdt, filing_dt <= linkenddt) |>
  select(patent_id, gvkey, permno, filing_date, xi_real, cites) |>
  distinct(patent_id, .keep_all = TRUE)

kpst_years <- fread("data/PatentSimilarityImportanceBreakthrough_forPost2022.csv",
                    select = c("patent_num","issue_year")) |>
  mutate(patent_id = as.character(patent_num)) |> select(patent_id, issue_year)
patent_firm <- patent_firm |> left_join(kpst_years, by = "patent_id") |> filter(!is.na(issue_year))
rm(kpst_years); gc()

patent_firm <- patent_firm |>
  left_join(kpst_bt, by = "patent_id") |>
  mutate(has_kpst = !is.na(breakthrough),
         is_nonbreakthrough = ifelse(has_kpst, as.integer(breakthrough == 0), NA_integer_))

# Add lqsim05 as separate column
kpst_lq <- fread("data/PatentSimilarityImportanceBreakthrough_forPost2022.csv",
                 select = c("patent_num","lqsim05")) |>
  mutate(patent_id = as.character(patent_num)) |> select(patent_id, lqsim05)
patent_firm <- patent_firm |> left_join(kpst_lq, by = "patent_id")
rm(kpst_bt, kpst_lq); gc()

# CPC breadth + primary class
patent_breadth <- pv_cpc |> select(patent_id, cpc_section) |> distinct() |>
  group_by(patent_id) |> summarise(n_cpc_sections = n_distinct(cpc_section), .groups = "drop")
patent_firm <- patent_firm |> left_join(patent_breadth, by = "patent_id"); rm(patent_breadth)
cpc_primary <- pv_cpc |> mutate(cpc_sub = substr(cpc_group, 1, 4)) |>
  group_by(patent_id) |> slice(1) |> ungroup() |> select(patent_id, cpc_sub)
patent_firm <- patent_firm |> left_join(cpc_primary, by = "patent_id"); rm(cpc_primary, pv_cpc); gc()

if (!is.null(pv_patent)) { patent_firm <- patent_firm |> left_join(pv_patent |> select(patent_id, num_claims), by = "patent_id"); rm(pv_patent); gc() }
if (!is.null(pv_app)) { patent_firm <- patent_firm |> left_join(pv_app |> select(patent_id, is_continuation), by = "patent_id"); rm(pv_app); gc() }
patent_firm <- patent_firm |>
  mutate(time_to_grant = ifelse(!is.na(filing_date) & !is.na(issue_year),
                                issue_year - year(ymd(filing_date)), NA_real_))
cat("  Patents:", nrow(patent_firm), "| KPST:", round(100*mean(patent_firm$has_kpst),1), "%\n")
rm(kpss, kpss_match); gc()

# ======================================================================
# A3: BUILD PANEL
# ======================================================================
cat("\n=== A3: Panel ===\n")
firm_yr_patents <- patent_firm |> filter(issue_year >= 1980) |>
  group_by(gvkey, issue_year) |>
  summarise(n_patents = n(), n_scored = sum(has_kpst),
            def_share = mean(is_nonbreakthrough, na.rm = TRUE),
            avg_xi_real = mean(xi_real, na.rm = TRUE), .groups = "drop") |>
  rename(fyear = issue_year)

panel <- comp |>
  left_join(firm_yr_patents, by = c("gvkey","fyear")) |>
  mutate(has_patents = !is.na(n_patents), n_patents = replace_na(n_patents, 0),
         n_scored = replace_na(n_scored, 0), log_emp = log(emp + 0.001), log_assets = log(at)) |>
  left_join(markup_estimates |> select(gvkey, fyear, markup_translog, elasticity_v), by = c("gvkey","fyear"))
rm(firm_yr_patents, markup_estimates); gc()
cat("  Panel:", nrow(panel), "\n")

# ======================================================================
# A4: TFP — SIC2 (baseline) with HP trend smoothing
# ======================================================================
cat("\n=== A4: TFP (SIC2, HP-smoothed) ===\n")
tfp_data <- panel |>
  filter(!is.na(sale), sale > 0, !is.na(emp), emp > 0,
         !is.na(ppegt), ppegt > 0, !is.na(cogs), cogs > 0, fyear >= 1980) |>
  mutate(log_va = log(sale - cogs + 1), log_k = log(ppegt), log_l = log(emp)) |>
  filter(!is.na(log_va), is.finite(log_va))

sic2_counts <- tfp_data |> count(sic2) |> filter(n >= 200)
tfp_data <- tfp_data |> filter(sic2 %in% sic2_counts$sic2)

tfp_estimates <- tfp_data |>
  group_by(sic2) |>
  group_modify(~{
    reg <- tryCatch(lm(log_va ~ log_k + log_l + factor(fyear), data = .x), error = function(e) NULL)
    if (is.null(reg)) return(.x |> mutate(tfp_raw = NA_real_))
    .x |> mutate(tfp_raw = residuals(reg))
  }) |> ungroup() |> select(gvkey, fyear, sic2, sic3, tfp_raw)
rm(tfp_data, sic2_counts); gc()

# HP filter to extract trend (lambda = 6.25 for annual data, Ravn-Uhlig)
# tfp = tfp_raw - HP_trend  =>  cyclically adjusted TFP
hp_smooth <- function(y, lambda = 6.25) {
  n <- length(y)
  if (n < 5 || all(is.na(y))) return(rep(NA_real_, n))
  # Fill NAs for filter then restore
  y_filled <- y; nas <- is.na(y)
  if (any(nas)) y_filled[nas] <- approx(seq_along(y), y, xout = which(nas), rule = 2)$y
  I <- diag(n)
  D <- diff(I, differences = 2)
  trend <- solve(I + lambda * t(D) %*% D, y_filled)
  out <- y - trend
  out[nas] <- NA_real_
  out
}

tfp_estimates <- tfp_estimates |>
  arrange(gvkey, fyear) |>
  group_by(gvkey) |>
  mutate(tfp = if (sum(!is.na(tfp_raw)) >= 5) hp_smooth(tfp_raw) else tfp_raw) |>
  ungroup()

cat("  TFP estimates:", sum(!is.na(tfp_estimates$tfp)), "\n")

ind_tfp <- tfp_estimates |> filter(!is.na(tfp)) |>
  group_by(sic3, fyear) |>
  summarise(avg_tfp = mean(tfp, na.rm = TRUE), n_firms_tfp = n(), .groups = "drop") |>
  rename(year = fyear)

# ======================================================================
# A5: BDS DYNAMISM
# ======================================================================
cat("\n=== A5: BDS ===\n")
bds_naics4 <- bds_ind |>
  mutate(across(c(firms, estabs, emp, denom, estabs_entry, estabs_exit,
                  job_creation, job_creation_births, job_destruction, job_destruction_deaths,
                  firmdeath_firms), ~suppressWarnings(as.numeric(.)))) |>
  group_by(vcnaics4, year) |>
  summarise(across(c(firms, estabs, emp, denom, estabs_entry, estabs_exit,
                     job_creation, job_creation_births, job_destruction, job_destruction_deaths,
                     firmdeath_firms), ~sum(., na.rm = TRUE)), .groups = "drop") |>
  mutate(naics3 = substr(as.character(vcnaics4), 1, 3),
         entry_rate = estabs_entry / estabs, exit_rate = estabs_exit / estabs,
         jc_births_rate = job_creation_births / denom, jd_deaths_rate = job_destruction_deaths / denom,
         realloc_rate = (job_creation + job_destruction) / denom, firmdeath_rate = firmdeath_firms / firms)

bds_naics3 <- bds_naics4 |> filter(!is.na(emp), emp > 0) |>
  group_by(naics3, year) |>
  summarise(entry_rate = weighted.mean(entry_rate, estabs, na.rm = TRUE),
            exit_rate = weighted.mean(exit_rate, estabs, na.rm = TRUE),
            jc_births_rate = weighted.mean(jc_births_rate, denom, na.rm = TRUE),
            jd_deaths_rate = weighted.mean(jd_deaths_rate, denom, na.rm = TRUE),
            realloc_rate = weighted.mean(realloc_rate, denom, na.rm = TRUE),
            firmdeath_rate = weighted.mean(firmdeath_rate, firms, na.rm = TRUE),
            .groups = "drop")
rm(bds_naics4, bds_ind); gc()

sic_naics_xwalk <- xwalk |>
  transmute(naics6 = as.character(`2022 NAICS Code`), sic4 = as.character(`Related SIC Code`),
            naics3 = substr(naics6, 1, 3), sic3 = substr(sic4, 1, 3)) |>
  filter(!is.na(naics3), !is.na(sic3), nchar(sic3) == 3) |>
  count(sic3, naics3) |> group_by(sic3) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |>
  select(sic3, naics3)
ind_bds <- bds_naics3 |> inner_join(sic_naics_xwalk, by = "naics3")
cat("  BDS:", nrow(ind_bds), "\n")

# ======================================================================
# A6: EXAMINER LENIENCY IV
# ======================================================================
cat("\n=== A6: Examiner leniency ===\n")
patex <- fread("data/patex_application_data.csv",
               select = c("examiner_full_name","examiner_art_unit","patent_number",
                          "filing_date","application_invention_type"))
patex <- patex[application_invention_type == "Utility" &
                 !is.na(examiner_full_name) & examiner_full_name != ""]
patex[, `:=`(patent_id = as.character(patent_number),
             filing_year = year(as.IDate(filing_date)),
             granted = as.integer(!is.na(patent_number) & patent_number != ""),
             art_unit = substr(as.character(examiner_art_unit), 1, 3),
             examiner_id = examiner_full_name)]
patex <- patex[filing_year >= 1980 & filing_year <= 2022]
patex[, c("examiner_full_name","examiner_art_unit","filing_date",
          "application_invention_type","patent_number") := NULL]; gc()

exam_stats <- patex[, .(n_apps = .N, n_grants = sum(granted)), by = .(art_unit, filing_year, examiner_id)]
au_yr <- exam_stats[, .(au_apps = sum(n_apps), au_grants = sum(n_grants), n_examiners = .N),
                    by = .(art_unit, filing_year)]
exam_leniency <- exam_stats[au_yr, on = .(art_unit, filing_year), nomatch = 0]
exam_leniency <- exam_leniency[au_apps - n_apps > 0 & n_examiners >= 5]
exam_leniency[, loo_grant_rate := (au_grants - n_grants) / (au_apps - n_apps)]
exam_leniency <- exam_leniency[, .(art_unit, filing_year, examiner_id, loo_grant_rate)]
rm(exam_stats, au_yr); gc()

patent_leniency <- patex[granted == 1 & !is.na(patent_id) & patent_id != "",
                         .(patent_id, examiner_id, art_unit, filing_year)]
patent_leniency <- exam_leniency[patent_leniency, on = .(examiner_id, art_unit, filing_year), nomatch = 0]
rm(patex, exam_leniency); gc()

ind_leniency <- patent_leniency |>
  inner_join(patent_firm |> select(patent_id, gvkey, issue_year) |> distinct(), by = "patent_id") |>
  inner_join(comp |> select(gvkey, fyear, sic3) |> distinct(), by = c("gvkey", "issue_year" = "fyear")) |>
  group_by(sic3, issue_year) |>
  summarise(avg_leniency = mean(loo_grant_rate, na.rm = TRUE), n_examined = n(), .groups = "drop") |>
  rename(year = issue_year)
rm(patent_leniency); gc()
cat("  Leniency:", nrow(ind_leniency), "\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  SECTION B: INDUSTRY AGGREGATION & LP-IV (SIC3)                         ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ======================================================================
# B1: INDUSTRY AGGREGATION
# ======================================================================
cat("\n=== B1: Industry aggregation (SIC3) ===\n")

ind_compustat <- panel |> filter(has_patents) |>
  group_by(sic3, fyear) |>
  summarise(n_firms = n(),
            ind_rd_intensity = sum(ifelse(is.na(xrd), 0, xrd)) / sum(sale),
            ind_avg_markup = weighted.mean(markup_translog, sale, na.rm = TRUE),
            ind_avg_markup_simple = weighted.mean(markup, sale, na.rm = TRUE),
            .groups = "drop") |> rename(year = fyear)

ind_def <- patent_firm |> filter(issue_year >= 1980, has_kpst) |>
  inner_join(comp |> select(gvkey, fyear, sic3) |> distinct(), by = c("gvkey", "issue_year" = "fyear")) |>
  group_by(sic3, issue_year) |>
  summarise(ind_n_patents = n(), ind_def_share = mean(is_nonbreakthrough, na.rm = TRUE), .groups = "drop") |>
  rename(year = issue_year)

ind_compustat <- ind_compustat |> mutate(across(c(ind_avg_markup, ind_rd_intensity, ind_avg_markup_simple), winsorize))
ind_def <- ind_def |> mutate(ind_def_share = winsorize(ind_def_share))

analysis <- ind_compustat |> filter(!is.na(ind_avg_markup)) |>
  inner_join(ind_def, by = c("sic3","year")) |>
  left_join(ind_leniency, by = c("sic3","year")) |>
  left_join(ind_bds, by = c("sic3","year")) |>
  left_join(ind_tfp, by = c("sic3","year")) |>
  filter(year >= 1983, year <= 2023) |>
  mutate(log_n_firms = log(n_firms + 1), log_n_patents = log(ind_n_patents + 1))

analysis <- analysis |> arrange(sic3, year) |> group_by(sic3) |>
  mutate(lag1_markup = lag(ind_avg_markup), lag1_def = lag(ind_def_share),
         lag1_rd = lag(ind_rd_intensity),
         lag1_entry = lag(entry_rate), lag1_exit = lag(exit_rate),
         lag1_realloc = lag(realloc_rate), lag1_jcb = lag(jc_births_rate),
         lag1_jdd = lag(jd_deaths_rate), lag1_fdeath = lag(firmdeath_rate),
         lag1_tfp = lag(avg_tfp)) |> ungroup()

iv_sample <- analysis |> filter(!is.na(avg_leniency), n_examined >= 5, year >= 1985, year <= 2022)
cat("  IV sample:", nrow(iv_sample), "| Industries:", n_distinct(iv_sample$sic3), "\n")

# ======================================================================
# B2: FIRST STAGE
# ======================================================================
cat("\n=== B2: First stage ===\n")
fs <- feols(ind_def_share ~ avg_leniency + lag1_def + ind_rd_intensity + lag1_rd +
              log_n_firms + log_n_patents + lag1_markup | sic3 + year,
            data = iv_sample |> filter(!is.na(lag1_def), !is.na(lag1_rd),
                                       !is.na(log_n_firms), !is.na(log_n_patents), !is.na(lag1_markup)),
            cluster = ~sic3)
t_val <- coef(fs)["avg_leniency"] / se(fs)["avg_leniency"]
cat("  F:", round(t_val^2, 1), "\n")
print(summary(fs))

bal <- feols(ind_rd_intensity ~ avg_leniency + lag1_def + lag1_rd + log_n_firms + log_n_patents + lag1_markup | sic3 + year,
             data = iv_sample |> filter(!is.na(lag1_def), !is.na(lag1_rd),
                                        !is.na(log_n_firms), !is.na(log_n_patents), !is.na(lag1_markup)),
             cluster = ~sic3)
cat("  Balance p:", round(pvalue(bal)["avg_leniency"], 3), "\n")

# ======================================================================
# B3: LP-IV (FULL SAMPLE 1985-2022)
# ======================================================================
cat("\n=== B3: LP-IV (full sample) ===\n")

lp_data <- analysis |> arrange(sic3, year)
outcomes <- list(c("mk","ind_avg_markup"), c("mks","ind_avg_markup_simple"),
                 c("entry","entry_rate"), c("exit","exit_rate"), c("realloc","realloc_rate"),
                 c("jcb","jc_births_rate"), c("jdd","jd_deaths_rate"), c("fdeath","firmdeath_rate"),
                 c("tfp","avg_tfp"))
for (h in 0:H) for (oc in outcomes) {
  lp_data <- lp_data |> group_by(sic3) |>
    mutate(!!paste0(oc[1], "_h", h) := lead(.data[[oc[2]]], h) - lag(.data[[oc[2]]], 1)) |> ungroup()
}
lp_iv <- lp_data |> filter(!is.na(avg_leniency), n_examined >= 5, year >= 1985, year <= 2022)

run_lp <- function(data, prefix, lag1_out = NULL, fe = "sic3") {
  map_dfr(0:H, function(h) {
    dep <- paste0(prefix, "_h", h)
    ctrl <- "lag1_def + ind_rd_intensity + lag1_rd + log_n_firms + log_n_patents"
    if (!is.null(lag1_out)) ctrl <- paste0(lag1_out, " + ", ctrl)
    f <- as.formula(paste0(dep, " ~ ", ctrl, " | ", fe, " + year | ind_def_share ~ avg_leniency"))
    d <- data |> filter(!is.na(!!sym(dep)), !is.na(lag1_def), !is.na(lag1_rd),
                        !is.na(log_n_firms), !is.na(log_n_patents))
    if (!is.null(lag1_out)) d <- d |> filter(!is.na(!!sym(lag1_out)))
    reg <- tryCatch(feols(f, data = d, cluster = as.formula(paste0("~", fe))), error = function(e) NULL)
    tibble(h = h, b = if(!is.null(reg)) coef(reg)["fit_ind_def_share"] else NA,
           se = if(!is.null(reg)) se(reg)["fit_ind_def_share"] else NA,
           n = if(!is.null(reg)) reg$nobs else NA, lo = b - z90*se, hi = b + z90*se)
  })
}

lp_mk      <- run_lp(lp_iv, "mk", "lag1_markup")
lp_mks     <- run_lp(lp_iv, "mks", NULL)
lp_tfp     <- run_lp(lp_iv, "tfp", "lag1_tfp")
lp_entry   <- run_lp(lp_iv, "entry", "lag1_entry")
lp_exit    <- run_lp(lp_iv, "exit", "lag1_exit")
lp_realloc <- run_lp(lp_iv, "realloc", "lag1_realloc")
lp_jcb     <- run_lp(lp_iv, "jcb", "lag1_jcb")
lp_jdd     <- run_lp(lp_iv, "jdd", "lag1_jdd")
lp_fdeath  <- run_lp(lp_iv, "fdeath", "lag1_fdeath")

cat("Markups:\n"); print(lp_mk)
cat("TFP:\n"); print(lp_tfp)

# ======================================================================
# B4: LP-IV (2000+ SUBSAMPLE)
# ======================================================================
cat("\n=== B4: LP-IV (2000+) ===\n")
lp_iv_2000 <- lp_iv |> filter(year >= 2000)

fs_2000 <- feols(ind_def_share ~ avg_leniency + lag1_def + ind_rd_intensity + lag1_rd +
                   log_n_firms + log_n_patents + lag1_markup | sic3 + year,
                 data = lp_iv_2000 |> filter(!is.na(lag1_def), !is.na(lag1_rd),
                                             !is.na(log_n_firms), !is.na(log_n_patents), !is.na(lag1_markup)),
                 cluster = ~sic3)
t_2000 <- coef(fs_2000)["avg_leniency"] / se(fs_2000)["avg_leniency"]
cat("  F (2000+):", round(t_2000^2, 1), "| N:", nrow(lp_iv_2000), "\n")

lp_mk_2000  <- run_lp(lp_iv_2000, "mk", "lag1_markup")
lp_tfp_2000 <- run_lp(lp_iv_2000, "tfp", "lag1_tfp")

cat("Markups 2000+:\n"); print(lp_mk_2000)
cat("TFP 2000+:\n"); print(lp_tfp_2000)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  SECTION C: SIC4 MARKUP & TFP ESTIMATION                               ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ======================================================================
# C1: TRANSLOG MARKUPS AT SIC4
# ======================================================================
cat("\n=== C1: Translog markups (SIC4) ===\n")
markup_data_4 <- panel |>
  filter(!is.na(sale), sale > 0, !is.na(cogs), cogs > 0, !is.na(ppegt), ppegt > 0) |>
  mutate(lrev = log(sale), lcogs = log(cogs), lcogs2 = log(cogs)^2, lk = log(ppegt))

sic4_mk_counts <- markup_data_4 |> count(sic4) |> filter(n >= 100)
markup_data_4 <- markup_data_4 |> filter(sic4 %in% sic4_mk_counts$sic4)

markup_est_4 <- markup_data_4 |>
  group_by(sic4) |>
  group_modify(~{
    reg <- tryCatch(lm(lrev ~ lcogs + lcogs2 + lk, data = .x), error = function(e) NULL)
    if (is.null(reg)) return(.x |> mutate(markup_translog_4 = NA_real_))
    alpha <- coef(reg)["lcogs"] + 2 * coef(reg)["lcogs2"] * .x$lcogs
    .x |> mutate(markup_translog_4 = alpha / (cogs / sale))
  }) |> ungroup() |> select(gvkey, fyear, markup_translog_4)

panel <- panel |> left_join(markup_est_4, by = c("gvkey","fyear"))
cat("  SIC4 markups:", sum(!is.na(panel$markup_translog_4)), "\n")
rm(markup_data_4, markup_est_4, sic4_mk_counts)

# ======================================================================
# C2: TFP AT SIC4 (HP-smoothed)
# ======================================================================
cat("\n=== C2: TFP (SIC4, HP-smoothed) ===\n")
tfp_data_4 <- panel |>
  filter(!is.na(sale), sale > 0, !is.na(emp), emp > 0,
         !is.na(ppegt), ppegt > 0, !is.na(cogs), cogs > 0, fyear >= 1980) |>
  mutate(log_va = log(sale - cogs + 1), log_k = log(ppegt), log_l = log(emp)) |>
  filter(!is.na(log_va), is.finite(log_va))

sic4_tfp_counts <- tfp_data_4 |> count(sic4) |> filter(n >= 100)
tfp_data_4 <- tfp_data_4 |> filter(sic4 %in% sic4_tfp_counts$sic4)
cat("  SIC4 TFP sample:", nrow(tfp_data_4), ",", n_distinct(tfp_data_4$sic4), "industries\n")

tfp_est_4 <- tfp_data_4 |>
  group_by(sic4) |>
  group_modify(~{
    reg <- tryCatch(lm(log_va ~ log_k + log_l + factor(fyear), data = .x), error = function(e) NULL)
    if (is.null(reg)) return(.x |> mutate(tfp_4_raw = NA_real_))
    .x |> mutate(tfp_4_raw = residuals(reg))
  }) |> ungroup() |> select(gvkey, fyear, tfp_4_raw)

# HP smooth
tfp_est_4 <- tfp_est_4 |>
  arrange(gvkey, fyear) |>
  group_by(gvkey) |>
  mutate(tfp_4 = if (sum(!is.na(tfp_4_raw)) >= 5) hp_smooth(tfp_4_raw) else tfp_4_raw) |>
  ungroup() |> select(gvkey, fyear, tfp_4)

panel <- panel |> left_join(tfp_est_4, by = c("gvkey","fyear"))
cat("  SIC4 TFP:", sum(!is.na(panel$tfp_4)), "\n")
rm(tfp_data_4, tfp_est_4, sic4_tfp_counts)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  SECTION D: FIRM-LEVEL TESTS                                            ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n=== D: Firm-level tests ===\n")

firm_data <- panel |>
  filter(has_patents, n_scored >= 3, fyear >= 1985,
         !is.na(sale), !is.na(at), at > 0, !is.na(emp), emp > 0) |>
  mutate(roa = (sale - cogs) / at,
         productive_rd = rd_intensity * (1 - def_share),
         defensive_rd = rd_intensity * def_share,
         size_tercile = ntile(log_sale, 3),
         log_lp = log(sale / emp), log_cogs_emp = log(cogs / emp)) |>
  arrange(gvkey, fyear) |> group_by(gvkey) |>
  mutate(dlp3 = lead(log_lp, 3) - log_lp,
         dcogs3 = lead(log_cogs_emp, 3) - log_cogs_emp,
         d_markup5 = lead(markup_translog, 5) - markup_translog) |>
  ungroup() |>
  left_join(tfp_estimates |> select(gvkey, fyear, tfp), by = c("gvkey","fyear")) |>
  arrange(gvkey, fyear) |> group_by(gvkey) |>
  mutate(dtfp3 = lead(tfp, 3) - tfp) |> ungroup()
cat("  Firm sample:", nrow(firm_data), "\n")

# --- Mechanism: Sales/emp vs COGS/emp ---
reg_sales <- feols(dlp3 ~ productive_rd + defensive_rd | sic3 + fyear, data = firm_data, cluster = ~gvkey)
reg_cogs  <- feols(dcogs3 ~ productive_rd + defensive_rd | sic3 + fyear, data = firm_data, cluster = ~gvkey)
reg_tfp_f <- feols(dtfp3 ~ productive_rd + defensive_rd | sic3 + fyear, data = firm_data, cluster = ~gvkey)
cat("\nSales/emp:\n"); print(summary(reg_sales))
cat("\nCOGS/emp:\n"); print(summary(reg_cogs))
cat("\nTFP:\n"); print(summary(reg_tfp_f))

# --- Markup interaction (Prediction 4) ---
reg_mu_int <- feols(markup_translog ~ productive_rd * log_sale + defensive_rd * log_sale | sic3 + fyear,
                    data = firm_data |> filter(!is.na(markup_translog), markup_translog > 0.5, markup_translog < 5),
                    cluster = ~gvkey)
cat("\nMarkup interaction:\n"); print(summary(reg_mu_int))

# --- Forward markup interaction ---
reg_fwd <- feols(d_markup5 ~ productive_rd * log_sale + defensive_rd * log_sale | sic3 + fyear,
                 data = firm_data |> filter(!is.na(d_markup5), !is.na(markup_translog),
                                            markup_translog > 0.5, markup_translog < 5),
                 cluster = ~gvkey)
cat("\nForward markup interaction:\n"); print(summary(reg_fwd))

# --- Size gradient ---
size_reg <- feols(def_share ~ log_sale | sic3 + fyear,
                  data = panel |> filter(has_patents, n_scored >= 3, fyear >= 1985), cluster = ~gvkey)
cat("\nSize gradient:", round(coef(size_reg)["log_sale"], 5),
    "p =", format.pval(pvalue(size_reg)["log_sale"], 3), "\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  SECTION E: FIGURES                                                     ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n=== E: Figures ===\n")

# Trends
mk_trend <- panel |> filter(fyear >= 1980, fyear <= 2022,
                            !is.na(markup_translog), markup_translog > 0.5, markup_translog < 5) |>
  group_by(fyear) |>
  summarise(avg = weighted.mean(markup_translog, sale, na.rm = TRUE),
            med = median(markup_translog, na.rm = TRUE), .groups = "drop")

ggsave("figures/fig_markup_trend.pdf",
       mk_trend |> ggplot(aes(x = fyear)) +
         geom_line(aes(y = avg, color = "Mean"), linewidth = 0.8) +
         geom_line(aes(y = med, color = "Median"), linewidth = 0.6, linetype = "dashed") +
         scale_color_manual(values = c(Mean = "#2166AC", Median = "#B2182B")) +
         labs(x = NULL, y = "Markup (translog)", title = "Markup Trends") + theme_econ(),
       width = 7, height = 4.5)

bds_total <- bds_sec |> group_by(year) |>
  summarise(entry_rate = weighted.mean(estabs_entry_rate, firms, na.rm = TRUE),
            exit_rate = weighted.mean(estabs_exit_rate, firms, na.rm = TRUE), .groups = "drop") |>
  filter(year <= 2023)

ggsave("figures/fig_bds_trend.pdf",
       bds_total |> pivot_longer(c(entry_rate, exit_rate), names_to = "m", values_to = "r") |>
         mutate(m = ifelse(m == "entry_rate", "Entry", "Exit")) |>
         ggplot(aes(x = year, y = r, color = m)) + geom_line(linewidth = 0.8) +
         scale_color_manual(values = c(Entry = "#2166AC", Exit = "#B2182B")) +
         labs(x = NULL, y = "Rate (%)", title = "Establishment Entry and Exit Rates") + theme_econ(),
       width = 7, height = 4.5)

def_trend <- ind_def |> group_by(year) |>
  summarise(avg_def = weighted.mean(ind_def_share, ind_n_patents, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1985, year <= 2022)
ggsave("figures/fig_def_trend.pdf",
       def_trend |> ggplot(aes(x = year, y = avg_def)) +
         geom_line(linewidth = 0.8, color = "#2166AC") +
         scale_y_continuous(labels = scales::percent_format(1)) +
         labs(x = NULL, y = "Non-breakthrough share", title = "Defensive R&D Share") + theme_econ(),
       width = 7, height = 4.5)

# LP-IV plots
ggsave("figures/fig_lp_iv_markup.pdf", plot_irf(lp_mk, "Effect on markup", "LP-IV: Defensive R&D -> Markups"), width = 6.5, height = 4.5)
ggsave("figures/fig_lp_iv_tfp.pdf", plot_irf(lp_tfp, "Effect on TFP", "LP-IV: Defensive R&D -> TFP", "#7570B3"), width = 6.5, height = 4.5)

all_bds <- bind_rows(
  lp_entry |> mutate(out = "Estab. entry"), lp_exit |> mutate(out = "Estab. exit"),
  lp_jcb |> mutate(out = "JC (births)"), lp_jdd |> mutate(out = "JD (deaths)"),
  lp_realloc |> mutate(out = "Gross realloc."), lp_fdeath |> mutate(out = "Firm death")
) |> mutate(out = factor(out, levels = c("Estab. entry","Estab. exit","JC (births)","JD (deaths)","Gross realloc.","Firm death")))

ggsave("figures/fig_lp_iv_bds_all.pdf",
       all_bds |> ggplot(aes(x = h, y = b)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
         geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#B2182B", alpha = 0.12) +
         geom_line(linewidth = 0.7, color = "#B2182B") + geom_point(size = 1.5, color = "#B2182B") +
         facet_wrap(~out, scales = "free_y", ncol = 3) +
         labs(x = "Horizon", y = "Cumulative effect", title = "LP-IV: Defensive R&D -> Business Dynamism") +
         theme_econ() + theme(strip.text = element_text(face = "bold")),
       width = 12, height = 8)

ggsave("figures/fig_lp_iv_markup_comparison.pdf",
       bind_rows(lp_mk |> mutate(type = "Translog"), lp_mks |> mutate(type = "Simple")) |>
         ggplot(aes(x = h, y = b, color = type, fill = type)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.1, color = NA) +
         geom_line(linewidth = 0.7) + geom_point(size = 2) +
         scale_color_manual(values = c(Translog = "#2166AC", Simple = "#B2182B")) +
         scale_fill_manual(values = c(Translog = "#2166AC", Simple = "#B2182B")) +
         labs(x = "Horizon", y = "Effect on markup", title = "Translog vs Simple Markup") + theme_econ(),
       width = 7, height = 5)

# 2000+ comparison
ggsave("figures/fig_lp_iv_markup_2000plus.pdf",
       bind_rows(lp_mk |> mutate(period = "1985-2022"), lp_mk_2000 |> mutate(period = "2000-2022")) |>
         ggplot(aes(x = h, y = b, color = period, fill = period)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.1, color = NA) +
         geom_line(linewidth = 0.7) + geom_point(size = 2) +
         scale_color_manual(values = c("1985-2022" = "#2166AC", "2000-2022" = "#B2182B")) +
         scale_fill_manual(values = c("1985-2022" = "#2166AC", "2000-2022" = "#B2182B")) +
         labs(x = "Horizon", y = "Effect on markup", title = "LP-IV Markups: Full vs 2000+") + theme_econ(),
       width = 7, height = 5)

# Mechanism bar chart
channel_data <- tibble(
  outcome = rep(c("Sales/emp", "COGS/emp"), each = 2),
  type = rep(c("Productive", "Defensive"), 2),
  b = c(coef(reg_sales)["productive_rd"], coef(reg_sales)["defensive_rd"],
        coef(reg_cogs)["productive_rd"], coef(reg_cogs)["defensive_rd"]),
  se = c(se(reg_sales)["productive_rd"], se(reg_sales)["defensive_rd"],
         se(reg_cogs)["productive_rd"], se(reg_cogs)["defensive_rd"])
) |> mutate(lo = b - z90*se, hi = b + z90*se)

ggsave("figures/fig_markup_channel.pdf",
       channel_data |> ggplot(aes(x = outcome, y = b, fill = type)) +
         geom_col(position = position_dodge(0.7), width = 0.6) +
         geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.7), width = 0.2) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         scale_fill_manual(values = c(Productive = "#2166AC", Defensive = "#B2182B")) +
         labs(x = NULL, y = "3-year growth", title = "Markup Channel") + theme_econ(),
       width = 7, height = 5)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  SECTION F: PRINTED OUTPUT                                              ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n\n")
cat("================================================================\n")
cat("RESULTS SUMMARY\n")
cat("================================================================\n\n")
cat("Panel:", nrow(panel), "| Patents:", nrow(patent_firm), "| IV:", nrow(iv_sample), "\n")
cat("First-stage F:", round(t_val^2, 1), "| Balance p:", round(pvalue(bal)["avg_leniency"], 3), "\n")
cat("First-stage F (2000+):", round(t_2000^2, 1), "\n\n")

print_lp <- function(tbl, label) {
  cat(paste0(label, ":\n"))
  for (hh in 0:8) { r <- tbl |> filter(h == hh)
  sig <- ifelse(!is.na(r$lo) & (r$lo > 0 | r$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, r$b, r$se, r$lo, r$hi, r$n, sig))
  }
}

print_lp(lp_mk, "LP-IV MARKUPS (full)")
print_lp(lp_tfp, "LP-IV TFP (full)")
print_lp(lp_mk_2000, "LP-IV MARKUPS (2000+)")
print_lp(lp_tfp_2000, "LP-IV TFP (2000+)")

for (nm in c("entry","exit","jcb","jdd","realloc","fdeath")) {
  print_lp(get(paste0("lp_", nm)), paste0("BDS: ", toupper(nm)))
}

cat("\nSIC4 coverage:\n")
cat("  Markup:", round(100*mean(!is.na(panel$markup_translog_4)), 1), "%\n")
cat("  TFP:", round(100*mean(!is.na(panel$tfp_4)), 1), "%\n")

cat("\nDone.\n")





# ==============================================================================
# LP-IV PRE-2000 (instrument period 1985-1999, outcomes through 2005)
# ==============================================================================

# Need to rebuild dep vars with longer forward leads
# Use the full analysis data but only instrument from pre-2000
lp_iv_pre2000 <- lp_iv |> filter(year < 2000)

cat("Pre-2000 IV sample:", nrow(lp_iv_pre2000), "\n")
cat("First-stage F:", round(t_pre2000^2, 1), "\n")

# Extend horizon to capture effects into 2000s
H_pre <- 12

# Build extended dep vars on full analysis data first
lp_data_ext <- analysis |> arrange(sic3, year)
for (h in 0:H_pre) {
  for (oc in list(c("mk","ind_avg_markup"), c("tfp","avg_tfp"),
                  c("entry","entry_rate"), c("exit","exit_rate"),
                  c("realloc","realloc_rate"))) {
    lp_data_ext <- lp_data_ext |> group_by(sic3) |>
      mutate(!!paste0(oc[1], "_h", h) := lead(.data[[oc[2]]], h) - lag(.data[[oc[2]]], 1)) |>
      ungroup()
  }
}

lp_iv_pre <- lp_data_ext |> filter(!is.na(avg_leniency), n_examined >= 5, year >= 1985, year < 2000)

run_lp_pre <- function(prefix, lag1_out = NULL) {
  map_dfr(0:H_pre, function(h) {
    dep <- paste0(prefix, "_h", h)
    ctrl <- "lag1_def + ind_rd_intensity + lag1_rd + log_n_firms + log_n_patents"
    if (!is.null(lag1_out)) ctrl <- paste0(lag1_out, " + ", ctrl)
    f <- as.formula(paste0(dep, " ~ ", ctrl, " | sic3 + year | ind_def_share ~ avg_leniency"))
    d <- lp_iv_pre |> filter(!is.na(!!sym(dep)), !is.na(lag1_def), !is.na(lag1_rd),
                             !is.na(log_n_firms), !is.na(log_n_patents))
    if (!is.null(lag1_out)) d <- d |> filter(!is.na(!!sym(lag1_out)))
    reg <- tryCatch(feols(f, data = d, cluster = ~sic3), error = function(e) NULL)
    tibble(h = h, b = if (!is.null(reg)) coef(reg)["fit_ind_def_share"] else NA,
           se = if (!is.null(reg)) se(reg)["fit_ind_def_share"] else NA,
           n = if (!is.null(reg)) reg$nobs else NA, lo = b - z90 * se, hi = b + z90 * se)
  })
}

lp_mk_pre      <- run_lp_pre("mk", "lag1_markup")
lp_tfp_pre     <- run_lp_pre("tfp", "lag1_tfp")
lp_entry_pre   <- run_lp_pre("entry", "lag1_entry")
lp_exit_pre    <- run_lp_pre("exit", "lag1_exit")
lp_realloc_pre <- run_lp_pre("realloc", "lag1_realloc")

# Print
cat("\nLP-IV MARKUPS pre-2000 (90% CI, h=0..12):\n")
for (hh in 0:H_pre) {
  row <- lp_mk_pre |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}

cat("\nLP-IV TFP pre-2000 (90% CI, h=0..12):\n")
for (hh in 0:H_pre) {
  row <- lp_tfp_pre |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}

# Figures
plot_irf_ext <- function(tbl, ylab, title, color = "#2166AC") {
  tbl |> ggplot(aes(x = h, y = b)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = color, alpha = 0.15) +
    geom_line(linewidth = 0.8, color = color) +
    geom_point(size = 2, color = color) +
    labs(x = "Horizon (years)", y = ylab, title = title) +
    scale_x_continuous(breaks = 0:12) + theme_econ()
}

ggsave("figures/fig_lp_iv_markup_pre2000.pdf",
       plot_irf_ext(lp_mk_pre, "Effect on markup", "LP-IV Markups (instrument: 1985-1999)"),
       width = 7, height = 4.5)

ggsave("figures/fig_lp_iv_tfp_pre2000.pdf",
       plot_irf_ext(lp_tfp_pre, "Effect on TFP", "LP-IV TFP (instrument: 1985-1999)", "#7570B3"),
       width = 7, height = 4.5)

# Comparison: full vs pre-2000
ggsave("figures/fig_lp_iv_markup_full_vs_pre2000.pdf",
       bind_rows(lp_mk |> mutate(period = "Full (1985-2022)"),
                 lp_mk_pre |> filter(h <= 8) |> mutate(period = "Pre-2000 (F=30)")) |>
         ggplot(aes(x = h, y = b, color = period, fill = period)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.1, color = NA) +
         geom_line(linewidth = 0.7) + geom_point(size = 2) +
         scale_color_manual(values = c("Full (1985-2022)" = "#2166AC", "Pre-2000 (F=30)" = "#B2182B")) +
         scale_fill_manual(values = c("Full (1985-2022)" = "#2166AC", "Pre-2000 (F=30)" = "#B2182B")) +
         scale_x_continuous(breaks = 0:12) +
         labs(x = "Horizon (years)", y = "Effect on markup",
              title = "LP-IV Markups: Full Sample vs Pre-2000 Instrument") + theme_econ(),
       width = 8, height = 5)




# ==============================================================================
# LP-IV WITH CONTINUOUS QUALITY MEASURE (lqsim05)
# ==============================================================================

# Aggregate continuous quality to industry-year
ind_quality <- patent_firm |> filter(issue_year >= 1980, !is.na(importance)) |>
  inner_join(comp |> select(gvkey, fyear, sic3) |> distinct(), by = c("gvkey", "issue_year" = "fyear")) |>
  group_by(sic3, issue_year) |>
  summarise(ind_avg_quality = mean(importance, na.rm = TRUE),
            n_quality = n(), .groups = "drop") |>
  rename(year = issue_year) |>
  mutate(ind_avg_quality = winsorize(ind_avg_quality))

# Merge to analysis
analysis_q <- analysis |>
  left_join(ind_quality, by = c("sic3", "year")) |>
  arrange(sic3, year) |> group_by(sic3) |>
  mutate(lag1_quality = lag(ind_avg_quality)) |> ungroup()

# Build dep vars (quality is HIGHER = more important, so sign flips vs def_share)
# We want NEGATIVE quality = more defensive, so use negative or just interpret
lp_data_q <- analysis_q |> arrange(sic3, year)
for (h in 0:H) {
  for (oc in list(c("mk","ind_avg_markup"), c("tfp","avg_tfp"),
                  c("entry","entry_rate"), c("exit","exit_rate"),
                  c("realloc","realloc_rate"))) {
    lp_data_q <- lp_data_q |> group_by(sic3) |>
      mutate(!!paste0(oc[1], "_h", h) := lead(.data[[oc[2]]], h) - lag(.data[[oc[2]]], 1)) |>
      ungroup()
  }
}

iv_q <- lp_data_q |> filter(!is.na(avg_leniency), n_examined >= 5,
                            !is.na(ind_avg_quality), year >= 1985, year <= 2022)
cat("Quality IV sample:", nrow(iv_q), "\n")

# First stage: does leniency predict continuous quality?
fs_q <- feols(ind_avg_quality ~ avg_leniency + lag1_quality + ind_rd_intensity + lag1_rd +
                log_n_firms + log_n_patents + lag1_markup | sic3 + year,
              data = iv_q |> filter(!is.na(lag1_quality), !is.na(lag1_rd),
                                    !is.na(log_n_firms), !is.na(log_n_patents), !is.na(lag1_markup)),
              cluster = ~sic3)
t_q <- coef(fs_q)["avg_leniency"] / se(fs_q)["avg_leniency"]
cat("First-stage F (quality):", round(t_q^2, 1), "\n")
print(summary(fs_q))

# Run LP-IV with continuous quality as treatment
run_lp_q <- function(prefix, lag1_out = NULL) {
  map_dfr(0:H, function(h) {
    dep <- paste0(prefix, "_h", h)
    ctrl <- "lag1_quality + ind_rd_intensity + lag1_rd + log_n_firms + log_n_patents"
    if (!is.null(lag1_out)) ctrl <- paste0(lag1_out, " + ", ctrl)
    f <- as.formula(paste0(dep, " ~ ", ctrl, " | sic3 + year | ind_avg_quality ~ avg_leniency"))
    d <- iv_q |> filter(!is.na(!!sym(dep)), !is.na(lag1_quality), !is.na(lag1_rd),
                        !is.na(log_n_firms), !is.na(log_n_patents))
    if (!is.null(lag1_out)) d <- d |> filter(!is.na(!!sym(lag1_out)))
    reg <- tryCatch(feols(f, data = d, cluster = ~sic3), error = function(e) NULL)
    tibble(h = h, b = if (!is.null(reg)) coef(reg)["fit_ind_avg_quality"] else NA,
           se = if (!is.null(reg)) se(reg)["fit_ind_avg_quality"] else NA,
           n = if (!is.null(reg)) reg$nobs else NA, lo = b - z90 * se, hi = b + z90 * se)
  })
}

lp_mk_q      <- run_lp_q("mk", "lag1_markup")
lp_tfp_q     <- run_lp_q("tfp", "lag1_tfp")
lp_entry_q   <- run_lp_q("entry", "lag1_entry")
lp_exit_q    <- run_lp_q("exit", "lag1_exit")
lp_realloc_q <- run_lp_q("realloc", "lag1_realloc")

# Note: signs are FLIPPED vs def_share. Higher quality = more productive.
# So negative coeff on markups = higher quality lowers markups (consistent)
# Positive coeff on TFP = higher quality raises TFP (consistent)

cat("\nLP-IV MARKUPS — continuous quality (90% CI):\n")
cat("(Negative = higher quality lowers markups)\n")
for (hh in 0:8) {
  row <- lp_mk_q |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}

cat("\nLP-IV TFP — continuous quality (90% CI):\n")
cat("(Positive = higher quality raises TFP)\n")
for (hh in 0:8) {
  row <- lp_tfp_q |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}

cat("\nLP-IV ENTRY — continuous quality (90% CI):\n")
for (hh in 0:8) {
  row <- lp_entry_q |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.4f (%6.4f) [%7.4f, %7.4f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}



# ==============================================================================
# PLOTS: CONTINUOUS QUALITY LP-IV (sign flipped: negative quality = defensive)
# ==============================================================================

# Flip signs so positive = more defensive (comparable to def_share results)
lp_mk_q_flip    <- lp_mk_q |> mutate(b = -b, lo_old = lo, lo = -hi, hi = -lo_old) |> select(-lo_old)
lp_tfp_q_flip   <- lp_tfp_q |> mutate(b = -b, lo_old = lo, lo = -hi, hi = -lo_old) |> select(-lo_old)
lp_entry_q_flip <- lp_entry_q |> mutate(b = -b, lo_old = lo, lo = -hi, hi = -lo_old) |> select(-lo_old)
lp_exit_q_flip  <- lp_exit_q |> mutate(b = -b, lo_old = lo, lo = -hi, hi = -lo_old) |> select(-lo_old)
lp_realloc_q_flip <- lp_realloc_q |> mutate(b = -b, lo_old = lo, lo = -hi, hi = -lo_old) |> select(-lo_old)

# Markup: binary vs continuous
ggsave("figures/fig_lp_iv_markup_binary_vs_continuous.pdf",
       bind_rows(lp_mk |> mutate(measure = "Binary (non-BT share)"),
                 lp_mk_q_flip |> mutate(measure = "Continuous (neg. quality)")) |>
         ggplot(aes(x = h, y = b, color = measure, fill = measure)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.1, color = NA) +
         geom_line(linewidth = 0.7) + geom_point(size = 2) +
         scale_color_manual(values = c("Binary (non-BT share)" = "#2166AC",
                                       "Continuous (neg. quality)" = "#B2182B")) +
         scale_fill_manual(values = c("Binary (non-BT share)" = "#2166AC",
                                      "Continuous (neg. quality)" = "#B2182B")) +
         scale_x_continuous(breaks = 0:8) +
         labs(x = "Horizon (years)", y = "Effect on markup",
              title = "LP-IV Markups: Binary vs Continuous Quality Measure") + theme_econ(),
       width = 8, height = 5)

# TFP: binary vs continuous
ggsave("figures/fig_lp_iv_tfp_binary_vs_continuous.pdf",
       bind_rows(lp_tfp |> mutate(measure = "Binary (non-BT share)"),
                 lp_tfp_q_flip |> mutate(measure = "Continuous (neg. quality)")) |>
         ggplot(aes(x = h, y = b, color = measure, fill = measure)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.1, color = NA) +
         geom_line(linewidth = 0.7) + geom_point(size = 2) +
         scale_color_manual(values = c("Binary (non-BT share)" = "#2166AC",
                                       "Continuous (neg. quality)" = "#B2182B")) +
         scale_fill_manual(values = c("Binary (non-BT share)" = "#2166AC",
                                      "Continuous (neg. quality)" = "#B2182B")) +
         scale_x_continuous(breaks = 0:8) +
         labs(x = "Horizon (years)", y = "Effect on TFP",
              title = "LP-IV TFP: Binary vs Continuous Quality Measure") + theme_econ(),
       width = 8, height = 5)

# BDS: entry + exit + reallocation for continuous
ggsave("figures/fig_lp_iv_bds_continuous.pdf",
       bind_rows(lp_entry_q_flip |> mutate(out = "Estab. entry"),
                 lp_exit_q_flip |> mutate(out = "Estab. exit"),
                 lp_realloc_q_flip |> mutate(out = "Gross realloc.")) |>
         mutate(out = factor(out, levels = c("Estab. entry", "Estab. exit", "Gross realloc."))) |>
         ggplot(aes(x = h, y = b)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
         geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#B2182B", alpha = 0.12) +
         geom_line(linewidth = 0.7, color = "#B2182B") +
         geom_point(size = 1.5, color = "#B2182B") +
         facet_wrap(~out, scales = "free_y", ncol = 3) +
         scale_x_continuous(breaks = 0:8) +
         labs(x = "Horizon (years)", y = "Cumulative effect",
              title = "LP-IV BDS: Continuous Quality Measure (sign flipped)") +
         theme_econ() + theme(strip.text = element_text(face = "bold")),
       width = 12, height = 4.5)

# Standalone IRFs for continuous measure
ggsave("figures/fig_lp_iv_markup_continuous.pdf",
       plot_irf(lp_mk_q_flip, "Effect on markup", "LP-IV Markups (continuous quality, flipped)"),
       width = 6.5, height = 4.5)

ggsave("figures/fig_lp_iv_tfp_continuous.pdf",
       plot_irf(lp_tfp_q_flip, "Effect on TFP", "LP-IV TFP (continuous quality, flipped)", "#7570B3"),
       width = 6.5, height = 4.5)

# Print flipped results
cat("\nLP-IV MARKUPS — continuous (sign-flipped, positive = defensive):\n")
for (hh in 0:8) {
  row <- lp_mk_q_flip |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}

cat("\nLP-IV TFP — continuous (sign-flipped, positive = defensive):\n")
for (hh in 0:8) {
  row <- lp_tfp_q_flip |> filter(h == hh)
  sig <- ifelse(!is.na(row$lo) & (row$lo > 0 | row$hi < 0), "*", "")
  cat(sprintf("  h=%d: %7.3f (%6.3f) [%7.3f, %7.3f] N=%d %s\n", hh, row$b, row$se, row$lo, row$hi, row$n, sig))
}