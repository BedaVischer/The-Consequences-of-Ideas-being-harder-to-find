kpss <- fread("data/KPSS_2024.csv",
              select = c("patent_num", "filing_date", "issue_date", "xi_real", "cites")) |>
  mutate(patent_id = as.character(patent_num)) |> select(-patent_num)
names(kpss) <- tolower(names(kpss))