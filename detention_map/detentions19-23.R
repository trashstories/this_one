
detentions23 <- read_csv("data/detentions23.csv") |> 
  mutate(in_date = mdy(`Detention Book In Date`),
         out_date = mdy(`Detention Book Out Date`),
         det_days = out_date - in_date)

all23 <- detentions23 |> 
  mutate(name = toupper(`Detention Facility`)) |>
  group_by(name) |>
  summarise(n = n()) |>
  mutate(adp_calc = round((n / 365), 0),
         year = 2023)

detentions22 <- read_csv("data/detentions22.csv") |> 
  mutate(in_date = mdy(`Detention Book In Date`),
         out_date = mdy(`Detention Book Out Date`),
         det_days = out_date - in_date)

all22 <- detentions22 |> 
  mutate(name = toupper(`Detention Facility`)) |>
  group_by(name) |>
  summarise(n = n()) |>
  mutate(adp_calc = round((n / 365), 0),
         year = 2022)

detentions21 <- read_csv("data/detentions21.csv") |> 
  mutate(in_date = mdy(`Detention Book In Date`),
         out_date = mdy(`Detention Book Out Date`),
         det_days = out_date - in_date)

all21 <- detentions21 |> 
  mutate(name = toupper(`Detention Facility`)) |>
  group_by(name) |>
  summarise(n = n()) |>
  mutate(adp_calc = round((n / 365), 0),
         year = 2021)

detentions20 <- read_csv("data/detentions20.csv") |> 
  mutate(in_date = mdy(`Detention Book In Date`),
         out_date = mdy(`Detention Book Out Date`),
         det_days = out_date - in_date)

all20 <- detentions20 |> 
  mutate(name = toupper(`Detention Facility`)) |>
  group_by(name) |>
  summarise(n = n()) |>
  mutate(adp_calc = round((n / 365), 0),
         year = 2020)

detentions19 <- read_csv("data/detentions19.csv") |> 
  mutate(in_date = mdy(`Detention Book In Date`),
         out_date = mdy(`Detention Book Out Date`),
         det_days = out_date - in_date)

all19 <- detentions19 |> 
  mutate(name = toupper(`Detention Facility`)) |>
  group_by(name) |>
  summarise(n = n()) |>
  mutate(adp_calc = round((n / 365), 0),
         year = 2019)

all <- all19 |> 
  rbind(all20, all21, all22, all23) |> 
  group_by(name) |> 
  summarise(mean_n = round(mean(n), 0))

write_csv(all, "data/all.csv")