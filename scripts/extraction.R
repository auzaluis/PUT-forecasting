
# Libraries
library(DBI)
library(bigrquery)
library(glue)
library(dotenv)
library(purrr)
library(arrow)


# Connect to BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project ="dl-datalake-gold-prd",
  dataset ="nielsen_gold" ,
  billing ="dl-datalake-gold-prd" 
)


# Load template2
query_template <- paste(
  readLines("scripts/query_template.sql"),
  collapse = "\n"
)


# Arguments
stream          <- "Live+SDP"
hispanic_flag   <- "National PUTs"
start_date      <- "2021-12-27"
end_date        <- "2026-08-30"
#data_type       <- "Updated BigData"
data_type       <- "Panel"

dayparts <- list(
  daytime = 7:18,
  prime_time = 19:22,
  total_day = c(0:1, 2:23)
)

age <- list(
  `P2+` = c(
    "2-17",
    "18-20", "21-24", "25-29",
    "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-64", "65+"
  ),
  `P18+` = c(
    "18-20", "21-24", "25-29",
    "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-64", "65+"
  ),
  `P18-49` = c(
    "18-20", "21-24", "25-29",
    "30-34", "35-39", "40-44", "45-49"
  )
)

queries <- map2_chr(
  rep(names(age), times = length(dayparts)),
  rep(names(dayparts), each = length(age)),
  ~ {
    glue_sql(
      "SELECT *, {age_label} AS age_range, {daypart_label} AS daypart FROM ({template})",
      age_label = SQL(glue("'{.x}'")),
      daypart_label = SQL(glue("'{.y}'")),
      template = glue_sql(.con = con,
                          query_template,
                          stream = stream,
                          data_type=data_type,## aqui
                          hispanic_flag = hispanic_flag,
                          age = age[[.x]],
                          start_date = start_date,
                          end_date = end_date,
                          daypart = SQL(dayparts[[.y]]))
    )
  }
)

final_query <- paste(
  queries,
  collapse = "\n\n\nUNION ALL\n\n\n"
)

cat(final_query)


# Run the query
df_panel<- dbGetQuery(con, final_query)


# Save the results to a Parquet file
write_parquet(df_panel, "data/raw_data_PUTs_Panel.parquet")
