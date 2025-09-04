
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
  project = Sys.getenv("BQ_PROJECT"),
  dataset = Sys.getenv("BQ_DATASET"),
  billing = Sys.getenv("BQ_BILLING")
)


# Load template
query_template <- paste(
  readLines("scripts/query_template.sql"),
  collapse = "\n"
)


# Arguments
stream          <- "Live+SDP"
hispanic_flag   <- "Hispanic PUTs"
start_date      <- "2021-12-27"
end_date        <- "2025-12-31"

dayparts <- list(
  daytime = 7:18,
  prime_time = 19:22,
  total_day = c(0:1, 7:23)
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
df <- dbGetQuery(con, final_query)


# Save the results to a Parquet file
write_parquet(df, "data/raw_data.parquet")
