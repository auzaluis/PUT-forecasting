WITH cte AS (
SELECT
    
    -- Core metric
    PUT,
    
    -- Date Info
    start_date,
    days,
    PARSE_DATE('%Y%m%d', CAST(week_start_date AS STRING))     AS week_start_date,
    PARSE_DATE('%Y%m%d', CAST(month_start_date AS STRING))    AS month_start_date,
    PARSE_DATE('%Y%m%d', CAST(quarter_start_date AS STRING))  AS quarter_start_date,
    PARSE_DATE('%Y%m%d', CAST(year_start_date AS STRING))     AS year_start_date,
    PARSE_DATE('%Y%m%d', CAST(season_start_date AS STRING))   AS season_start_date,

    -- Time Info
    quarter_hour,
    hours,
    
    CONCAT(
      CAST(start_date AS STRING), ' ', FORMAT('%02d', hours), ':00:00'
    ) AS full_hour,
    
    CONCAT(
      CAST(start_date AS STRING), ' ', FORMAT('%02d', hours), ':', FORMAT('%02d', quarter_hour), ':00'
    ) AS full_time,

    -- Demographics
    hispanic_flag,
    gender,
    age,
    
    -- Time Aggregations
    daily,
    normal_week,
    weekly,
    monthly,
    quarterly,
    yearly,
    seasonly,

    -- Nielsen Period Start Dates
    MIN(start_date) OVER (PARTITION BY yearly, weekly ORDER BY start_date) AS nielsen_start_week,

    -- Flags
    full_week,
    full_month,
    full_quarter,
    
    -- Broadcast Details
    Stream
FROM
    `dl-datalake-gold-prd.nielsen_gold.gold_nielsen_record_usage_p`
WHERE
    stream = {stream}
    AND hispanic_flag = {hispanic_flag}
    AND age IN ({age*})
    AND hours IN ({daypart*})
    AND start_date BETWEEN {start_date} AND {end_date}
),


quarter_hour_avg AS (
   SELECT
    full_time,
    full_hour,
    daily,

    CAST(
      MIN(Daily) OVER (
      	PARTITION BY
      	normal_week,
  		  monthly,
  		  quarterly,
  		  yearly
      ) AS STRING
    ) AS min_week,

    normal_week,
    monthly,
    quarterly,
    yearly,
    seasonly,
    SUM(PUT) AS quarter_hour_average
  FROM
    cte
  GROUP BY
    full_time,
    full_hour,
    daily,
    normal_week,
    monthly,
    quarterly,
    yearly,
    seasonly
),


interval_avg_base AS(
  SELECT
  	MIN(Daily) AS min_date,
    
    -- Harcoding Quarter-Hour
    CAST(MIN(full_time) AS STRING) AS intervals_dim,
  
  	-- Aggregation metric
  	AVG(quarter_hour_average) AS interval_average
  
  FROM
    quarter_hour_avg
  
  GROUP BY
  	full_time
),

final_df AS (
  SELECT
    intervals_dim,
    interval_average / 1000 AS PUTs
  FROM interval_avg_base

)

SELECT *
FROM final_df
