#function to get AIS from BigQuery

get_AIS<-function(year, bq_path, x_max, x_min, y_max, y_min, country, gear_type, hours,bp='facet-ais'){
  
  #establishing a connection with bigquery 
  BQ_connection<-dbConnect(bigquery(),
                           project = 'global-fishing-watch',
                           dataset = 'gfw_public_data',
                           billing = bp)
  
  #custom sql string
  sql_query_txt<-paste0("SELECT date, cell_ll_lat, cell_ll_lon, fishing_hours FROM `global-fishing-watch.gfw_public_data.fishing_effort_v2` WHERE ((`cell_ll_lat` <= ", y_max, " AND `cell_ll_lat` >= ", y_min,") AND (`cell_ll_lon` >= ", x_min, " AND `cell_ll_lon` <= ", x_max,") AND (`flag` = '", country, "' AND `geartype` = '", gear_type,"') AND (`fishing_hours` > ", hours,") AND (`date` >= '", year,"-01-01' AND `date` <= '", year,"-12-31'))")
  
  print(sql_query_txt)

  
  bq_auth(path = bq_path)
  
  
  #querying and returning a dataframe
  ais_df<-dbGetQuery(con = BQ_connection,
                     sql_query_txt)
  
  return(ais_df)
}
