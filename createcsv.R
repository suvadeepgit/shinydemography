library(RODBC)

conn.dma = odbcConnect("dma", uid='postgres',
                      pwd='postgres2015', rows_at_time = 1,
                      believeNRows = FALSE)

dma_borders = sqlQuery(conn.dma, paste("SELECT
                                       dma_borders.group,
                                       dma_borders.lon,
                                       dma_borders.lat,
                                       dma_borders.dma_id,
                                       dma_borders.dma_lable
                                       FROM dma_borders ORDER BY id
                                       "))
write.table(dma_borders, "dma_borders.csv", sep="|") 