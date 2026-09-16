proj_name <- 'football'

library(nflfastR)
library(initR)

future::plan("multisession")
nflfastR::update_db(
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con(x.host = 'localhost')
)
