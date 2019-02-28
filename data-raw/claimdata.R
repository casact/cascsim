load('data-raw/claimdata.rda')

save(
    claimdata
  , file = 'data/claimdata.Rdata'
  , compress = 'xz'
)
