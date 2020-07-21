# Sportradar --------------------------------------------------------------

f.sportradar <- paste0("../initR/sportradar_con.rds")
if (file.exists(f.sportradar) == TRUE) {
  sportradar_con <- readRDS(f.sportradar)
}
if (file.exists(f.sportradar) == FALSE) {
  sportradar_sports <-
    c(
      "Cricket",
      "Cycling",
      "F1",
      "Formula E",
      "Global Ice Hockey",
      "Golf",
      "IndyCar",
      "MLB",
      "NASCAR",
      "NBA",
      "NCAA Football",
      "NCAAMB",
      "NCAAMH",
      "NCAAWB",
      "NFL Official",
      "NHL Official",
      "Soccer Americas",
      "Soccer Europe",
      "Soccer International",
      "Tennis"
    )
  sportradar_keys <-
    c(
      NA,
      "wc5e7z38ncfyjehpyy2kg9ug",
      "8nkt9raeetpfr46k3bfdwq3j",
      "a5ux57r4y67jnhrr6myp3y5z",
      "f796s3fd3ejj4fezm3ujnk46",
      "vhcp84c3uunm7ub2cr3777dg",
      "49qmegdbjjsa6sasnxz763be",
      "ecr62bqd76vzftmbmerzw5nm",
      NA,
      "8yxcujp9cjmv5nr4kx3nj3ww",
      "wtwzs9faya4nsqj8e3f8whyj",
      "vx96fywt45m5xpd5mwzjxuda",
      "nws2vguejfj4nc4ecd574pm6",
      NA,
      "qeku77txdetdmy86ncn4cfer",
      "3g6tq73cjqe3dpn6g8amvn5m",
      "dsgmfyc8ebfb973xvvj39rdv",
      "uxxkvfq4t33bws8cstjf5zpa",
      "d7f87fcrr7n8sf6jh8nfnh9c",
      "nrbeyqma2fq25we7uxwjarrr"
    )
  sportradar_db_match <-
    c(
      NA,
      NA,
      NA,
      NA,
      NA,
      "golf",
      NA,
      "baseball",
      NA,
      "basketball",
      "ncaa_football",
      "ncaa_basketball",
      "ncaa_hockey",
      NA,
      "football",
      "hockey",
      NA,
      NA,
      NA,
      NA
    )
  sportradar_rate_limit <-
    c(
      NA, # cricket
      1000, # cycling
      1000, # f1
      1000, # formula e
      1000, # global ice hockey
      200, # golf
      1000, # indycar
      1000, # mlb
      NA, # nascar
      1000, # nba
      1000, # ncaa football
      1000, # ncaa basketball
      1000, # ncaa hockey
      NA, # ncaa womens basketball
      1000, #nfl
      1000, # nhl
      1000, # soccer americas
      1000, # soccer europe
      1000, # soccer internationla
      200 # tennis
    )
  sportradar_con <-
    data.frame(
      sportradar_sports,
      sportradar_keys,
      sportradar_db_match,
      sportradar_rate_limit
    )
  saveRDS(sportradar_con, f.sportradar)
}