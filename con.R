# brew install mariadb-connector-c
# sudo chown -R $(whoami) /usr/local/share/man/man5 /usr/local/share/man/man7
# chmod u+w /usr/local/share/man/man5 /usr/local/share/man/man7

db_user <- "publiccolin"

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = rstudioapi::askForSecret("Enter Iceman IP address",
                                  title = paste("Iceman IP Address")),
  # appears above the prompt
  port = "3306",
  user = db_user,
  password = rstudioapi::askForSecret("Enter password",
                                      title = paste("Iceman Password")),
  # appears above the prompt
  # password = rstudioapi::askForPassword(paste("Enter MariaDB password for user:", db_user, sep=" ")),
  dbname = proj_name,
  Trusted_Connection = "True"
)

# all_projectsDb <- dbConnect(RMariaDB::MariaDB(), user='user', password=localuserpassword, dbname='projects', host='localhost')