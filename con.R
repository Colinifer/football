# brew install mariadb-connector-c
# sudo chown -R $(whoami) /usr/local/share/man/man5 /usr/local/share/man/man7
# chmod u+w /usr/local/share/man/man5 /usr/local/share/man/man7

db_user <- "publiccolin"

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "72.78.233.235",
                 port = "3306",
                 user = db_user,
                 password = rstudioapi::askForPassword(paste("Enter MariaDB password for user:", db_user, sep=" ")),
                 dbname = "football",
                 # database = "football",
                 # Server = "localhost\\SQLEXPRESS", 
                 # Database = "datawarehouse", 
                 Trusted_Connection = "True")

# all_projectsDb <- dbConnect(RMariaDB::MariaDB(), user='user', password=localuserpassword, dbname='projects', host='localhost')