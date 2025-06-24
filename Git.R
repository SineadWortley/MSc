install.packages("usethis")
library(usethis)
install.packages("gitcreds")
library(gitcreds)
usethis::use_git_config(
  user.name = "SineadWortley", 
  user.email = "u18122672@tuks.co.za"
)
usethis::create_github_token()
gitcreds::gitcreds_set()

usethis::use_git()
usethis::use_github()
