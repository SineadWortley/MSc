install.packages("usethis")
library(usethis)
install.packages("gitcreds")
library(gitcreds)
install.packages("curl", type = "binary")
library(curl)
usethis::use_git_config(
  user.name = "SineadWortley", 
  user.email = "u18122672@tuks.co.za"
)
usethis::create_github_token()
gitcreds::gitcreds_set()

usethis::use_git()
usethis::use_github()


lock_path <- file.path(Sys.getenv("R_LIBS_USER"), "00LOCK-curl")
unlink(lock_path, recursive = TRUE, force = TRUE)
