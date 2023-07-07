.
install.packages("usethis")
usethis::use_git()
usethis::use_git_config(
  user.name = "Lucas Coraça Germano",
  user.email = "lucascgermano@gmail.com"
)

credentials::ssh_setup_github()
