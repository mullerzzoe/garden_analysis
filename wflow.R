wflow_git_config(user.name= "mullerzzoe", user.email = "mullerz@dickinson.edu", overwrite = TRUE)

wflow_start("garden_analysis")

wflow_build()
wflow_view()
wflow_status()
wflow_publish()
wflow_use_github("mullerzzoe")
wflow_git_pull()
wflow_git_push()