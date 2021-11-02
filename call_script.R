source("01_scripts/create_yahoo_token.R")
yahoo_token <- httr::oauth2.0_access_token(yahoo,myapp,code="g55f994")
save(yahoo_token,file="yahoo_token.Rdata")

week_num <- 7

rmarkdown::render("GOAT_Dashboard.Rmd", 
                  params = list(week_num = week_num)
                  )

file.rename(from = "GOAT_Dashboard.html", to = str_c("02_dashboards/GOAT_Dashbaord_",week_num,".html"))


system("git status")
system("git add .")
system('git commit -m "commit dashboard"')
system("git push", input = "notepad")

# shell("shell1.sh")
# shell("C:/Users/turne/Desktop/Dustys_Files/R_Work/fantasyfootball2021yahoo/shell1.sh")
# system("pwd, input", input = "notepad")
library(git2r)
gitpush()


gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}
