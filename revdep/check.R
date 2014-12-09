library("devtools")

res <- revdep_check(dependencies = c("Depends", "Imports"))
revdep_check_save_summary(res)
revdep_check_save_logs(res)
