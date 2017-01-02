library("devtools")

revdep_check(dependencies = c("Depends", "Imports"))
revdep_check_save_summary()
revdep_check_print_problems()
