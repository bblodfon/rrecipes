# Session info to file
writeLines(capture.output(xfun::session_info()), 'session_info.txt')

