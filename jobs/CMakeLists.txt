list(APPEND jobs_files
  JISATINGEST
  AWS_SYNC 
  )

install(PROGRAMS ${jobs_files}
        DESTINATION "jobs")
