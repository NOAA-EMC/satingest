list(APPEND jobs_files
  JISATINGEST
  )

install(PROGRAMS ${jobs_files}
        DESTINATION "jobs")
