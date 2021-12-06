list(APPEND scripts_files
  existday.sh
  existore.sh
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")
