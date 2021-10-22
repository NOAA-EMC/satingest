list(APPEND scripts_files
  existday.sh.ecf
  existore.sh.ecf
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")
