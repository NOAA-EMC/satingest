list(APPEND ush_files
  ingest_check_lapsed_data.sh
  ingest_copy.sh
  ingest_copytotarget.sh
  ingest_get.sh
  ingest_process_days.sh
  ingest_process_onetype_newdays.sh
  ingest_process_onetype_neworbits.sh
  ingest_process_orbits.sh
  ingest_process_orbits_subscript.sh
  ingest_qmgr.sh
  ingest_query.sh
  ingest_script_atovs1b.sh
  ingest_script_mls.sh
  ingest_script_omi.sh
  ingest_sncvgrib.sh
  ingest_sncvgrib96.sh
  ingest_snodepgr.sh
  ingest_translate_orbits.sh
  ingest_transst_poes.sh
  ingest_transst_poes_navo.sh
  navo_poes_sst_bundle.sh
  )

install(PROGRAMS ${ush_files}
        DESTINATION "ush")
