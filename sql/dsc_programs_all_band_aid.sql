SELECT ipeds_award_lvl AS ipeds_award_level,
       prgm_code AS highest_earned_degree_program_code
FROM dscir.dsc_programs_all
WHERE acyr_code = '1920';