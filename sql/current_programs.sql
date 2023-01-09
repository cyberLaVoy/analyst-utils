-- all relevant information on a given program
SELECT prgm_code AS program_code,
       degc_code AS degree_code,
       majr_code AS major_code,
       majr_desc AS major_description,
       school_code AS college_code,
       dept_code AS department_code
FROM enroll.dsc_programs_current
WHERE active_program = 'Y'
ORDER BY program_code;