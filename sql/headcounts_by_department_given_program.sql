-- headcount by department
SELECT COUNT(UNIQUE a.s_id) AS third_week_headcount,
       a.banner_term AS term
FROM enroll.students03 a
LEFT JOIN enroll.dsc_programs_current p1
       ON a.cur_prgm1 = p1.prgm_code
LEFT JOIN enroll.dsc_programs_current p2
       ON a.cur_prgm2 = p2.prgm_code
-- third week
WHERE a.s_extract = '3'
-- Fall terms only
AND a.banner_term LIKE '%40'
-- either collected dept equal to the dept in question
AND ( p1.dept_code = (SELECT p3.dept_code
                      FROM enroll.dsc_programs_current p3
                      WHERE p3.prgm_code = :p_program_code)
   OR p2.dept_code = (SELECT p4.dept_code
                      FROM enroll.dsc_programs_current p4
                      WHERE p4.prgm_code = :p_program_code) )
GROUP BY a.banner_term
ORDER BY a.banner_term;