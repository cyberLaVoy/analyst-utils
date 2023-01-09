-- headcount by program
SELECT COUNT(UNIQUE a.s_id) AS third_week_headcount,
       a.banner_term AS term
FROM enroll.students03 a
-- third week
WHERE a.s_extract = '3'
-- Fall terms only
AND a.banner_term LIKE '%40'
-- either collected program equal to the program in question
AND (a.cur_prgm1 = :p_program_code
    OR a.cur_prgm2 = :p_program_code)
GROUP BY a.banner_term
ORDER BY a.banner_term;