-- graduates by department
SELECT COUNT(UNIQUE a.dxgrad_pidm) AS total_graduates,
       a.dxgrad_acyr AS academic_year
FROM enroll.dxgrad_all a
LEFT JOIN enroll.dsc_programs_current p
       ON p.prgm_code = a.dxgrad_dgmr_prgm
WHERE p.dept_code = (SELECT p1.dept_code
                     FROM enroll.dsc_programs_current p1
                     WHERE p1.prgm_code = :p_program_code)
GROUP BY a.dxgrad_acyr
ORDER BY a.dxgrad_acyr;