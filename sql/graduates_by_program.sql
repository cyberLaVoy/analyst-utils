-- graduates by program
SELECT COUNT(UNIQUE a.dxgrad_pidm) AS total_graduates,
       a.dxgrad_acyr AS academic_year
FROM enroll.dxgrad_all a
WHERE a.dxgrad_dgmr_prgm = :p_program_code
GROUP BY a.dxgrad_acyr
ORDER BY a.dxgrad_acyr;