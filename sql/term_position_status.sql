SELECT UNIQUE c.stvterm_code AS term,
       -- A unique identifier for a position is the combination of the posn code and suff code, for said position.
       f.nbrjobs_posn || f.nbrjobs_suff AS position,
       CASE
            WHEN ( g.ptrecls_long_desc LIKE 'FT%' ) THEN 'On Professional-Track'
            ELSE g.ptrecls_long_desc
       END AS status
FROM saturn.stvterm c
LEFT JOIN posnctl.nbrjobs f
       ON f.nbrjobs_effective_date = (SELECT MAX(f1.nbrjobs_effective_date)
                                        FROM posnctl.nbrjobs f1
                                         -- limit on the max for historical records
                                         WHERE f1.nbrjobs_effective_date < c.stvterm_end_date)
LEFT JOIN payroll.ptrecls g
       ON f.nbrjobs_ecls_code = g.ptrecls_code
ORDER BY term, position, status;

