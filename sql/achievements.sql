   SELECT a.sgrchrt_pidm AS pidm,
          SUBSTR(a.sgrchrt_term_code_eff, 0, 4) AS cohort_year,
          b.shrdgmr_term_code_grad AS achievement_term,
          -- note: this calculation only works when summer semesters are not included
          FLOOR( (b.shrdgmr_term_code_grad-a.sgrchrt_term_code_eff)/50 ) + 1 AS relative_term_index,
          CASE
            WHEN SUBSTR(d.stvterm_code, 5, 6) = 40 THEN 'Fall'
            WHEN SUBSTR(d.stvterm_code, 5, 6) = 20 THEN 'Spring'
          END || ' ' || d.relative_term AS relative_term_desc,
          b.shrdgmr_program AS achievement_program,
          d.stvdegc_desc AS achievement,
          CASE SUBSTR(b.shrdgmr_degc_code, 0, 1)
             WHEN 'A' THEN 'Associate'
             WHEN 'B' THEN 'Bachelor'
             WHEN 'M' THEN 'Masters'
             WHEN 'C' THEN 'Certificate'
          END AS achievement_type
     FROM saturn.sgrchrt a
INNER JOIN saturn.shrdgmr b
       ON a.sgrchrt_pidm = b.shrdgmr_pidm
        -- only include records where something was awarded(AW)
       AND b.shrdgmr_degs_code = 'AW'
       -- remove summer term from calculations (note: removing this line will break relative indexing)
       AND SUBSTR(b.shrdgmr_term_code_grad, 5, 6) != 30
       AND b.shrdgmr_term_code_grad >= 200940
       -- only include achievement terms that are >= to the initial cohort
       AND b.shrdgmr_term_code_grad >= a.sgrchrt_term_code_eff

LEFT JOIN (SELECT ROW_NUMBER() OVER (PARTITION BY SUBSTR(d1.stvterm_code, 5, 6) ORDER BY d1.stvterm_code) AS relative_term,
                   d1.stvterm_code
              FROM saturn.stvterm d1
              WHERE d1.stvterm_code >= 200940
              ORDER BY stvterm_code) d
           ON d.stvterm_code = b.shrdgmr_term_code_grad-a.sgrchrt_term_code_eff+200940

LEFT JOIN saturn.stvdegc d
       ON d.stvdegc_code = b.shrdgmr_degc_code

   WHERE a.sgrchrt_term_code_eff >= 200940
      AND SUBSTR(a.sgrchrt_term_code_eff, 5, 6) = 40
      AND SUBSTR(a.sgrchrt_chrt_code, 1, 2) = 'FT'
    ORDER BY a.sgrchrt_pidm, a.sgrchrt_term_code_eff, d.stvterm_code;

