-- !preview con=con
   SELECT a.sgrchrt_pidm AS pidm,
          SUBSTR(a.sgrchrt_term_code_eff, 0, 4) AS cohort_year,
          b.stvterm_code AS retained_term,
          CASE
            WHEN SUBSTR(b.stvterm_code, 5, 6) = 40 THEN 'Fall'
            WHEN SUBSTR(b.stvterm_code, 5, 6) = 20 THEN 'Spring'
          END || ' ' || c.relative_term AS relative_term_desc,
          -- note: this calculation only works when summer semesters are not included
          FLOOR( (b.stvterm_code-a.sgrchrt_term_code_eff)/50 ) + 1 AS relative_term_index,
          ROW_NUMBER() OVER (PARTITION BY a.sgrchrt_pidm, a.sgrchrt_term_code_eff ORDER BY b.stvterm_code) AS individual_term_attended,
          CASE
            WHEN SUBSTR(b.stvterm_code, 5, 6) = 20 AND c.relative_term = 1 THEN 1
            ELSE 0
          END AS first_spring_retained
     FROM saturn.sgrchrt a

LEFT JOIN saturn.stvterm b
       ON ( b.stvterm_code IN (SELECT b2.sfrstcr_term_code
                                 FROM saturn.sfrstcr b2
                                 -- join with base query pidm
                                 WHERE a.sgrchrt_pidm = b2.sfrstcr_pidm
                                 -- only include retained terms that are >= to the initial cohort
                                 AND b2.sfrstcr_term_code >= a.sgrchrt_term_code_eff
                                 -- remove summer term from calculations (note: removing this line will break relative indexing)
                                 AND SUBSTR(b2.sfrstcr_term_code, 5, 6) != 30
                                 -- only include records that count towards enrollment
                                 AND b2.sfrstcr_rsts_code IN (SELECT b2.stvrsts_code
                                                                    FROM saturn.stvrsts b2
                                                                    WHERE b2.stvrsts_incl_sect_enrl = 'Y') )
       -- also grab terms where the initial cohort matches
       OR b.stvterm_code = a.sgrchrt_term_code_eff )

LEFT JOIN (SELECT ROW_NUMBER() OVER (PARTITION BY SUBSTR(c1.stvterm_code, 5, 6) ORDER BY c1.stvterm_code) AS relative_term,
                   c1.stvterm_code
              FROM saturn.stvterm c1
              WHERE c1.stvterm_code >= 200940
              ORDER BY stvterm_code) c
           ON c.stvterm_code = b.stvterm_code-a.sgrchrt_term_code_eff+200940

    WHERE a.sgrchrt_term_code_eff >= 200940
      AND SUBSTR(a.sgrchrt_term_code_eff, 5, 6) = 40
      AND SUBSTR(a.sgrchrt_chrt_code, 1, 2) = 'FT'
    ORDER BY a.sgrchrt_pidm, a.sgrchrt_term_code_eff, b.stvterm_code;
