-- !preview con=con
   SELECT a.sgrchrt_pidm,
          a.sgrchrt_term_code_eff AS cohort_year,
          b.sfrstcr_term_code AS enrolled_term,
          b.term_math_score,
          b.enrolled_math
     FROM sgrchrt a
LEFT JOIN (
             SELECT b1.sfrstcr_pidm,
                    b1.sfrstcr_term_code,
                    MAX(b3.shrgrde_quality_points) AS term_math_score,
                    1 AS enrolled_math
               FROM sfrstcr b1
          LEFT JOIN ssbsect b2
                 ON b2.ssbsect_crn = b1.sfrstcr_crn
                AND b2.ssbsect_term_code = b1.sfrstcr_term_code
          LEFT JOIN shrgrde b3
                 ON b3.shrgrde_code = b1.sfrstcr_grde_code
              WHERE b1.sfrstcr_rsts_code IN (SELECT bb1.stvrsts_code
                                               FROM stvrsts bb1
                                              WHERE bb1.stvrsts_incl_sect_enrl ='Y')
                AND b2.ssbsect_subj_code = 'MATH'
                AND b3.shrgrde_levl_code = 'UG'
           GROUP BY b1.sfrstcr_pidm,
                    b1.sfrstcr_term_code
         ) b
       ON b.sfrstcr_pidm = a.sgrchrt_pidm
    WHERE SUBSTR(a.sgrchrt_chrt_code, 1, 2) = 'FT'
      AND a.sgrchrt_term_code_eff >= 200940
      AND SUBSTR(a.sgrchrt_term_code_eff, 5,6) = 40
