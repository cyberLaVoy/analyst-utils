SELECT sgrsprt_pidm AS pidm,
       listagg(sgrsprt_actc_code,',') WITHIN GROUP (ORDER BY sgrsprt_actc_code)
       AS s_sport,
       dsc.ipeds_ethnicity(sgrsprt_pidm, 'D') as Ethnicity,
       spbpers_sex as Gender
                   FROM   sgrsprt
                    LEFT JOIN spbpers on sgrsprt_pidm = spbpers_pidm
                   WHERE  sgrsprt_term_code = '201930'
                   GROUP  BY sgrsprt_pidm, spbpers_sex;