SELECT Distinct sgrsprt_pidm AS pidm,
       listagg(sgrsprt_actc_code,',') WITHIN GROUP (ORDER BY sgrsprt_actc_code)
       AS s_sport,
       dsc.ipeds_ethnicity(sgrsprt_pidm, 'D') as Ethnicity,
       spbpers_sex as Gender
                   FROM   sgrsprt
                    LEFT JOIN spbpers on sgrsprt_pidm = spbpers_pidm
                   WHERE  sgrsprt_term_code in ('202140', '202220', '202230')
                   GROUP  BY sgrsprt_pidm, spbpers_sex;

SELECT Distinct sgrsprt_pidm AS pidm
                    FROM   sgrsprt
                   WHERE  sgrsprt_term_code in ('202140', '202220', '202230');

SELECT *
                    FROM   sgrsprt
                   WHERE  sgrsprt_term_code in ('202140', '202220', '202230');

select * from stvelig;

select * from stvspst;