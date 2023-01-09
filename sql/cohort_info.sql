-- !preview con=con
WITH
population AS (
    SELECT a.*,
           (EXTRACT(YEAR FROM c.stvterm_start_date) - EXTRACT(YEAR FROM b.spbpers_birth_date)) AS age
      FROM saturn.sgrchrt a
 LEFT JOIN saturn.spbpers b
        ON b.spbpers_pidm = a.sgrchrt_pidm
 LEFT JOIN saturn.stvterm c
        ON a.sgrchrt_term_code_eff = c.stvterm_code
     WHERE SUBSTR(a.sgrchrt_chrt_code, 1, 2) = 'FT'
       AND a.sgrchrt_term_code_eff >= 200940
       AND SUBSTR(a.sgrchrt_term_code_eff, 5,6) = 40
)
   SELECT a.sgrchrt_pidm AS pidm,
          j.spriden_first_name AS first_name,
          j.spriden_last_name AS last_name,
          SUBSTR(a.sgrchrt_term_code_eff, 0, 4) AS cohort_year,
          CASE
            WHEN a.age < 18 THEN '0-17'
            WHEN a.age >= 18 AND a.age <= 20 THEN '18-20'
            WHEN a.age >= 21 AND a.age <= 22 THEN '21-22'
            WHEN a.age >= 23 AND a.age <= 34 THEN '23-34'
            WHEN a.age >= 35 AND a.age < 55 THEN '35-54'
            WHEN a.age >= 55 THEN '55+'
          END AS age_group,
          COALESCE(a.sgrchrt_active_ind, 'N') AS exlusion,
          CASE SUBSTR(b.stvchrt_desc, 10, 2)
            WHEN 'FT' THEN 'Full-Time'
            WHEN 'PT' THEN 'Part-Time'
          END AS initial_effort,
          CASE SUBSTR(b.stvchrt_desc, 13, 2)
            WHEN 'BS' THEN 'Bachelor Seeking'
            WHEN 'OT' THEN 'Other'
          END AS initial_goal,
          COALESCE(m.stvresd_desc, 'Unspecified') AS residency_status,
          d.smrprle_program_desc AS initial_program,
          CASE
            WHEN g.spbpers_sex = 'M' THEN 'Male'
            WHEN g.spbpers_sex = 'F' THEN 'Female'
            ELSE 'Unspecified'
          END AS recorded_sex,
          -- ethnicity defined as defined for ipeds
          CASE
            WHEN (k.gorvisa_vtyp_code IS NOT NULL OR k.gorvisa_visa_expire_date > SYSDATE) THEN 'Non-Resident Alien'
            WHEN g.spbpers_ethn_cde = '2' THEN 'Hispanic or Latino'
            WHEN INSTR(l.all_race_desc, 'Hispanic') > 0 THEN 'Hispanic or Latino'
            WHEN INSTR(l.all_race_desc, ',') > 1 THEN 'Multiple'
            ELSE COALESCE(l.all_race_desc, 'Unspecified')
          END AS ethnicity,
          CASE
              WHEN f.stvcoll_desc = 'Coll of Sci, Engr & Tech' THEN 'CSET'
              WHEN f.stvcoll_desc = 'Technologies' THEN 'CSET'
              WHEN f.stvcoll_desc = 'Mathematics' THEN 'CSET'
              WHEN f.stvcoll_desc = 'Computer Information Tech' THEN 'CSET'
              WHEN f.stvcoll_desc = '* Natural Sciences' THEN 'CSET'
              WHEN f.stvcoll_desc = 'College of Business' THEN 'COB'
              WHEN f.stvcoll_desc = 'College of Health Sciences' THEN 'COHS'
              WHEN f.stvcoll_desc = 'Humanities & Social Sciences' THEN 'CHASS'
              WHEN f.stvcoll_desc = 'Coll of Humanities/Soc Sci' THEN 'CHASS'
              WHEN f.stvcoll_desc = 'History/Political Science' THEN 'CHASS'
              WHEN f.stvcoll_desc = 'College of the Arts' THEN 'COA'
              WHEN f.stvcoll_desc = '*Education/Family Studies/PE' THEN 'COE'
              WHEN f.stvcoll_desc = 'College of Education' THEN 'COE'
              WHEN f.stvcoll_desc = 'General Education' THEN 'GE'
              ELSE f.stvcoll_desc
          END AS college_abbreviation,
          -- NOTE: there are many missing department codes from this
          c.sgbstdn_dept_code AS department,
          i.sgradvr_advr_pidm AS advisor_pidm,
          CASE
              WHEN i.sgradvr_advr_pidm IS NOT NULL
              THEN i.spriden_first_name || ', ' || i.spriden_last_name
          END AS advisor_full_name
     FROM population a
LEFT JOIN saturn.stvchrt b
       ON b.stvchrt_code = a.sgrchrt_chrt_code
LEFT JOIN saturn.sgbstdn c
       ON c.sgbstdn_pidm = a.sgrchrt_pidm
      AND c.sgbstdn_term_code_eff = a.sgrchrt_term_code_eff
LEFT JOIN saturn.smrprle d
       ON d.smrprle_program = c.sgbstdn_program_1
LEFT JOIN saturn.stvcoll f
       ON f.stvcoll_code = c.sgbstdn_coll_code_1
LEFT JOIN saturn.spbpers g
       ON a.sgrchrt_pidm = g.spbpers_pidm
LEFT JOIN ( SELECT *
            FROM saturn.sgradvr i1
            LEFT JOIN saturn.spriden i2
                   ON i2.spriden_pidm = i1.sgradvr_advr_pidm
                   AND i2.spriden_change_ind IS NULL
            -- only grab info for the primary advisor
            WHERE i1.sgradvr_prim_ind = 'Y') i
       ON i.sgradvr_pidm = a.sgrchrt_pidm
      AND i.sgradvr_term_code_eff = a.sgrchrt_term_code_eff
LEFT JOIN saturn.spriden j
       ON j.spriden_pidm = a.sgrchrt_pidm
      AND j.spriden_change_ind IS NULL
LEFT JOIN general.gorvisa k
       ON k.gorvisa_pidm = a.sgrchrt_pidm
LEFT JOIN ( SELECT l1.gorprac_pidm,
                   LISTAGG(l2.stvethn_desc, ', ') WITHIN GROUP (ORDER BY l2.stvethn_desc) AS all_race_desc
              FROM general.gorprac l1
         LEFT JOIN saturn.stvethn l2
                ON l2.stvethn_code = l1.gorprac_race_cde
          GROUP BY l1.gorprac_pidm ) l
       ON l.gorprac_pidm = a.sgrchrt_pidm
LEFT JOIN saturn.stvresd m
       ON m.stvresd_code = c.sgbstdn_resd_code;

