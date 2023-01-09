-- !preview con=con
   SELECT UNIQUE advisor_info.sgradvr_advr_pidm AS advisor_pidm,
          CASE
              WHEN advisor_info.sgradvr_advr_pidm IS NOT NULL
              THEN
                advisor_info.spriden_first_name || ', ' || advisor_info.spriden_last_name
              ELSE
                  'None Assigned'
          END AS advisor_full_name,
          advisor_info.spriden_id AS advisor_banner_id,
          a.sfrstcr_pidm AS student_pidm,
          student_info.spriden_first_name || ', ' || student_info.spriden_last_name AS student_full_name,
          student_info.spriden_id AS student_banner_id,
          'd' || student_info.spriden_id || '@domain' AS student_email,
          CASE
              WHEN h.sprtele_phone_number IS NOT NULL
              THEN '(' || h.sprtele_phone_area || ') ' || SUBSTR(h.sprtele_phone_number, 0, 3) || '-' || SUBSTR(h.sprtele_phone_number, 4, 4)
          END AS student_phone,
          cohort_info.stvterm_acyr_code - 1 AS cohort_year,
          cohort_info.stvterm_desc AS cohort_semester,
          CASE SUBSTR(cohort_info.sgrchrt_term_code_eff, 5, 6)
              WHEN '40' THEN 'Fall'
              WHEN '20' THEN 'Spring'
          END AS cohort_semester_category,
          CASE
              WHEN e.stvcoll_desc = 'Coll of Sci, Engr & Tech' THEN 'CSET'
              WHEN e.stvcoll_desc = 'Technologies' THEN 'CSET'
              WHEN e.stvcoll_desc = 'Mathematics' THEN 'CSET'
              WHEN e.stvcoll_desc = 'Computer Information Tech' THEN 'CSET'
              WHEN e.stvcoll_desc = '* Natural Sciences' THEN 'CSET'
              WHEN e.stvcoll_desc = 'College of Business' THEN 'COB'
              WHEN e.stvcoll_desc = 'College of Health Sciences' THEN 'COHS'
              WHEN e.stvcoll_desc = 'Humanities & Social Sciences' THEN 'CHASS'
              WHEN e.stvcoll_desc = 'Coll of Humanities/Soc Sci' THEN 'CHASS'
              WHEN e.stvcoll_desc = 'History/Political Science' THEN 'CHASS'
              WHEN e.stvcoll_desc = 'College of the Arts' THEN 'COA'
              WHEN e.stvcoll_desc = '*Education/Family Studies/PE' THEN 'COE'
              WHEN e.stvcoll_desc = 'College of Education' THEN 'COE'
              WHEN e.stvcoll_desc = 'General Education' THEN 'GE'
              ELSE e.stvcoll_desc
          END AS college_abbreviation,
          f.stvmajr_desc AS major,
          -- undergraduate cumulative gpa
          ROUND(g.shrlgpa_gpa, 2) AS cumulative_gpa,
          -- undergraduate term gpa
          ROUND(j.shrtgpa_gpa, 2) AS term_gpa,
          -- undergraduate credit hours earned
          ROUND(g.shrlgpa_hours_earned) AS credit_hours_earned,
          CASE
              -- when pidm not found in enrollment table
              WHEN b.sfrstcr_pidm IS NULL THEN 'False'
              ELSE 'True'
          END AS is_enrolled
     FROM (SELECT UNIQUE a1.sfrstcr_pidm
           FROM saturn.sfrstcr a1
           -- was enrolled, at some point, in current semester
           WHERE a1.sfrstcr_term_code = 202120
           -- not reported in enrollment campus
           AND a1.sfrstcr_camp_code != 'XXX'
           AND a1.sfrstcr_levl_code IN ('UG','GR')
           AND a1.sfrstcr_rsts_code IN ( SELECT a2.stvrsts_code
                                         FROM saturn.stvrsts a2
                                         WHERE a2.stvrsts_incl_sect_enrl = 'Y' ) ) a
-- collect enrollment info for the next semester
LEFT JOIN (SELECT UNIQUE b1.sfrstcr_pidm
           FROM saturn.sfrstcr b1
           WHERE b1.sfrstcr_term_code = 202140
           -- not reported in enrollment campus
           AND b1.sfrstcr_camp_code != 'XXX'
           AND b1.sfrstcr_levl_code IN ('UG','GR')
           AND b1.sfrstcr_rsts_code IN (SELECT b1.stvrsts_code
                                        FROM saturn.stvrsts b1
                                        WHERE b1.stvrsts_incl_sect_enrl = 'Y') ) b
   ON b.sfrstcr_pidm = a.sfrstcr_pidm
-- collect student info
LEFT JOIN saturn.spriden student_info
       ON student_info.spriden_pidm = a.sfrstcr_pidm
      AND student_info.spriden_change_ind IS NULL
-- collect cohort info
LEFT JOIN (SELECT UNIQUE c1.sgrchrt_pidm,
                         c2.stvterm_acyr_code,
                         c2.stvterm_desc,
                         c1.sgrchrt_term_code_eff
           FROM saturn.sgrchrt c1
           LEFT JOIN saturn.stvterm c2
                  ON c1.sgrchrt_term_code_eff = c2.stvterm_code
           -- only grab most recent term code record from the cohort table
           WHERE c1.sgrchrt_term_code_eff = (SELECT MAX(c3.sgrchrt_term_code_eff)
                                             FROM saturn.sgrchrt c3
                                             WHERE c3.sgrchrt_pidm = c1.sgrchrt_pidm)) cohort_info
       ON cohort_info.sgrchrt_pidm = a.sfrstcr_pidm
-- collect base student academic info
INNER JOIN saturn.sgbstdn d
       ON d.sgbstdn_pidm = a.sfrstcr_pidm
      AND d.sgbstdn_stst_code = 'AS'
      -- only grab most recent term code record from the student base table
      AND d.sgbstdn_term_code_eff =  (SELECT MAX(d1.sgbstdn_term_code_eff)
                                      FROM saturn.sgbstdn d1
                                      WHERE d1.sgbstdn_pidm = d.sgbstdn_pidm
                                      AND d1.sgbstdn_term_code_eff <= 202120)
LEFT JOIN saturn.stvcoll e
       ON d.sgbstdn_coll_code_1 = e.stvcoll_code
LEFT JOIN saturn.stvmajr f
       ON d.sgbstdn_majr_code_1 = f.stvmajr_code
-- collect cumulative gpa info
LEFT JOIN saturn.shrlgpa g
       ON g.shrlgpa_pidm = a.sfrstcr_pidm
      AND g.shrlgpa_gpa_type_ind = 'O'
      AND g.shrlgpa_levl_code = 'UG'
-- collect term gpa info
LEFT JOIN saturn.shrtgpa j
       ON j.shrtgpa_pidm = a.sfrstcr_pidm
      AND j.shrtgpa_term_code = 202120
      AND j.shrtgpa_gpa_type_ind = 'I'
      AND j.shrtgpa_levl_code = 'UG'
-- collect telephone information
LEFT JOIN saturn.sprtele h
       ON h.sprtele_pidm = a.sfrstcr_pidm
      AND h.sprtele_tele_code = 'CELL'
      AND h.sprtele_seqno = (select max(h1.sprtele_addr_seqno)
                        from saturn.sprtele h1
                        WHERE h1.sprtele_pidm = h.sprtele_pidm)
-- collect advisor info
LEFT JOIN ( SELECT *
            FROM saturn.sgradvr i1
            LEFT JOIN saturn.spriden i2
                   ON i2.spriden_pidm = i1.sgradvr_advr_pidm
                   AND i2.spriden_change_ind IS NULL
            -- only grab info for the primary advisor
            WHERE i1.sgradvr_prim_ind = 'Y'
              -- make sure advisor assignment is the most recent
              AND i1.sgradvr_term_code_eff = (SELECT MAX( i3.sgradvr_term_code_eff )
                                              FROM saturn.sgradvr i3
                                              WHERE i3.sgradvr_pidm = i1.sgradvr_pidm) ) advisor_info
    ON advisor_info.sgradvr_pidm = a.sfrstcr_pidm
    -- college filter
    WHERE e.stvcoll_desc != 'Global & Community Outreach'
    -- graduation filter
    AND a.sfrstcr_pidm NOT IN ( SELECT a2.shrdgmr_pidm
                                FROM saturn.shrdgmr a2
                                -- record from current or summer semester
                                WHERE a2.shrdgmr_term_code_grad IN ( 202120, 202130 )
                                -- degree of bachelors or masters or AAS
                                AND ( a2.shrdgmr_degc_code LIKE 'B%' OR a2.shrdgmr_degc_code LIKE 'M%' OR a2.shrdgmr_degc_code = 'AAS' )
                                -- has a pending or awarded degree
                                AND a2.shrdgmr_degs_code IN ('AW', 'PN') )



