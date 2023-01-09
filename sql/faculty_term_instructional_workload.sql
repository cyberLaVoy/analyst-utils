SELECT b.spriden_id AS sis_id,
       a.sirasgn_term_code AS term_code,
      -- 'CONTRACT TYPE: Indicates the type of job.  Valid values are P = Primary, S = Secondary, or O = Overload. Only one primary job per person is allowed.'
      CASE
        WHEN w.nbrbjob_contract_type = 'P'
            THEN 'Primary'
        WHEN w.nbrbjob_contract_type = 'S'
            THEN 'Secondary'
        WHEN w.nbrbjob_contract_type = 'O'
            THEN 'Overload'
      END AS payroll_contract_type,
      x.stvfcnt_desc AS course_contract_type,
      CASE WHEN a.sirasgn_posn LIKE 'GNC%' OR a.sirasgn_fcnt_code = 'NC' -- Logic for is_non_compensated
                -- Faculty workload is zero, if the position is non-compensated
           THEN 0
                -- sirasgn_workload_adjust translates to override workload field in Banner input UI
                -- So if this value is NOT null, we use that before the original workload field ( perfasg_workload ).
                -- Also times this workload value by percent of responsibility.
           ELSE COALESCE( a.sirasgn_workload_adjust, k.perfasg_workload, 0 ) * a.sirasgn_percent_response / 100
      END AS course_workload,
      COALESCE(l.stvcoll_statscan_cde3, l.stvcoll_desc, 'Unavailable') AS course_college,
      m.stvdept_desc AS course_department,
      c.ssbsect_subj_code AS course_subject_code,
      n.stvsubj_desc AS course_subject,
      c.ssbsect_crse_numb AS course_number,
      c.ssbsect_seq_numb AS course_section_number,
      a.sirasgn_crn AS course_crn,
      j.scbcrse_title AS course_title,
      CASE SUBSTR(c.ssbsect_crse_numb, 1, 1)
           WHEN '0' THEN 'Remedial'
           WHEN '1' THEN 'Lower'
           WHEN '2' THEN 'Lower'
           WHEN '3' THEN 'Upper'
           WHEN '4' THEN 'Upper'
           WHEN '5' THEN 'Advanced Upper'
           WHEN '6' THEN 'Graduate'
           WHEN '7' THEN 'Graduate'
      END AS course_division,
      t.ssrxlst_xlst_group AS course_cross_list_group,
      a.sirasgn_percent_response AS percentage_of_responsibility,
      CASE WHEN a.sirasgn_crn IN (SELECT a1.sirasgn_crn
                                    FROM saturn.sirasgn a1
                                  WHERE a1.sirasgn_term_code = a.sirasgn_term_code
                                 GROUP BY a1.sirasgn_crn
                                  HAVING COUNT(a1.sirasgn_crn) > 1)
           THEN 'Yes'
           ELSE 'No'
      END AS is_team_taught,
      CASE WHEN a.sirasgn_posn LIKE 'GNC%' OR a.sirasgn_fcnt_code = 'NC'
           THEN 'Yes'
           ELSE 'No'
      END AS is_non_compensated,
      v.stvschd_desc AS course_schedule_desc,
      c.ssbsect_max_enrl AS course_maximum_enrollment,

      COALESCE(c.ssbsect_credit_hrs, j.scbcrse_credit_hr_low, 0) AS course_credit_hours,
      COALESCE(c.ssbsect_enrl, 0) AS course_student_count,
      COALESCE(c.ssbsect_tot_credit_hrs, 0) AS course_student_credit_hours,

      /* These are used to get Faculty Contact Hours */
      COALESCE(c.ssbsect_lec_hr, j.scbcrse_lec_hr_low, 0) AS course_lecture_hours,
      COALESCE(c.ssbsect_lab_hr, j.scbcrse_lab_hr_low, 0) AS course_lab_hours,
      COALESCE(c.ssbsect_oth_hr, j.scbcrse_oth_hr_low, 0) AS course_other_hours,
      COALESCE(c.ssbsect_cont_hr, j.scbcrse_cont_hr_low, 0) AS course_contact_hours,
      /* These are used to get Faculty Contact Hours */

      CASE WHEN u.scrlevl_levl_code = 'UG' THEN
            ROUND(c.ssbsect_tot_credit_hrs/15, 2)
           WHEN u.scrlevl_levl_code = 'GR' THEN
            ROUND(c.ssbsect_tot_credit_hrs/10, 2)
      END AS course_fte

-- CONSTRAINT pk_sirasgn
--      PRIMARY KEY (sirasgn_term_code, sirasgn_crn, sirasgn_pidm, sirasgn_category)
FROM saturn.sirasgn a

-- faculty demographic info
LEFT JOIN saturn.spriden b
       ON b.spriden_pidm = a.sirasgn_pidm
      AND b.spriden_change_ind IS NULL
-- course section info
-- CONSTRAINT pk_ssbsect
--   PRIMARY KEY (ssbsect_term_code, ssbsect_crn)
LEFT JOIN saturn.ssbsect c
       ON a.sirasgn_term_code = c.ssbsect_term_code
      AND a.sirasgn_crn = c.ssbsect_crn
-- term info on course assignment
LEFT JOIN saturn.stvterm d
       ON d.stvterm_code = a.sirasgn_term_code
/* payroll.perfasg */
-- faculty workload info
--    CONSTRAINT pk_perfasg
--        PRIMARY KEY (perfasg_term_code, perfasg_pidm, perfasg_crn, perfasg_category)
LEFT JOIN payroll.perfasg k
       ON k.perfasg_pidm = a.sirasgn_pidm
      AND k.perfasg_crn = a.sirasgn_crn
      AND k.perfasg_term_code = a.sirasgn_term_code
      AND k.perfasg_category = a.sirasgn_category
-- course related info
-- No primary key on table.
-- CREATE UNIQUE INDEX scbcrse_key_index
--     ON scbcrse (scbcrse_subj_code, scbcrse_crse_numb, scbcrse_eff_term)
LEFT JOIN saturn.scbcrse j
       ON j.scbcrse_crse_numb = c.ssbsect_crse_numb
      AND j.scbcrse_subj_code = c.ssbsect_subj_code
      AND j.scbcrse_eff_term = (SELECT MAX(j1.scbcrse_eff_term)
                                  FROM saturn.scbcrse j1
                                 WHERE j.scbcrse_crse_numb = j1.scbcrse_crse_numb
                                   AND j.scbcrse_subj_code = j1.scbcrse_subj_code
                                   -- limit on the max for historical records
                                   AND j1.scbcrse_eff_term <= d.stvterm_code)
-- CONSTRAINT pk_scrlevl
--        PRIMARY KEY (scrlevl_subj_code, scrlevl_crse_numb, scrlevl_eff_term, scrlevl_levl_code)
LEFT JOIN saturn.scrlevl u
       ON u.scrlevl_crse_numb = c.ssbsect_crse_numb
      AND u.scrlevl_subj_code = c.ssbsect_subj_code
      AND u.scrlevl_eff_term = (SELECT MAX(u1.scrlevl_eff_term)
                                  FROM saturn.scrlevl u1
                                 WHERE u.scrlevl_crse_numb = u1.scrlevl_crse_numb
                                   AND u.scrlevl_subj_code = u1.scrlevl_subj_code
                                   -- limit on the max for historical records
                                   AND u1.scrlevl_eff_term <= d.stvterm_code)
LEFT JOIN saturn.stvcoll l
       ON l.stvcoll_code = j.scbcrse_coll_code
LEFT JOIN saturn.stvdept m
       ON m.stvdept_code = j.scbcrse_dept_code
LEFT JOIN saturn.stvsubj n
       ON c.ssbsect_subj_code = n.stvsubj_code
-- CONSTRAINT pk_ssrxlst
--        PRIMARY KEY (ssrxlst_term_code, ssrxlst_xlst_group, ssrxlst_crn)
LEFT JOIN saturn.ssrxlst t
       ON t.ssrxlst_crn = a.sirasgn_crn
      AND t.ssrxlst_term_code = a.sirasgn_term_code
LEFT JOIN saturn.stvschd v
       ON v.stvschd_code = c.ssbsect_schd_code
-- pull job info on faculty assignment (could probably be removed on next iteration)
-- CONSTRAINT pk_nbrbjob
--        PRIMARY KEY (nbrbjob_pidm, nbrbjob_posn, nbrbjob_suff)
LEFT JOIN posnctl.nbrbjob w
       ON w.nbrbjob_pidm = a.sirasgn_pidm
      AND w.nbrbjob_posn = a.sirasgn_posn
      AND w.nbrbjob_suff = a.sirasgn_suff
      AND w.nbrbjob_begin_date = (SELECT MAX(w1.nbrbjob_begin_date)
                                    FROM posnctl.nbrbjob w1
                                   WHERE w.nbrbjob_pidm = w1.nbrbjob_pidm
                                     AND w.nbrbjob_posn = w1.nbrbjob_posn
                                     AND w.nbrbjob_suff = w1.nbrbjob_suff
                                     -- limit on the max for historical records
                                     AND w1.nbrbjob_begin_date < d.stvterm_end_date)
-- Faculty Contract Code Validation Table
LEFT JOIN saturn.stvfcnt x
    ON x.stvfcnt_code = a.sirasgn_fcnt_code

-- Keep all records where the number of students OR the amount of workload is not zero.
WHERE ( c.ssbsect_enrl != 0 OR COALESCE( a.sirasgn_workload_adjust, k.perfasg_workload, 0 ) != 0 )
-- Only go back as far as Fall 2018.
AND a.sirasgn_term_code >= 201840;