SELECT c.ssbsect_term_code AS term_id,
       c.ssbsect_crn AS course_reference_number,

      /* sirasign required data fields */
      a.sirasgn_fcnt_code AS contract_type,
      CASE WHEN a.sirasgn_posn LIKE 'GNC%' OR a.sirasgn_fcnt_code = 'NC' -- Logic for is_non_compensated
                -- Faculty workload is zero, if the position is non-compensated
           THEN 0
                -- sirasgn_workload_adjust translates to override workload field in Banner input UI
                -- So if this value is NOT null, we use that before the original workload field ( perfasg_workload ).
                -- Also times this workload value by percent of responsibility.
           ELSE COALESCE( a.sirasgn_workload_adjust, k.perfasg_workload, 0 ) * a.sirasgn_percent_response / 100
      END AS ut_assigned_workload,
      a.sirasgn_percent_response AS percentage_of_responsibility,
      a.sirasgn_percent_sess AS percentage_of_session,
      a.sirasgn_primary_ind AS primary_faculty_indicator,
      CASE WHEN a.sirasgn_crn IN (SELECT a1.sirasgn_crn
                                    FROM saturn.sirasgn a1
                                  WHERE a1.sirasgn_term_code = a.sirasgn_term_code
                                 GROUP BY a1.sirasgn_crn
                                  HAVING COUNT(a1.sirasgn_crn) > 1)
           THEN 'Yes'
           ELSE 'No'
      END AS is_team_taught,
      /* sirasign required data fields */

      -- Section level information
      t.ssrxlst_xlst_group AS cross_list_group,
      c.ssbsect_schd_code AS schedule_code,
      COALESCE(c.ssbsect_lec_hr, j.scbcrse_lec_hr_low, 0) AS lecture_hours,
      COALESCE(c.ssbsect_lab_hr, j.scbcrse_lab_hr_low, 0) AS lab_hours,
      COALESCE(c.ssbsect_oth_hr, j.scbcrse_oth_hr_low, 0) AS other_hours,
      COALESCE(c.ssbsect_cont_hr, j.scbcrse_cont_hr_low, 0) AS contact_hours,
      COALESCE(c.ssbsect_credit_hrs, j.scbcrse_credit_hr_low, 0) AS credit_hours,
      COALESCE(c.ssbsect_enrl, 0) AS student_count,

      -- Data fields used for communication with data stewards
      n.stvsubj_desc AS course_subject,
      c.ssbsect_crse_numb AS course_number,
      c.ssbsect_seq_numb AS course_section_number

-- CONSTRAINT pk_sirasgn
--      PRIMARY KEY (sirasgn_term_code, sirasgn_crn, sirasgn_pidm, sirasgn_category)
-- CONSTRAINT pk_ssbsect
--   PRIMARY KEY (ssbsect_term_code, ssbsect_crn)
FROM saturn.ssbsect c
LEFT JOIN saturn.sirasgn a
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
LEFT JOIN saturn.stvsubj n
       ON c.ssbsect_subj_code = n.stvsubj_code
-- CONSTRAINT pk_ssrxlst
--        PRIMARY KEY (ssrxlst_term_code, ssrxlst_xlst_group, ssrxlst_crn)
LEFT JOIN saturn.ssrxlst t
       ON t.ssrxlst_crn = a.sirasgn_crn
      AND t.ssrxlst_term_code = a.sirasgn_term_code
-- Keep all records where the number of students OR the amount of workload is not zero.
WHERE ( c.ssbsect_enrl != 0 OR COALESCE( a.sirasgn_workload_adjust, k.perfasg_workload, 0 ) != 0 )
-- Only go back as far as Fall 2018.
AND a.sirasgn_term_code >= 201840;