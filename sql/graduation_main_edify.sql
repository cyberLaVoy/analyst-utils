-- easy to modify population cte
WITH cte_population AS (
    SELECT DISTINCT sis_system_id,
                    term_id
    FROM student_term_cohort
)
SELECT population.sis_system_id,
       a.student_id,
       population.term_id,
       c.term_start_date,
       a.is_athlete,
       a.term_id AS enrolled_term_id,
       a.is_enrolled,
       a.level_id AS enrolled_level,

       e.cohort_degree_level_code AS ipeds_dlev_code,
       e.exclusions_reason_desc AS exclusion_reason,
       e.cohort_code,
       e.cohort_code_desc,
       ( e.is_exclusion = 'true' ) AS exclusion_ind,
       CASE
          WHEN SUBSTR(e.cohort_code, 3, 1) = 'F' AND e.cohort_code LIKE 'FT%'
             THEN 'FTFT'
          WHEN SUBSTR(e.cohort_code, 3, 1) = 'P' AND e.cohort_code LIKE 'FT%'
             THEN 'FTPT'
          WHEN SUBSTR(e.cohort_code, 3, 1) = 'F' AND e.cohort_code NOT LIKE 'FT%'
             THEN 'NFTFT'
          WHEN SUBSTR(e.cohort_code, 3, 1) = 'P' AND e.cohort_code NOT LIKE 'FT%'
             THEN 'NFTPT'
       END AS ipeds_cohort_desc,

       f.is_veteran,
       f.primary_visa_type_code AS visa_code,
       f.gender_code AS gender,
       f.ipeds_race_ethnicity,

       -- awarded indicators
       g.is_pell_ind,
       CASE
           WHEN g.is_pell_ind THEN FALSE
           ELSE h.sub_loan_ind
       END AS sub_loan_ind,
       ( NOT COALESCE(g.is_pell_ind, FALSE) AND NOT COALESCE(h.sub_loan_ind, FALSE) ) AS no_pell_sub_loan_ind,

       COALESCE(i.bs_grad_date, j.as_grad_date, k.cert_grad_date) - c.term_start_date AS highest_earned_degree_days_to_grad,
       COALESCE(i.bs_grad_date, j.as_grad_date, k.cert_grad_date) as highest_earned_degree_grad_date,
       COALESCE(i.b_degree_program, j.a_degree_program, k.c_degree_program) as highest_earned_degree_program_code,
       COALESCE(i.b_degree_id, j.a_degree_id, k.c_degree_id) as highest_earned_degree_id,
       i.bs_grad_date - c.term_start_date AS bs_grad_days,
       j.as_grad_date - c.term_start_date AS as_grad_days,
       k.cert_grad_date - c.term_start_date AS cert_grad_days

FROM cte_population population
LEFT JOIN student_term_level a
    ON population.sis_system_id = a.sis_system_id
LEFT JOIN term c
    ON c.term_id = population.term_id
/* FA: Pell Grant */
LEFT JOIN (SELECT DISTINCT sis_system_id,
                   financial_aid_year_id,
                   TRUE AS is_pell_ind
            FROM student_financial_aid_year__fund_detail
            WHERE offer_amount > 0
            AND financial_aid_fund_id IN ('FPELL', 'FPELL1') ) g
    ON c.financial_aid_year_id = g.financial_aid_year_id
    AND population.sis_system_id = g.sis_system_id
/* Subsidized Student Loans */
LEFT JOIN (SELECT DISTINCT sis_system_id,
                   financial_aid_year_id,
                   TRUE AS sub_loan_ind
            FROM student_financial_aid_year__fund_detail
            WHERE offer_amount > 0
            AND financial_aid_fund_id = 'DIRECT') h
    ON c.financial_aid_year_id = h.financial_aid_year_id
    AND population.sis_system_id = h.sis_system_id
LEFT JOIN student_term_cohort e
    ON population.sis_system_id = e.sis_system_id
    AND population.term_id = e.term_id
LEFT JOIN student f
    ON population.sis_system_id = f.sis_system_id
/* For degrees we take the highest earned degree based on the date they first earned the degree.
   The order is as follows: Bachelors, Associates, Certificates */
-- Bachelors
LEFT JOIN (SELECT MIN(graduation_date) AS bs_grad_date,
           MIN(graduated_term_id) AS term_code,
           MAX(program_id) AS b_degree_program,
           MAX(degree_id) AS b_degree_id,
           sis_system_id
           FROM degrees_awarded
           WHERE degree_id LIKE 'B%'
           AND degree_status_code  = 'AW'
           GROUP BY sis_system_id) i
   ON i.sis_system_id = population.sis_system_id
  AND i.term_code >= population.term_id
-- Associates
LEFT JOIN (SELECT MIN(graduation_date) AS as_grad_date,
           MIN(graduated_term_id) AS term_code,
           MAX(program_id) AS a_degree_program,
           MAX(degree_id) AS a_degree_id,
           sis_system_id
           FROM degrees_awarded
           WHERE degree_id LIKE 'A%'
           AND degree_status_code  = 'AW'
           GROUP BY sis_system_id) j
   ON j.sis_system_id = population.sis_system_id
  AND j.term_code >= population.term_id
-- Certificates
LEFT JOIN (SELECT MIN(graduation_date) AS cert_grad_date,
           MIN(graduated_term_id) AS term_code,
           MAX(program_id) AS c_degree_program,
           MAX(degree_id) AS c_degree_id,
           sis_system_id
           FROM degrees_awarded
           WHERE degree_id LIKE 'C%'
           AND degree_status_code  = 'AW'
           GROUP BY sis_system_id) k
   ON k.sis_system_id = population.sis_system_id
  AND k.term_code >= population.term_id
;
