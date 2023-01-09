-- Fall to Spring retention
WITH cte_population_fall AS (
    SELECT a.term_id,
           a.sis_system_id
    FROM student_term_level a
    WHERE is_enrolled
    AND a.term_desc LIKE 'Fall%'
    AND CAST ( a.term_id AS INTEGER ) >= 201740
),
cte_population_spring AS (
    SELECT a.term_id,
           a.sis_system_id
    FROM student_term_level a
    WHERE is_enrolled
    AND a.term_desc LIKE 'Spring%'
    AND CAST ( a.term_id AS INTEGER ) >= 201820
),
cte_population_full AS (
    SELECT a.sis_system_id,
           a.term_id AS term,
           CAST(a.term_id AS integer)+80 AS return_term,
           CASE
               WHEN (a.sis_system_id, CAST(a.term_id AS integer)+80) IN (SELECT b.sis_system_id, CAST(b.term_id AS integer)
                                                                           FROM cte_population_spring b)
               THEN 'True'
               ELSE 'False'
           END AS retained
FROM cte_population_fall a )
SELECT a.term,
       a.return_term,
       a.retained,
       a.sis_system_id,
       c.student_id,
       b.primary_major_college_desc,
       b.primary_program_desc,
       b.freshman_cohort_desc,
       b.overall_gpa,
       b.overall_cumulative_gpa,
       b.registered_credits,
       b.institutional_cumulative_credits_earned + b.transfer_cumulative_credits_earned,
       b.residency_code_desc,
       b.is_graduated_from_primary_degree,
       c.gender_code,
       c.ipeds_race_ethnicity,
       c.latest_high_school_gpa
FROM cte_population_full a
LEFT JOIN student_term_level b ON ( a.sis_system_id = b.sis_system_id
                                    AND a.term = b.term_id )
LEFT JOIN student c ON ( a.sis_system_id = c.sis_system_id )
;