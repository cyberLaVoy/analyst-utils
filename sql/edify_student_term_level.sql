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
SELECT *
FROM cte_population_full
;