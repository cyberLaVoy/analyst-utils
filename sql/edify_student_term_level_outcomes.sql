-- Fall to Spring retention
WITH cte_population_full AS (
    SELECT CAST (a.sis_system_id AS INTEGER),
           a.term_id AS term,
           CAST(a.term_id AS INTEGER)+80 AS return_term,
           a.returned_next_spring AS retained
    FROM public.student_term_level_outcome a
    WHERE CAST ( a.term_id AS INTEGER ) >= 201740
    AND a.term_type = 'Fall'
    AND a.is_enrolled
)
SELECT *
FROM cte_population_full
;
