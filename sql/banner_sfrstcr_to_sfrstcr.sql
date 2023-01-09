-- Fall to Spring retention
WITH cte_population_base AS (
    SELECT UNIQUE a.sfrstcr_term_code,
                  a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    -- include records that will be listed on an individuals transcript
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
    -- records with a campus code like this have been manually selected to not be included in enrollment
    AND a.sfrstcr_camp_code != 'XXX'
    -- filter for Fall population
    AND a.sfrstcr_term_code LIKE '%40'
    -- records starting from this semester
    AND a.sfrstcr_term_code >= 201740
),
cte_population_compare AS (
    SELECT UNIQUE a.sfrstcr_term_code,
                  a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    -- include records that will be listed on an individuals transcript
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
    -- records with a campus code like this have been manually selected to not be included in enrollment
    AND a.sfrstcr_camp_code != 'XXX'
     -- filter for Spring population
    AND a.sfrstcr_term_code LIKE '%20'
    -- records starting from this semester
    AND a.sfrstcr_term_code >= 201820
),
cte_population_full AS (
    SELECT a.sfrstcr_pidm AS sis_system_id,
           a.sfrstcr_term_code AS term,
           a.sfrstcr_term_code+80 AS return_term,
           CASE
               WHEN (a.sfrstcr_pidm, a.sfrstcr_term_code+80) IN ( SELECT b.sfrstcr_pidm, b.sfrstcr_term_code
                                                                 FROM cte_population_compare b) THEN 'True'
               ELSE 'False'
           END AS retained

    FROM cte_population_base a
)
SELECT *
FROM cte_population_full
;

