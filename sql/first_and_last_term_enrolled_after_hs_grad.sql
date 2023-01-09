/* PROPOSED QUERY */
WITH cte_population AS (
    SELECT a.sis_system_id,
           b.stvterm_code AS expected_high_school_graduation_date_as_term
    FROM export.student a
    LEFT JOIN banner.stvterm b
        -- Convert date field into the term that it falls within
        -- for use in later calculations.
        ON a.expected_high_school_graduation_date >= TO_DATE(b.stvterm_start_date, 'YYYY-MM-DD')
        -- add an arbitrary number of days,
        -- to deal with edge cases where date field lands on a date between terms
        AND a.expected_high_school_graduation_date < ( TO_DATE(b.stvterm_end_date, 'YYYY-MM-DD') + INTERVAL '21 days' )
),
cte_utah_tech_enrollment AS (

    /* sfrstcr */
    -- data for current date ranges
    SELECT a.sfrstcr_pidm AS sis_system_id,
           a.sfrstcr_term_code AS term_code,
           c.sgbstdn_levl_code AS student_level
    /* WHERE? answered by nature of table */
    FROM banner.sfrstcr a
    LEFT JOIN cte_population b
        ON a.sfrstcr_pidm = b.sis_system_id
    LEFT JOIN banner.sgbstdn c
        ON a.sfrstcr_pidm  = c.sgbstdn_pidm
        AND c.sgbstdn_term_code_eff = (SELECT MAX(c1.sgbstdn_term_code_eff)
                                       FROM banner.sgbstdn c1
                                       WHERE c1.sgbstdn_pidm = c.sgbstdn_pidm
                                       AND c.sgbstdn_term_code_eff >= a.sfrstcr_term_code)
    -- include records that will be listed on an individuals transcript
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                FROM banner.stvrsts a1
                                WHERE a1.stvrsts_incl_sect_enrl = 'Y')
    -- records with a campus code like this have been manually selected to not be included in enrollment
    AND a.sfrstcr_camp_code != 'XXX'
    /* WHEN? */
    -- This is to deal with cases where expected_high_school_graduation_date is during the summer semester.
    AND a.sfrstcr_term_code >= b.expected_high_school_graduation_date_as_term

    UNION

    /* shrtgpa */
    -- data for mid date ranges
    SELECT a.shrtgpa_pidm AS sis_system_id,
           a.shrtgpa_term_code AS term_code,
           a.shrtgpa_levl_code AS student_level
    FROM banner.shrtgpa a
    LEFT JOIN cte_population b
        ON a.shrtgpa_pidm = b.sis_system_id
    -- sanitize for bad records
    WHERE a.shrtgpa_term_code != '000000'
    /* WHERE? */
    -- records only from local institution
    AND a.shrtgpa_gpa_type_ind = 'I'
    -- This is to deal with cases where expected_high_school_graduation_date is during the summer semester.
    /* WHEN? */
    AND a.shrtgpa_term_code >= b.expected_high_school_graduation_date_as_term

    UNION

    /* historic_transcript_records */
    -- data for mid to historic date ranges
    SELECT a.pidm AS sis_system_id,
           -- convert quarter based schedule to equivalent term based schedule
           CASE
              WHEN a.quarter IN ('1', '2') THEN
                '19' || a.year || '20'
              WHEN a.quarter IN ('3', '4') THEN
                '19' || a.year || '30'
              WHEN a.quarter IN ('5', '6') THEN
                '19' || a.year || '40'
           END AS term_code,
           'UG' AS student_level
    /* WHERE? answered by nature of table */
    FROM flat_files.historic_transcript_records a
    LEFT JOIN cte_population b
        ON a.pidm = b.sis_system_id
    -- sanitize for bad records
    WHERE a.year != 'DE'
    AND a.year != ''
    /* WHEN? */
    AND ( CASE
             WHEN a.quarter IN ('1', '2') THEN
               '19' || a.year || '20'
             WHEN a.quarter IN ('3', '4') THEN
               '19' || a.year || '30'
             WHEN a.quarter IN ('5', '6') THEN
               '19' || a.year || '40'
          END ) >= b.expected_high_school_graduation_date_as_term

    UNION

    /* ushe.students */
    -- accounts for historical 3rd week snapshot data
    SELECT a.s_ssid AS sis_system_id,
           a.s_term AS term_code,
           a.s_level AS student_level
    /* WHERE? answered by nature of table */
    FROM ushe.students a
    LEFT JOIN cte_population b
         ON a.s_ssid = b.sis_system_id
    -- sanitize issues with text data type
    WHERE a.s_hs_grad_date IS NOT NULL
    AND a.s_hs_grad_date != '0'
    AND a.s_hs_grad_date != ''
    /* WHEN? */
    -- only 3rd week snapshot data
    AND a.s_extract = '3'
    AND term_code >= b.expected_high_school_graduation_date_as_term

    UNION

    /* export.student_term_level_version */
    -- accounts for 3rd week snap shot data, moving forward
    SELECT a.sis_system_id,
           a.term_id AS term_code,
           a.level_id AS student_level
    /* WHERE? answered by nature of table */
    FROM export.student_term_level_version a
    LEFT JOIN cte_population b
        ON a.sis_system_id = b.sis_system_id
    /* WHEN? */
    -- only 3rd week snapshot data
    WHERE a.is_census_version
    AND a.term_id >= b.expected_high_school_graduation_date_as_term

)
SELECT a.sis_system_id,
       a.student_level,
       /* RESULTING VARIABLE */
       MIN( a.term_code ) AS first_term_enrolled_after_expected_high_school_graduation_date,
       /* RESULTING VARIABLE */
       MAX( a.term_code ) AS last_term_enrolled_after_expected_high_school_graduation_date
FROM cte_utah_tech_enrollment a
GROUP BY a.sis_system_id, a.student_level
;


/* relevant tables */

SELECT *
FROM banner.sfrstcr;

SELECT *
FROM banner.shrtgpa;

SELECT *
FROM flat_files.historic_transcript_records;

SELECT *
FROM ushe.students;

SELECT *
FROM dscir.students;


/* time frame of each relevant table */

-- 200540, 202240
SELECT MIN(a.sfrstcr_term_code), MAX(a.sfrstcr_term_code)
FROM banner.sfrstcr a;

-- 196240, 202220
SELECT MIN(a.shrtgpa_term_code), MAX(a.shrtgpa_term_code)
FROM banner.shrtgpa a
-- sanitize bad data
WHERE a.shrtgpa_term_code != '000000'
-- records only from local institution
AND a.shrtgpa_gpa_type_ind = 'I';

-- 38, 88
SELECT MIN(a.year), MAX(a.year)
FROM flat_files.historic_transcript_records a
-- sanitize bad data
WHERE a.year != 'DE'
AND a.year != '';

SELECT DISTINCT CASE
        WHEN a.s_term = '1' THEN
            a.s_year || '20'
        WHEN a.s_term = '2' THEN
            a.s_year || '30'
        WHEN a.s_term = '3' THEN
            a.s_year || '40'
        END AS term_code
FROM ushe.students a;

select DISTINCT a.s_level
from ushe.students a;

-- 200930, 202430
SELECT MIN(a.term_id), MAX(a.term_id)
FROM export.student_term_level_version a;

select DISTINCT a.level_id
from export.student_term_level_version a;


-- 2001, 2022
SELECT MIN(a.s_year), MAX(a.s_year)
FROM dscir.students a;



-- question: can we use sfrstcr just for current term records?
-- The following query returns 500+ records. So the answer to this is no, we can't exclude past records in sfrstcr.
WITH cte_population AS (
    SELECT *,
           b.stvterm_code AS expected_high_school_graduation_date_as_term
    FROM export.student a
    LEFT JOIN banner.stvterm b
        -- Convert expected_high_school_graduation_date into the term that it falls within
        -- for use in later calculations.
        ON a.expected_high_school_graduation_date >= TO_DATE(b.stvterm_start_date, 'YYYY-MM-DD')
        AND a.expected_high_school_graduation_date <= TO_DATE(b.stvterm_end_date, 'YYYY-MM-DD')
)
-- data for current date ranges
SELECT a.sfrstcr_pidm AS sis_system_id,
       a.sfrstcr_term_code AS term_code,
       c.sgbstdn_levl_code AS student_level,
       'sfrstcr' AS source_table
FROM banner.sfrstcr a
LEFT JOIN cte_population b
    ON a.sfrstcr_pidm = b.sis_system_id
LEFT JOIN banner.sgbstdn c
    ON a.sfrstcr_pidm  = c.sgbstdn_pidm
    AND c.sgbstdn_term_code_eff = (SELECT MAX(c1.sgbstdn_term_code_eff)
                                   FROM banner.sgbstdn c1
                                   WHERE c1.sgbstdn_pidm = c.sgbstdn_pidm
                                   AND c.sgbstdn_term_code_eff >= a.sfrstcr_term_code)
-- include records that will be listed on an individuals transcript
WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                            FROM banner.stvrsts a1
                            WHERE a1.stvrsts_incl_sect_enrl = 'Y')
-- records with a campus code like this have been manually selected to not be included in enrollment
AND a.sfrstcr_camp_code != 'XXX'
-- This is to deal with cases where expected_high_school_graduation_date is during the summer semester.
AND a.sfrstcr_term_code >= b.expected_high_school_graduation_date_as_term

/* analysis filters */
-- remove records for the current term
AND a.sfrstcr_term_code != '202240'
-- remove records that are already covered by shrtgpa and historic_transcript_records
AND (a.sfrstcr_pidm, a.sfrstcr_term_code, c.sgbstdn_levl_code)
    NOT IN (
        -- data for mid date ranges
        SELECT a.shrtgpa_pidm AS sis_system_id,
               a.shrtgpa_term_code AS term_code,
               a.shrtgpa_levl_code AS student_level
        FROM banner.shrtgpa a
        LEFT JOIN cte_population b
            ON a.shrtgpa_pidm = b.sis_system_id
        -- records only from local institution
        WHERE a.shrtgpa_gpa_type_ind = 'I'
        -- sanitize for bad records
        AND a.shrtgpa_term_code != '000000'
        -- This is to deal with cases where expected_high_school_graduation_date is during the summer semester.
        AND a.shrtgpa_term_code >= b.expected_high_school_graduation_date_as_term

        UNION

        -- data for mid to historic date ranges
        SELECT a.pidm AS sis_system_id,
               -- convert quarter based schedule to equivalent term based schedule
               CASE
                  WHEN a.quarter IN ('1', '2') THEN
                    '19' || a.year || '20'
                  WHEN a.quarter IN ('3', '4') THEN
                    '19' || a.year || '30'
                  WHEN a.quarter IN ('5', '6') THEN
                    '19' || a.year || '40'
               END AS term_code,
               'UG' AS student_level
        FROM flat_files.historic_transcript_records a
        LEFT JOIN cte_population b
            ON a.pidm = b.sis_system_id
        -- sanitize for bad records
        WHERE a.year != 'DE'
        AND a.year != ''
        AND ( CASE
                 WHEN a.quarter IN ('1', '2') THEN
                   '19' || a.year || '20'
                 WHEN a.quarter IN ('3', '4') THEN
                   '19' || a.year || '30'
                 WHEN a.quarter IN ('5', '6') THEN
                   '19' || a.year || '40'
              END ) >= b.expected_high_school_graduation_date_as_term
    )
;


-- earliest term
SELECT TO_DATE(b.stvterm_start_date, 'YYYY-MM-DD')
FROM banner.stvterm b
ORDER BY b.stvterm_start_date ASC
LIMIT 1;


LEFT JOIN banner.stvterm b
    -- Convert date field into the term that it falls within
    -- for use in later calculations.
    ON a.base_table_date >= TO_DATE(b.stvterm_start_date, 'YYYY-MM-DD')
    -- add an arbitrary number of days,
    -- to deal with edge cases where date field lands on a date between terms
    AND a.base_table_date < ( TO_DATE(b.stvterm_end_date, 'YYYY-MM-DD') + INTERVAL '21 days' )