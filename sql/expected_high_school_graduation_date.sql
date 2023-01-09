/* PROPOSED QUERY */
WITH cte_population AS (
    -- If we don't have a high school graduation date,
    -- we use best_guess_high_school_graduation_year.
    SELECT a.sis_system_id,
           -- When a birth date is during the fall,
           -- a person will start high school a year later than those with a prior birthdate.
           CASE WHEN to_char(a.birth_date, 'MMDD') BETWEEN '0902' AND '1231' THEN
                EXTRACT( YEAR FROM (a.birth_date + INTERVAL '19 years') )
           ELSE
                EXTRACT( YEAR FROM (a.birth_date + INTERVAL '18 years') )
           -- provides the best guess on a high school graduation year
           END AS best_guess_high_school_graduation_year
    FROM export.student a
),
cte_high_school_graduation_date_versions AS (
    SELECT a.sis_system_id,
           MAX(b.sorhsch_graduation_date) AS latest_high_school_graduation_date_from_banner,
           -- Concatenate a month and a day to best guess high school graduation year to get a proper date,
           -- where 05-30 is an average day for high school graduation.
           MAX(best_guess_high_school_graduation_year) || '-05-30' AS best_guess_high_school_graduation_date
    FROM cte_population a
    LEFT JOIN banner.sorhsch b
        ON a.sis_system_id = b.sorhsch_pidm
        -- filter for only high school records
        AND b.sorhsch_sbgi_code IN (SELECT b1.stvsbgi_code
                                    FROM banner.stvsbgi b1
                                    WHERE b1.stvsbgi_type_ind = 'H')
        -- sanitize issues with text data type
        AND b.sorhsch_graduation_date IS NOT NULL
        AND b.sorhsch_graduation_date != '0'
        AND b.sorhsch_graduation_date != ''
    GROUP BY a.sis_system_id
)
SELECT a.sis_system_id,
        /* RESULTING VARIABLE */
       -- coalesce function will prioritize the Banner hs grad date
       -- that way, if the data is fixed in Banner, that is what will be made available
       TO_DATE( COALESCE(a.latest_high_school_graduation_date_from_banner,
                         a.best_guess_high_school_graduation_date), 'YYYY-MM-DD') AS expected_high_school_graduation_date
FROM cte_high_school_graduation_date_versions a
;

/* Exploratory SQL */

-- population
select * from export.student;

-- data sources
select * from banner.sorhsch;
select * from ushe.students;
select * from dscir.students;


/* how many relevant records does each data source hold? */

-- how many unique records do we have hs grad date information on in Banner?
-- ~269,978 records
SELECT COUNT(DISTINCT sorhsch_pidm)
FROM banner.sorhsch
-- filter for only high school records
WHERE sorhsch_sbgi_code IN (SELECT a.stvsbgi_code
                              FROM banner.stvsbgi a
                              WHERE a.stvsbgi_type_ind = 'H')
-- sanitize issues with text data type
AND sorhsch_graduation_date IS NOT NULL
AND sorhsch_graduation_date != '0'
AND sorhsch_graduation_date != '';

-- how many unique records do we have hs grad date information on in ushe students table?
-- ~53,466 records
select count(DISTINCT s_id)
from ushe.students
where s_hs_grad_date is not null;

-- how many unique records do we have hs grad date information on in dscir students table?
-- ~135,233 records
select count(DISTINCT s_id)
from dscir.students
where dsc_hsgrad_dt is not null;


/* how far back does each data source go? */

-- 1947-05-01
select min(s_hs_grad_date)
from ushe.students
where s_hs_grad_date is not null
and s_hs_grad_date != '0';

-- must run query and sort on date to get min because of bad data
-- 1944-06-01
select dsc_hsgrad_dt
from dscir.students
where dsc_hsgrad_dt is not null
and dsc_hsgrad_dt != ''
and dsc_hsgrad_dt != '0';

-- must run query and sort on date to get min because of bad data
-- 1910-05-27 00:00:00
SELECT sorhsch_graduation_date
FROM banner.sorhsch a
-- filter for only high school records
WHERE sorhsch_sbgi_code IN (SELECT a.stvsbgi_code
                              FROM banner.stvsbgi a
                              WHERE a.stvsbgi_type_ind = 'H')
-- sanitize issues with text data type
AND sorhsch_graduation_date IS NOT NULL
AND sorhsch_graduation_date != '0'
AND sorhsch_graduation_date != '';


/* example of joining with all data sources */

-- This query shows records that are either in ushe.students or dscir.students, but not in banner.sorhsch.
-- 42 records that can be also filled in with our best guess instead.
WITH cte_population AS (
    SELECT *,
           CASE WHEN to_char(a.birth_date, 'MMDD') BETWEEN '0902' AND '1231' THEN
                EXTRACT( YEAR FROM (a.birth_date + INTERVAL '19 years') )
           ELSE
                EXTRACT( YEAR FROM (a.birth_date + INTERVAL '18 years') )
           END AS best_guess_high_school_graduation_year
    FROM export.student a
),
cte_hs_graduation_date_versions AS (
    SELECT a.sis_system_id,
           MAX(b.sorhsch_graduation_date) AS latest_high_school_graduation_date_from_banner,
           MAX(c.dsc_hsgrad_dt) AS dscir_high_school_graduation_date,
           MAX(d.s_hs_grad_date) AS ushe_high_school_graduation_date,
           MAX(best_guess_high_school_graduation_year) || '-05-30' AS best_guess_high_school_graduation_date
    FROM cte_population a
    LEFT JOIN banner.sorhsch b
        ON a.sis_system_id = b.sorhsch_pidm
         -- filter for only high school records
        AND b.sorhsch_sbgi_code IN (SELECT b1.stvsbgi_code
                                    FROM banner.stvsbgi b1
                                    WHERE b1.stvsbgi_type_ind = 'H')
        -- sanitize issues with text data type
        AND b.sorhsch_graduation_date IS NOT NULL
        AND sorhsch_graduation_date != '0'
        AND sorhsch_graduation_date != ''
    LEFT JOIN dscir.students c
        ON a.sis_system_id = c.dsc_pidm
        -- sanitize issues with text data type
        AND c.dsc_hsgrad_dt IS NOT NULL
        AND c.dsc_hsgrad_dt != '0'
        AND c.dsc_hsgrad_dt != ''
    LEFT JOIN ushe.students d
        ON d.s_id = c.s_id
        -- sanitize issues with text data type
        AND d.s_hs_grad_date IS NOT NULL
        AND d.s_hs_grad_date != '0'
        AND d.s_hs_grad_date != ''
    GROUP BY a.sis_system_id
)
SELECT a.sis_system_id,
       COALESCE(a.latest_high_school_graduation_date_from_banner,
                a.dscir_high_school_graduation_date,
                a.ushe_high_school_graduation_date,
                a.best_guess_high_school_graduation_date) AS expected_high_school_graduation_date,
                a.best_guess_high_school_graduation_date
FROM cte_hs_graduation_date_versions a
WHERE a.latest_high_school_graduation_date_from_banner IS NULL
AND ( a.dscir_high_school_graduation_date IS NOT NULL
      OR a.ushe_high_school_graduation_date IS NOT NULL )
;

-- Why filter on sbgi code? Here's why:
SELECT DISTINCT b.stvsbgi_type_ind
FROM banner.sorhsch a
LEFT JOIN banner.stvsbgi b
    ON b.stvsbgi_code = a.sorhsch_sbgi_code ;
-- also note: PRIMARY KEY (sorhsch_pidm, sorhsch_sbgi_code)