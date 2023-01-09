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
-- Collect all records of undergraduate or above work that occurred after expected high school graduation,
-- and at an institution that is not Utah Tech.
cte_transfer_credits AS (

    /* banner.shrtgpa */
    SELECT a.shrtgpa_pidm AS sis_system_id,
           a.shrtgpa_levl_code AS student_level
    FROM banner.shrtgpa a
    -- these tables provide additional information on shrtgpa records
    LEFT JOIN banner.shrtrit b
        ON a.shrtgpa_pidm = b.shrtrit_pidm
        AND a.shrtgpa_trit_seq_no = b.shrtrit_seq_no
    LEFT JOIN banner.stvsbgi c
        ON b.shrtrit_sbgi_code = c.stvsbgi_code
    -- this join is to have expected_high_school_graduation_date_as_term available
    LEFT JOIN cte_population d
        ON a.shrtgpa_pidm = d.sis_system_id
    -- 'GPA Type Indicator. Valid values are: I - Institutional Course Term GPA, and T - Transfer Course Term GPA.'
    WHERE a.shrtgpa_gpa_type_ind = 'T'
    /* WHERE? */
    -- 'This field identifies the source/background institution type (e.g. college, high school, source-only)'
    AND c.stvsbgi_type_ind = 'C'
    -- 'This field indicates whether source/background institution is a recruiting source.'
    -- excludes entities that award college credit but aren't colleges (e.g. AP credits, FLATS, etc.)
    AND c.stvsbgi_srce_ind = 'Y'
    /* WHEN? */
    -- If their hs grad date falls on a summer semester, and the transfer record term falls on a summer semester,
    -- then we count them as having transfer "freshman" credits.
    -- This is to deal with cases where expected_high_school_graduation_date is during the summer semester.
    AND a.shrtgpa_term_code >= d.expected_high_school_graduation_date_as_term

    UNION

    /* banner.sordegr */
    SELECT a.sordegr_pidm AS sis_system_id,
           CASE
               WHEN c.stvdegc_dlev_code IN ('MA', 'DR') THEN 'GR'
               -- Any transfer record that is not a masters or doctorates degree is considered undergraduate work.
               ELSE 'UG'
           END AS student_level
    FROM banner.sordegr a
    -- these tables provide additional information on sordegr records
    LEFT JOIN banner.sorpcol b
        -- Constraint on sordegr that shows why we join with sorpcol in this fashion.
        -- CONSTRAINT fk1_sordegr_inv_sorpcol_key
        --      FOREIGN KEY (sordegr_pidm, sordegr_sbgi_code) REFERENCES sorpcol
        ON b.sorpcol_pidm = a.sordegr_pidm
        AND a.sordegr_sbgi_code = b.sorpcol_sbgi_code
    -- Using info from this table to determine the student level.
    LEFT JOIN banner.stvdegc c
        ON c.stvdegc_code = a.sordegr_degc_code
    LEFT JOIN banner.stvsbgi f
        ON a.sordegr_sbgi_code = f.stvsbgi_code
     -- this join is to have expected_high_school_graduation_date_as_term available
    LEFT JOIN cte_population e
        ON a.sordegr_pidm = e.sis_system_id
    -- what term does the last day attended fall under?
    LEFT JOIN banner.stvterm d
        ON TO_DATE(a.sordegr_attend_to, 'YYYY-MM-DD') >= TO_DATE(d.stvterm_start_date, 'YYYY-MM-DD')
        AND TO_DATE(a.sordegr_attend_to, 'YYYY-MM-DD') < ( TO_DATE(d.stvterm_end_date, 'YYYY-MM-DD') + INTERVAL '21 days' )
    /* WHEN? */
    -- If their hs grad date falls on a summer semester, and the last date attended at another institution also falls on a summer semester,
    -- then we count them as having transfer "freshman" credits.
    -- This is to deal with cases where expected_high_school_graduation_date is during the summer semester.
    WHERE d.stvterm_code >= e.expected_high_school_graduation_date_as_term
    /* WHERE? */
    -- 'This field identifies the source/background institution type (e.g. college, high school, source-only)'
    AND f.stvsbgi_type_ind = 'C'
    -- 'This field indicates whether source/background institution is a recruiting source.'
    -- excludes entities that award college credit but aren't colleges (e.g. AP credits, FLATS, etc.)
    AND f.stvsbgi_srce_ind = 'Y'
)
SELECT a.sis_system_id,
       b.student_level,
       /* RESULTING VARIABLE */
       ( student_level IS NOT NULL ) AS has_transfer_credits_after_expected_high_school_graduation_date
FROM cte_population a
LEFT JOIN cte_transfer_credits b
    ON a.sis_system_id = b.sis_system_id
GROUP BY a.sis_system_id, b.student_level
;

/* MAIN relevant tables */

-- Term GPA Table
SELECT *
FROM banner.shrtgpa;

-- Prior College Degree Table
SELECT *
FROM banner.sordegr;

/* AUXILIARY relevant tables */

SELECT *
FROM banner.shrtrit;

SELECT *
FROM banner.stvsbgi;

SELECT *
FROM banner.sorpcol;

SELECT *
FROM banner.stvdegc;

SELECT *
FROM banner.stvdlev;

SELECT *
FROM banner.stvterm;


/* table ANALYSIS queries */

-- institutions in sordegr that are not a college or a "recruiting source"
SELECT DISTINCT a.sordegr_sbgi_code, b.stvsbgi_desc
FROM banner.sordegr a
LEFT JOIN banner.stvsbgi b
    ON a.sordegr_sbgi_code = b.stvsbgi_code
WHERE (
    -- 'This field identifies the source/background institution type (e.g. college, high school, source-only)'
    b.stvsbgi_type_ind != 'C'
    -- 'This field indicates whether source/background institution is a recruiting source.'
    -- excludes entities that award college credit but aren't colleges (e.g. AP credits, FLATS, etc.)
    OR b.stvsbgi_srce_ind != 'Y'
    OR b.stvsbgi_srce_ind IS NULL );


/* BEGIN dlev code question from main query */
SELECT DISTINCT a.stvdegc_dlev_code, a.stvdegc_desc
FROM banner.stvdegc a;

-- these records should be considered undergraduate work
SELECT DISTINCT a.sordegr_degc_code, b.stvdegc_dlev_code, b.stvdegc_desc, c.stvsbgi_desc
FROM banner.sordegr a
LEFT JOIN banner.stvdegc b
    ON a.sordegr_degc_code = b.stvdegc_code
LEFT JOIN banner.stvsbgi c
    ON c.stvsbgi_code = a.sordegr_sbgi_code
WHERE c.stvsbgi_type_ind = 'C'
-- 'This field indicates whether source/background institution is a recruiting source.'
-- excludes entities that award college credit but aren't colleges (e.g. AP credits, FLATS, etc.)
AND c.stvsbgi_srce_ind = 'Y'
AND b.stvdegc_dlev_code NOT IN ('MA', 'DR', 'AS', 'BS', 'BA');
/* END dlev code question from main query */

-- clarification on if the name of these columns actually match the data
SELECT a.sordegr_pidm, MIN(a.sordegr_attend_from) AS from, MIN(a.sordegr_attend_to) AS to
FROM banner.sordegr a
WHERE a.sordegr_attend_from != ''
AND a.sordegr_attend_to != ''
GROUP BY a.sordegr_pidm;


/* queries from existing audit report */

SELECT a.shrtgpa_pidm,
       a.shrtgpa_levl_code,
       MAX(a.shrtgpa_term_code) AS last_transfer_term,
       MAX(d.stvterm_start_date) AS last_transfer_term_start_date
     FROM banner.shrtgpa a
LEFT JOIN banner.shrtrit b
    ON b.shrtrit_pidm = a.shrtgpa_pidm
    AND b.shrtrit_seq_no = a.shrtgpa_trit_seq_no
LEFT JOIN banner.stvsbgi c
    ON c.stvsbgi_code = b.shrtrit_sbgi_code
LEFT JOIN banner.stvterm d
    ON d.stvterm_code = a.shrtgpa_term_code
    WHERE shrtgpa_gpa_type_ind = 'T' -- Transfer GPA
    AND stvsbgi_type_ind = 'C' -- From a College
    AND stvsbgi_srce_ind = 'Y'
    --AND shrtgpa_term_code < (SELECT dsc.f_get_term(SYSDATE,'nterm') FROM dual)
    AND shrtgpa_term_code < :parameter_term
GROUP BY shrtgpa_pidm,
         shrtgpa_levl_code;

SELECT a.sordegr_pidm,
        CASE
            WHEN c.stvdegc_dlev_code = 'MA' THEN 'GR'
            ELSE 'UG'
        END AS student_level,
        d.stvterm_code,
        a.sordegr_attend_to
FROM banner.sordegr a

LEFT JOIN banner.sorpcol b
    -- Constraint on sordegr that shows why we join with sorpcol in this fashion.
    -- CONSTRAINT fk1_sordegr_inv_sorpcol_key
    --      FOREIGN KEY (sordegr_pidm, sordegr_sbgi_code) REFERENCES sorpcol
    ON b.sorpcol_pidm = a.sordegr_pidm
    AND a.sordegr_sbgi_code = b.sorpcol_sbgi_code
LEFT JOIN banner.stvdegc c
    ON c.stvdegc_code = a.sordegr_degc_code
LEFT JOIN banner.stvterm d
    ON a.sordegr_attend_to > d.stvterm_end_date

-- WHERE d.stvterm_code < (SELECT dsc.f_get_term(SYSDATE, 'nterm') FROM dual)
WHERE d.stvterm_code < :parameter_term;

/* From f_calc_entry_action_...
Is this relevant?
AND shrtrit_sbgi_code NOT LIKE 'AP%'
AND shrtrit_sbgi_code NOT LIKE 'CLEP%'
AND shrtrit_sbgi_code NOT LIKE 'CLP%'
AND shrtrit_sbgi_code NOT LIKE 'FL%'
AND shrtrit_sbgi_code NOT LIKE 'VERT%'
AND shrtrit_sbgi_code NOT LIKE 'DSU001%'
AND shrtrit_sbgi_code NOT LIKE 'IBO%'
AND shrtrit_sbgi_code NOT LIKE 'MIL%'
AND shrtrit_sbgi_code NOT LIKE 'MLASP%'
 */




