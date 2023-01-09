WITH cte_population AS (
    SELECT a.sis_system_id,
           -- If we don't have a high school graduation date,
           -- we use best_guess_high_school_graduation_year.
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

/* BEGIN quantum realm high school graduation calculations */
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
),
cte_high_school_graduation_date_calculated_lookup AS (
    SELECT a.sis_system_id,
        /* RESULTING VARIABLE */
        -- coalesce function will prioritize the Banner hs grad date
        -- that way, if the data is fixed in Banner, that is what will be made available
        TO_DATE( COALESCE(a.latest_high_school_graduation_date_from_banner,
                          a.best_guess_high_school_graduation_date), 'YYYY-MM-DD') AS calculated_high_school_graduation_date
    FROM cte_high_school_graduation_date_versions a
),
cte_high_school_graduation_date_calculated_lookup_as_term AS (
    SELECT a.sis_system_id,
           b.stvterm_code AS calculated_high_school_graduation_term
    FROM cte_high_school_graduation_date_calculated_lookup a
    LEFT JOIN banner.stvterm b
        -- Convert date field into the term that it falls within
        -- for use in later calculations.
        ON a.calculated_high_school_graduation_date >= TO_DATE(b.stvterm_start_date, 'YYYY-MM-DD')
        -- add an arbitrary number of days,
        -- to deal with edge cases where date field lands on a date between terms
        AND a.calculated_high_school_graduation_date < ( TO_DATE(b.stvterm_end_date, 'YYYY-MM-DD') + INTERVAL '21 days' )
),
/* END quantum realm high school graduation calculations */

/* BEGIN Utah Tech enrollment lookup */
cte_utah_tech_enrollment AS (

    /* sfrstcr */
    -- data for current date ranges
    SELECT a.sfrstcr_pidm AS sis_system_id,
           a.sfrstcr_term_code AS term_code,
           c.sgbstdn_levl_code AS student_level
    /* WHERE? answered by nature of table */
    FROM banner.sfrstcr a
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term b
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
    -- This is to deal with cases where calculated_high_school_graduation_date is during the summer semester.
    AND a.sfrstcr_term_code >= b.calculated_high_school_graduation_term

    UNION

    /* shrtgpa */
    -- data for mid date ranges
    SELECT a.shrtgpa_pidm AS sis_system_id,
           a.shrtgpa_term_code AS term_code,
           a.shrtgpa_levl_code AS student_level
    FROM banner.shrtgpa a
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term b
        ON a.shrtgpa_pidm = b.sis_system_id
    -- sanitize for bad records
    WHERE a.shrtgpa_term_code != '000000'
    /* WHERE? */
    -- records only from local institution
    AND a.shrtgpa_gpa_type_ind = 'I'
    -- This is to deal with cases where calculated_high_school_graduation_date is during the summer semester.
    /* WHEN? */
    AND a.shrtgpa_term_code >= b.calculated_high_school_graduation_term

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
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term b
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
          END ) >= b.calculated_high_school_graduation_term

    UNION

    /* ushe.students */
    -- accounts for historical 3rd week snapshot data
    SELECT a.s_ssid AS sis_system_id,
           CASE
               WHEN a.s_term = '1' THEN
                   a.s_year || '20'
               WHEN a.s_term = '2' THEN
                   a.s_year || '30'
               WHEN a.s_term = '3' THEN
                   a.s_year || '40'
           END AS term_code,
           a.s_level AS student_level
    /* WHERE? answered by nature of table */
    FROM ushe.students a
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term b
         ON a.s_ssid = b.sis_system_id
    /* WHEN? */
    -- only 3rd week snapshot data
    WHERE a.s_extract = '3'
    AND (CASE
           WHEN a.s_term = '1' THEN
               a.s_year || '20'
           WHEN a.s_term = '2' THEN
               a.s_year || '30'
           WHEN a.s_term = '3' THEN
               a.s_year || '40'
         END ) >= b.calculated_high_school_graduation_term

    UNION

    /* export.student_term_level_version */
    -- accounts for 3rd week snap shot data, moving forward
    SELECT a.sis_system_id,
           a.term_id AS term_code,
           a.level_id AS student_level
    /* WHERE? answered by nature of table */
    FROM export.student_term_level_version a
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term b
        ON a.sis_system_id = b.sis_system_id
    /* WHEN? */
    -- only 3rd week snapshot data
    WHERE a.is_census_version
    AND a.term_id >= b.calculated_high_school_graduation_term

),
cte_utah_tech_enrollment_edge_dates AS (
    SELECT a.sis_system_id,
        a.student_level,
        /* RESULTING VARIABLE */
        MIN ( a.term_code ) AS first_term_enrolled_on_or_after_calculated_hs_graduation_term,
        /* RESULTING VARIABLE */
        MAX ( a.term_code ) AS last_term_enrolled_on_or_after_calculated_hs_graduation_term
    FROM cte_utah_tech_enrollment a
    GROUP BY a.sis_system_id, a.student_level
),
/* END Utah Tech enrollment lookup */

/* BEGIN transfer credits lookup */
-- Collect all records of undergraduate or above work that occurred after calculated high school graduation,
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
    -- this join is to have calculated_high_school_graduation_term available
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term d
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
    -- This is to deal with cases where calculated_high_school_graduation_date is during the summer semester.
    AND a.shrtgpa_term_code >= d.calculated_high_school_graduation_term

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
     -- this join is to have calculated_high_school_graduation_term available
    LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term e
        ON a.sordegr_pidm = e.sis_system_id
    -- what term does the last day attended fall under?
    LEFT JOIN banner.stvterm d
        ON TO_DATE(a.sordegr_attend_to, 'YYYY-MM-DD') >= TO_DATE(d.stvterm_start_date, 'YYYY-MM-DD')
        AND TO_DATE(a.sordegr_attend_to, 'YYYY-MM-DD') < ( TO_DATE(d.stvterm_end_date, 'YYYY-MM-DD') + INTERVAL '21 days' )
    /* WHEN? */
    -- If their hs grad date falls on a summer semester, and the last date attended at another institution also falls on a summer semester,
    -- then we count them as having transfer "freshman" credits.
    -- This is to deal with cases where calculated_high_school_graduation_date is during the summer semester.
    WHERE d.stvterm_code >= e.calculated_high_school_graduation_term
    /* WHERE? */
    -- 'This field identifies the source/background institution type (e.g. college, high school, source-only)'
    AND f.stvsbgi_type_ind = 'C'
    -- 'This field indicates whether source/background institution is a recruiting source.'
    -- excludes entities that award college credit but aren't colleges (e.g. AP credits, FLATS, etc.)
    AND f.stvsbgi_srce_ind = 'Y'
),
cte_has_transfer_credits_lookup AS (
    SELECT  a.sis_system_id,
            a.student_level AS transfer_credits_level,
            TRUE AS has_transfer_credits_on_or_after_calculated_hs_graduation_term
    FROM cte_transfer_credits a
    GROUP BY a.sis_system_id, a.student_level
)
/* END transfer credits lookup */

/* MAIN QUERY */
SELECT a.sis_system_id,

       /* RESULTING TRANSFER CREDIT VARIABLES */
       b.has_transfer_credits_on_or_after_calculated_hs_graduation_term,
       b.transfer_credits_level,

       /* RESULTING UTAH TECH ENROLLMENT VARIABLES */
       c.first_term_enrolled_on_or_after_calculated_hs_graduation_term,
       c.last_term_enrolled_on_or_after_calculated_hs_graduation_term,
       c.student_level,

       /* RESULTING HIGH SCHOOL GRADUATION VARIABLE */
       d.calculated_high_school_graduation_term

FROM cte_population a
LEFT JOIN cte_has_transfer_credits_lookup b
    ON a.sis_system_id = b.sis_system_id
LEFT JOIN cte_utah_tech_enrollment_edge_dates c
    ON a.sis_system_id = c.sis_system_id
LEFT JOIN cte_high_school_graduation_date_calculated_lookup_as_term d
    ON a.sis_system_id = d.sis_system_id
;