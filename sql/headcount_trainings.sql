
/* Relevant Tables */
-- base tables
SELECT * FROM saturn.sfrstcr;

SELECT * FROM saturn.sfrstca;

SELECT * FROM saturn.sgbstdn;

-- validation tables
SELECT * FROM saturn.stvrsts;

SELECT * FROM saturn.stvstst;

SELECT * FROM saturn.stvstyp;

select * from saturn.sobcurr;


/* Selecting certain columns with filter */
-- pulling from actual registration table
-- "after the dust settles"
SELECT sfrstcr_term_code,
       sfrstcr_pidm,
       sfrstcr_crn,
       sfrstcr_rsts_code
FROM saturn.sfrstcr
WHERE sfrstcr_term_code = 202120;

-- pulling from archive table
-- Are we able to do this?
-- There has been mention of problems with doing this.
SELECT sfrstca_term_code,
       sfrstca_pidm,
       sfrstca_crn,
       sfrstca_seq_number,
       sfrstca_rsts_code,
       sfrstca_activity_date
FROM saturn.sfrstca
WHERE sfrstca_term_code = 202120;


/* Ways to pull from current registration table */
-- a list of unique person term code combination
SELECT UNIQUE a.sfrstcr_term_code, a.sfrstcr_pidm
FROM saturn.sfrstcr a
WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                              FROM saturn.stvrsts a1
                              WHERE a1.stvrsts_incl_sect_enrl = 'Y');

-- an actual count, with subquery and aggregation
SELECT a.sfrstcr_term_code, COUNT(UNIQUE a.sfrstcr_pidm)
FROM saturn.sfrstcr a
WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                              FROM saturn.stvrsts a1
                              WHERE a1.stvrsts_incl_sect_enrl = 'Y')
GROUP BY a.sfrstcr_term_code;


/* Joining base headcount query with other tables */

-- A Common Table Expression, also called as CTE in short form,
-- is a temporary named result set that you can reference within a
-- SELECT, INSERT, UPDATE, or DELETE statement.
WITH cte_population AS (
    SELECT UNIQUE a.sfrstcr_term_code, a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
)
SELECT *
FROM cte_population;

-- joining with sgbstdn
-- student type for term available
WITH cte_population AS (
    SELECT UNIQUE a.sfrstcr_term_code, a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
)
SELECT a.sfrstcr_term_code,
       a.sfrstcr_pidm,
       c.stvstyp_desc
FROM cte_population a
INNER JOIN saturn.sgbstdn b
ON ( b.sgbstdn_pidm = a.sfrstcr_pidm
     AND b.sgbstdn_term_code_eff = (SELECT MAX(b1.sgbstdn_term_code_eff)
                                    FROM saturn.sgbstdn b1
                                    WHERE b1.sgbstdn_pidm = b.sgbstdn_pidm
                                    AND b1.sgbstdn_term_code_eff <= a.sfrstcr_term_code) )
LEFT JOIN saturn.stvstyp c
ON c.stvstyp_code = b.sgbstdn_styp_code;


-- Joining with curriculum info
WITH cte_population AS (
    SELECT UNIQUE a.sfrstcr_term_code, a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
)
SELECT a.sfrstcr_term_code,
       a.sfrstcr_pidm,
       -- why are these different?
       /**/
       b.sgbstdn_levl_code,
       c.sobcurr_levl_code,
       /**/
       /**/
       b.sgbstdn_camp_code,
       c.sobcurr_camp_code,
       /**/
       /**/
       b.sgbstdn_coll_code_1,
       c.sobcurr_coll_code,
       /**/
       /**/
       b.sgbstdn_degc_code_1,
       c.sobcurr_degc_code,
       /**/
       /**/
       b.sgbstdn_program_1,
       c.sobcurr_program
       /**/
FROM cte_population a
INNER JOIN saturn.sgbstdn b
ON ( b.sgbstdn_pidm = a.sfrstcr_pidm
     AND b.sgbstdn_term_code_eff = (SELECT MAX(b1.sgbstdn_term_code_eff)
                                    FROM saturn.sgbstdn b1
                                    WHERE b1.sgbstdn_pidm = b.sgbstdn_pidm
                                    AND b1.sgbstdn_term_code_eff <= a.sfrstcr_term_code) )
LEFT JOIN saturn.sobcurr c
ON c.sobcurr_curr_rule = b.sgbstdn_curr_rule_1;



/* Filters to discuss */
-- strange campus code; using it to:
-- 1. award credit
-- 2. recognize non credit courses
-- 3. not on DSU campus (University of Utah students)
WHERE sfrstcr_camp_code != 'XXX'
-- options with inclusion of courses
WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                              FROM saturn.stvrsts a1
                              WHERE a1.stvrsts_incl_sect_enrl = 'Y')
-- graduate vs undergraduate courses (should come from sgbstdn)
WHERE sfrstcr_levl_code = 'UG'
-- making sure they are an active student
-- What does this actually mean?
WHERE a.sgbstdn_stst_code = 'AS'


/* More notes:
Metrics to consider with headcount.
    1. The person
    2. Time of enrollment
        a. Term
        b. Datetime
    3. Type of course
    4. Money (did they pay for classes?)
    5. Is there time here going to show up on their transcript?
 */

/* DEFINITION
University Headcount -
Given an observation of a student's action with a course in a given term,
the number of individuals where a record will be found on their permanent transcript,
indicating said observation,
is the base headcount for the University for said term.
 */



-- TODO: add pell award
-- TODO: get list of AP courses (separate tab in rmarkdown)
WITH cte_population AS (
    SELECT SUM(a.sfrstcr_credit_hr) AS credits_earned,
           a.sfrstcr_term_code,
           a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
    AND a.sfrstcr_camp_code != 'XXX'
    GROUP BY a.sfrstcr_term_code, a.sfrstcr_pidm
),
best_test_scores AS (
    SELECT a.sortest_pidm,
           a.sortest_tesc_code,
           a.sortest_test_score
      FROM (SELECT a1.sortest_pidm,
                   a1.sortest_tesc_code,
                   a1.sortest_test_score,
                   -- used final where clause to get the best test score (per person, test code combination)
                   ROW_NUMBER() OVER (PARTITION BY a1.sortest_pidm, a1.sortest_tesc_code ORDER BY a1.sortest_test_score DESC) AS rowrank
              FROM (SELECT a2.sortest_pidm,
                           a2.sortest_test_score,
                           CASE
                               -- consider A02 and A02N as the same test
                               WHEN a2.sortest_tesc_code IN ('A02','A02N') THEN 'A02'
                               -- consider ALEKS and ALEKSN as the same test
                               WHEN a2.sortest_tesc_code IN ('ALEKS','ALEKSN') THEN 'ALEKS'
                               ELSE a2.sortest_tesc_code
                               END AS sortest_tesc_code
                      FROM saturn.sortest a2
                     -- only grab certain test scores
                     WHERE a2.sortest_tesc_code IN ('A02','A02N','A01','A05','CPTW','ALEKS','ALEKSN') ) a1
            ) a
      -- filter for top test score
      WHERE a.rowrank = 1
)
SELECT a.sfrstcr_pidm,
       a.sfrstcr_term_code,
       v1.stvstyp_desc AS student_type,
       dsc.ipeds_ethnicity(a.sfrstcr_pidm, 'D') AS ipeds_race_ethnicity,
       baninst1.gp_goksdif.f_get_sd_text('SPBPERS', 'FIRST_GEN_STUDENT', a.sfrstcr_pidm, 1) AS is_first_gen,
       v2.stvresd_desc AS utah_residency_status,
       a.credits_earned,
       -- Initial registration date is relative to the term.
       c.sfbetrm_initial_reg_date AS init_term_reg_date,
       test1.sortest_test_score,
       test2.sortest_test_score,
       test3.sortest_test_score,
       test4.sortest_test_score,
       test5.sortest_test_score
FROM cte_population a
INNER JOIN saturn.sgbstdn b
ON ( b.sgbstdn_pidm = a.sfrstcr_pidm
     AND b.sgbstdn_term_code_eff = (SELECT MAX(b1.sgbstdn_term_code_eff)
                                    FROM saturn.sgbstdn b1
                                    WHERE b1.sgbstdn_pidm = b.sgbstdn_pidm
                                    AND b1.sgbstdn_term_code_eff <= a.sfrstcr_term_code) )
LEFT JOIN saturn.sfbetrm c
ON ( c.sfbetrm_pidm = a.sfrstcr_pidm
     AND c.sfbetrm_term_code = a.sfrstcr_term_code)
/* BEGIN validation joins */
LEFT JOIN saturn.stvstyp v1
ON v1.stvstyp_code = b.sgbstdn_styp_code
LEFT JOIN saturn.stvresd v2
ON v2.stvresd_code = b.sgbstdn_resd_code
/* END validation joins */
/* BEGIN best_test_scores CTE joins by specific tests */
 LEFT JOIN best_test_scores test1
        ON a.sfrstcr_pidm = test1.sortest_pidm
       AND test1.sortest_tesc_code = 'A02'
 LEFT JOIN best_test_scores test2
        ON a.sfrstcr_pidm = test2.sortest_pidm
       AND test2.sortest_tesc_code = 'A01'
 LEFT JOIN best_test_scores test3
        ON a.sfrstcr_pidm = test3.sortest_pidm
       AND test3.sortest_tesc_code = 'A05'
 LEFT JOIN best_test_scores test4
        ON a.sfrstcr_pidm = test4.sortest_pidm
       AND test4.sortest_tesc_code = 'CPTW'
 LEFT JOIN best_test_scores test5
        ON a.sfrstcr_pidm = test5.sortest_pidm
       AND test5.sortest_tesc_code = 'ALEKS'
/* END best_test_scores CTE joins by specific tests */
WHERE a.sfrstcr_term_code = 202140;