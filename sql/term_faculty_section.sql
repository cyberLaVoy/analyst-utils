WITH cte_term_faculty_section AS (
    SELECT UNIQUE a.sirasgn_term_code AS term_id,
                  b.spriden_id AS faculty_id,
                  a.sirasgn_crn AS course_reference_number
    -- CONSTRAINT pk_sirasgn
    --      PRIMARY KEY (sirasgn_term_code, sirasgn_crn, sirasgn_pidm, sirasgn_category)
    FROM saturn.sirasgn a
    LEFT JOIN saturn.spriden b ON a.sirasgn_pidm = b.spriden_pidm
                              AND b.spriden_change_ind IS NULL
    -- Only go back as far as Fall 2018.
    WHERE a.sirasgn_term_code >= 201840
    )
SELECT *
FROM cte_term_faculty_section;