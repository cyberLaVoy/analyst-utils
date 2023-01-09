WITH cte_faculty_term_position AS (
    SELECT UNIQUE a.sirasgn_pidm AS pidm,
                  a.sirasgn_term_code AS term_code,
                  -- A unique identifier for a position is the combination of the posn code and suff code, for said position.
                  a.sirasgn_posn AS position,
                  a.sirasgn_suff AS position_suffix
    -- Instructional workload table.
    FROM saturn.sirasgn a
    UNION
    SELECT UNIQUE b.sirnist_pidm AS pidm,
                  b.sirnist_term_code AS term_code,
                  -- A unique identifier for a position is the combination of the posn code and suff code, for said position.
                  b.sirnist_posn AS position,
                  b.sirnist_suff AS position_suffix
    -- Non-Instructional workload table.
    FROM saturn.sirnist b
),
cte_faculty_term_position_detail AS (
    SELECT a.pidm,
           a.term_code AS term_code,
           p.stvdept_desc AS faculty_department,
           COALESCE(q.stvcoll_statscan_cde3, q.stvcoll_desc) AS faculty_college,
           CASE WHEN i.ptrtenr_code IN ('T', 'O') THEN i.ptrtenr_desc
                WHEN g.ptrecls_long_desc LIKE 'FT%' THEN 'On Professional-Track'
                ELSE g.ptrecls_long_desc
           END AS faculty_status,
           s.stvfctg_desc AS faculty_rank
    FROM cte_faculty_term_position a
    -- Employment term info
    LEFT JOIN saturn.stvterm c
           ON c.stvterm_code = a.term_code
    -- Faculty college and department info
    -- This table is not well documented,
    -- so joining in this way was determined by looking at the actual data in the table.
    LEFT JOIN saturn.sirdpcl o
           ON a.pidm = o.sirdpcl_pidm
          AND (o.sirdpcl_term_code_eff, o.sirdpcl_home_ind) = (SELECT MAX(o1.sirdpcl_term_code_eff),
                                                                      MAX(o1.sirdpcl_home_ind)
                                                                 FROM saturn.sirdpcl o1
                                                                WHERE o1.sirdpcl_pidm = o.sirdpcl_pidm
                                                                  -- limit on the max for historical records
                                                                  AND o1.sirdpcl_term_code_eff <= c.stvterm_code)
    LEFT JOIN saturn.stvdept p
           ON p.stvdept_code = o.sirdpcl_dept_code
    LEFT JOIN saturn.stvcoll q
           ON q.stvcoll_code = o.sirdpcl_coll_code
    -- job info on faculty assignment
    -- ALTER TABLE nbrjobs
    --    ADD CONSTRAINT pk_nbrjobs
    --        PRIMARY KEY (nbrjobs_pidm, nbrjobs_posn, nbrjobs_suff, nbrjobs_effective_date)
    LEFT JOIN posnctl.nbrjobs  f
           ON f.nbrjobs_pidm = a.pidm
          AND f.nbrjobs_posn = a.position
          AND f.nbrjobs_suff = a.position_suffix
          AND f.nbrjobs_effective_date = (SELECT MAX(f1.nbrjobs_effective_date)
                                            FROM posnctl.nbrjobs f1
                                           WHERE f.nbrjobs_pidm = f1.nbrjobs_pidm
                                             AND f.nbrjobs_posn = f1.nbrjobs_posn
                                             AND f.nbrjobs_suff = f1.nbrjobs_suff
                                             -- limit on the max for historical records
                                             AND f1.nbrjobs_effective_date < c.stvterm_end_date)
    -- ptrecls_code varchar2(2 char)  NOT NULL
    -- CONSTRAINT pk_ptrecls
    --  PRIMARY KEY
    LEFT JOIN payroll.ptrecls g
           ON f.nbrjobs_ecls_code = g.ptrecls_code
    -- tenure status info
    -- this table is not well documented,
    -- so joining in this way was determined by looking at the actual data in the table.
    LEFT JOIN payroll.perappt h
           ON h.perappt_pidm = a.pidm
          AND h.perappt_appt_eff_date = (SELECT MAX(h1.perappt_appt_eff_date)
                                           FROM payroll.perappt h1
                                          WHERE h.perappt_pidm = h1.perappt_pidm
                                            -- limit on the max for historical records
                                            AND h1.perappt_appt_eff_date < c.stvterm_end_date)
    -- ptrtenr_code varchar2(2 char)  NOT NULL
    --        CONSTRAINT pk_ptrtenr
    --            PRIMARY KEY
    LEFT JOIN payroll.ptrtenr i
           ON i.ptrtenr_code = h.perappt_tenure_code
    -- faculty rank info
    -- CONSTRAINT pk_sibinst
    --        PRIMARY KEY (sibinst_pidm, sibinst_term_code_eff)
    LEFT JOIN saturn.sibinst r
           ON A.pidm = r.sibinst_pidm
          AND r.sibinst_term_code_eff = ( SELECT MAX(r2.sibinst_term_code_eff)
                                             FROM saturn.sibinst r2
                                            WHERE r2.sibinst_pidm = r.sibinst_pidm
                                              -- limit on the max for historical records
                                              AND r2.sibinst_term_code_eff <= c.stvterm_code )
    LEFT JOIN saturn.stvfctg s
           ON s.stvfctg_code = r.sibinst_fctg_code
),
cte_faculty_term AS (
    SELECT a.pidm,
           a.term_code,
           LISTAGG( DISTINCT a.faculty_college, ' | ') WITHIN GROUP ( ORDER BY a.faculty_college) AS faculty_college,
           LISTAGG( DISTINCT a.faculty_department, ' | ') WITHIN GROUP ( ORDER BY a.faculty_department) AS faculty_department,
           LISTAGG( DISTINCT a.faculty_status, ' | ') WITHIN GROUP ( ORDER BY a.faculty_status) AS faculty_status,
           LISTAGG( DISTINCT a.faculty_rank, ' | ') WITHIN GROUP ( ORDER BY a.faculty_rank) AS faculty_rank
    FROM cte_faculty_term_position_detail a
    GROUP BY (a.pidm, a.term_code)
)
SELECT b.spriden_id AS sis_id,
       b.spriden_last_name || ', ' || b.spriden_first_name AS full_name,
       c.stvterm_code AS term_code,
       c.stvterm_desc AS term,
       c.stvterm_acyr_code AS academic_year,
       COALESCE(a.faculty_college, 'Missing') AS faculty_college,
       COALESCE(a.faculty_department, 'Missing') AS faculty_department,
       COALESCE(a.faculty_status, 'Missing') AS faculty_status,
       COALESCE(a.faculty_rank, 'Missing') AS faculty_rank
FROM cte_faculty_term a
LEFT JOIN saturn.spriden b
       ON b.spriden_pidm = a.pidm
      AND b.spriden_change_ind IS NULL
LEFT JOIN saturn.stvterm c
       ON c.stvterm_code = a.term_code
WHERE a.term_code >= 201840
;