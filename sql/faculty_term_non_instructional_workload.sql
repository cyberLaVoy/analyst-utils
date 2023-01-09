SELECT c.spriden_id AS sis_id,
       a.sirnist_term_code AS term_code,
       /* workload */
       b.stvnist_desc AS description,
       CASE
           -- banked credits in the FALL are subtractive workload
           WHEN (a.sirnist_nist_code = 'BNKO' AND a.sirnist_term_code LIKE '%40') THEN
                COALESCE( 0 - a.sirnist_nist_workload , 0)
           ELSE
                a.sirnist_nist_workload
       END AS non_instructional_workload

FROM saturn.sirnist a
LEFT JOIN saturn.stvnist b
       ON a.sirnist_nist_code = b.stvnist_code
LEFT JOIN saturn.spriden c
       ON c.spriden_pidm = a.sirnist_pidm
      AND c.spriden_change_ind IS NULL
WHERE a.sirnist_term_code >= 201840
-- Remove records where there is no workload associated.
AND (a.sirnist_nist_workload IS NOT NULL AND a.sirnist_nist_workload != 0);