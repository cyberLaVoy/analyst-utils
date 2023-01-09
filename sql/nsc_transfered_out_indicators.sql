SELECT DISTINCT a.requester_return_field AS sis_system_id,
                CASE
                           -- when college is anything but our institution (including null)
                    WHEN ( SUBSTR(a.college_branch_code, 1, 6) != '003671' OR a.college_branch_code IS NULL ) THEN TRUE
                    ELSE FALSE
                END AS nsc_transfered_out
FROM dscir.nsc_se_detail_rpt a
WHERE SUBSTR(a.college_branch_code, 1, 6) != '003671';