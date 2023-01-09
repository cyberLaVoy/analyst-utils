SELECT a.sprhold_pidm AS student_pidm,
       b.stvhldd_desc AS hold,
       a.sprhold_reason AS hold_reason
FROM saturn.sprhold a
LEFT JOIN saturn.stvhldd b
       ON a.sprhold_hldd_code = b.stvhldd_code
WHERE a.sprhold_to_date > sysdate;