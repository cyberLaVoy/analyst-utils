WITH cte_population AS (
    SELECT UNIQUE a.sfrstcr_term_code,
                  a.sfrstcr_pidm
    FROM saturn.sfrstcr a
    -- include records that will be listed on an individuals transcript
    WHERE a.sfrstcr_rsts_code IN (SELECT a1.stvrsts_code
                                  FROM saturn.stvrsts a1
                                  WHERE a1.stvrsts_incl_sect_enrl = 'Y')
    -- records with a campus code like this have been manually selected to not be included in enrollment
    AND a.sfrstcr_camp_code != 'XXX'
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
ON c.stvstyp_code = b.sgbstdn_styp_code
WHERE a.sfrstcr_term_code = '202040'
