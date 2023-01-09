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
       a.sfrstcr_pidm
FROM cte_population a;
