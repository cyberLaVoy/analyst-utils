SELECT UNIQUE a.sirasgn_term_code AS term,
              a.sirasgn_pidm AS faculty,
              -- A unique identifier for a position is the combination of the posn code and suff code, for said position.
              a.sirasgn_posn || a.sirasgn_suff AS position
-- Instructional workload table.
FROM saturn.sirasgn a
UNION
SELECT UNIQUE b.sirnist_term_code AS term,
              b.sirnist_pidm AS faculty,
              -- A unique identifier for a position is the combination of the posn code and suff code, for said position.
              b.sirnist_posn || b.sirnist_suff AS position
-- Non-Instructional workload table.
FROM saturn.sirnist b
ORDER BY term, faculty, position;