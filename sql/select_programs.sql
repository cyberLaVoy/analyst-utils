    SELECT  f.smrprle_program_desc AS program,
            a.sobcurr_degc_code AS degree,
            c.stvmajr_desc AS major,
            a.sobcurr_levl_code level_of_study,
            CASE
                WHEN e.stvcoll_desc = 'Coll of Sci, Engr & Tech' THEN 'CSET'
                WHEN e.stvcoll_desc = 'Technologies' THEN 'CSET'
                WHEN e.stvcoll_desc = 'Mathematics' THEN 'CSET'
                WHEN e.stvcoll_desc = 'Computer Information Tech' THEN 'CSET'
                WHEN e.stvcoll_desc = '* Natural Sciences' THEN 'CSET'
                WHEN e.stvcoll_desc = 'College of Business' THEN 'COB'
                WHEN e.stvcoll_desc = 'College of Health Sciences' THEN 'COHS'
                WHEN e.stvcoll_desc = 'Humanities & Social Sciences' THEN 'CHASS'
                WHEN e.stvcoll_desc = 'Coll of Humanities/Soc Sci' THEN 'CHASS'
                WHEN e.stvcoll_desc = 'History/Political Science' THEN 'CHASS'
                WHEN e.stvcoll_desc = 'College of the Arts' THEN 'COA'
                WHEN e.stvcoll_desc = '*Education/Family Studies/PE' THEN 'COE'
                WHEN e.stvcoll_desc = 'College of Education' THEN 'COE'
                WHEN e.stvcoll_desc = 'General Education' THEN 'GE'
                ELSE e.stvcoll_desc
            END AS college,
            COALESCE(g.stvdept_desc, 'Unavailable') AS department,
            COALESCE(c.stvmajr_valid_minor_ind, 'N') AS available_as_minor,
            COALESCE(c.stvmajr_valid_concentratn_ind, 'N') AS available_as_concentration,
            h.stvterm_desc initial_term,
            i.stvterm_desc effective_term
     FROM saturn.sobcurr a
LEFT JOIN saturn.sorcmjr b
       ON b.sorcmjr_curr_rule = a.sobcurr_curr_rule
LEFT JOIN saturn.stvmajr c
       ON c.stvmajr_code = b.sorcmjr_majr_code
LEFT JOIN saturn.stvdegc d
       ON d.stvdegc_code = a.sobcurr_degc_code
LEFT JOIN saturn.stvcoll e
       ON e.stvcoll_code = a.sobcurr_coll_code
LEFT JOIN saturn.smrprle f
       ON f.smrprle_program = a.sobcurr_program
LEFT JOIN saturn.stvdept g
       ON b.sorcmjr_dept_code = g.stvdept_code
LEFT JOIN saturn.stvterm h
       ON h.stvterm_code = a.sobcurr_term_code_init
LEFT JOIN saturn.stvterm i
       ON i.stvterm_code = b.sorcmjr_term_code_eff;
