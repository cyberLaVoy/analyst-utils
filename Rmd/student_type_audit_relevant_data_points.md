## Relevant data points for student type audit


#### Continuous monitoring needed

> banner.sgbstdn.sgbstdn_styp_code
>
> banner.spbpers.spbpers_birth_date        
>
> banner.sorhsch.sorhsch_graduation_date           
>
> banner.sgbstdn.sgbstdn_levl_code
>
> banner.stvrsts.stvrsts_incl_sect_enrl
>
> banner.sfrstcr.sfrstcr_rsts_code
>
> banner.sfrstcr.sfrstcr_camp_code
>
> banner.shrtgpa.shrtgpa_levl_code
>
> banner.shrtgpa.shrtgpa_gpa_type_ind
>
> banner.stvsbgi.stvsbgi_type_ind
>
> banner.stvsbgi.stvsbgi_srce_ind
> 
> banner.stvdegc.stvdegc_dlev_code
>
> banner.sgbstdn.sgbstdn_levl_code
>
> banner.stvterm.stvterm_code
>
> banner.sfrstcr.sfrstcr_term_code
>
> banner.shrtgpa.shrtgpa_term_code


#### One-shot fixes

> flat_files.historic_transcript_records.quarter
>
> flat_files.historic_transcript_records.year
>               
> ushe.students.s_term
>
> ushe.students.s_year
>
> ushe.students.s_level
>
> ushe.students.s_extract


### Student type determination variables

> calculated_high_school_graduation_term

With this variable, we use the high school graduation date from Banner. If that date is not available, we use the birth date in Banner to derive the best guess for when the high school graduation date would be. We then convert that date into the term that date falls within. This is the most critical variable, as it is used to derive all other variables.

> first_term_enrolled_on_or_after_calculated_hs_graduation_term
>
> last_term_enrolled_on_or_after_calculated_hs_graduation_term

Taking into account all current and historical enrollment data available, these are the first and last terms (after high school graduation) that one would see on a student's transcript.

> student_level

This is the student level associate with the first and last term enrolled variables.

> has_transfer_credits_on_or_after_calculated_hs_graduation_term

This is a boolean value (yes, or no) that tells us if they have any transfer credits from another university (after high school graduation). 

> transfer_credits_level

This is the transfer credits level associate with the has transfer credits variable.


#### Summary

Taking all of these variables into account, we are able to calculate a given student's type classification for any given term. In words, we would say the following, in order:

- If their calculated high school graduation term is greater than the provided term, then they are classified as a "High School" student.

- If their last term enrolled as an undergraduate (after high school graduation) is equal to or greater than the term two terms prior to the provided term, then they are classified as a "Continuing Undergraduate".

- If their last term enrolled as an undergraduate (after high school graduation) is less than the term two terms prior to the provided term, then they are classified as a "Readmit Undergraduate".

- If they have undergraduate transfer credits (after high school graduation) and their first term enrolled as an undergraduate (after high school graduation) is equal to the proved term, then they are classified as a "Transfer Undergraduate".

- If their first term enrolled as an undergraduate (after high school graduation) is equal to the provided term, then they are classified as a "Freshman".

The same exact logic is applied to all calculations for graduate students. The only difference is, we look at graduate enrollment and graduate transfer credits instead.

This gives us 4 groupings of students:

1. High School Students
2. Undergraduate Students
3. Graduate Students
4. And a miscellaneous group (any students that don't fit into the first three groups)
