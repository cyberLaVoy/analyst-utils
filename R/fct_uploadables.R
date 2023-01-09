#' Make APR Uploadable
#'
#' @description A fct function
#'
#' @param sql_results The results of a SQL query.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
#'
make_apr_uploadable <- function(sql_results) {
    apr_uploadable <- as.data.frame( matrix(ncol=65, nrow=nrow(sql_results)) )
    common_names <- c(
         # From data warehouse
         "student_athlete_last_name",
         "student_athlete_first_name",
         "student_athlete_middle_initial",
         "student_athlete_school_id_number",
         "ncaa_id",
         "student_athlete_gender",
         "student_athlete_ethnicity",
         "earned_associates_degree",
         "primary_major_cip_code",
         "total_hours_required_for_degree",
         "ap_credits",
         "total_cumulative_clep_credits_earned",
         "credits_earned_prior_to_full_time_enrollment",
         "summer_bridge_program",
         "met_cohort_definition",
         "this_term_code",
         "hours_attempted",
         "hours_earned",
         "remedial_hours",
         "gpa",
         "cumulative_gpa",
         "sport_code",
         "received_athletics_aid",
         "exhausted_eligibility",
         "first_year_any_university",
         "first_term_any_university",
         "first_year_your_university",
         "first_term_your_university",
         # From supplemental data
         "cumulative_credit_hours_earned_towards_degree",
         "degree_applicable_hours",
         "eligibility_status",
         "retention_status",
         "x6_hour_rule",
         "x18_27_hour_rule",
         "x24_26_hour_rule",
         "percentage_of_degree",
         "did_not_graduate_after_10_15_terms",
         "gpa_requirement",
         "conference_requirement",
         "institutional_requirement",
         "other_reason_for_ineligibility",
         "reason_for_not_returning_to_institution",
         "reason_for_leaving_institution_by_choice",
         "reason_for_allowable_exclusion_legislated_exception",
         "transfer_transferred_immediately_to_another_four_year_institution",
         "transfer_documentation_exists",
         "transfer_attended_your_institution_for_at_least_one_academic_year",
         "transfer_was_academically_elig",
         "transfer_cum_gpa_2_6_or_higher",
         "professional_athletics_documentation_exists",
         "professional_athletics_was_academically_elig_at_end_of_this_term",
         "medical_absence_did_the_student_athlete_receive_a_medical_absence_waiver",
         "medical_absence_is_there_documentation",
         "medical_absence_does_the_medical_absence_waiver_apply_to_the_term_in_which_the_retention_and_or_eligibility_point_was_lost",
         "missed_term_did_the_student_athlete_qualify_for_the_missed_term_exception_pursuant_to_bylaw_14_4_3_5_a",
         "missed_term_is_there_documentation",
         "missed_term_did_the_student_athlete_later_return_to_your_institution_as_a_full_time_student_during_a_regular_academic_term",
         "missed_term_did_the_student_athlete_earn_the_eligibility_point_in_the_last_term_of_enrollment_prior_to_departure",
         "missed_term_the_student_athlete_has_not_had_a_retention_point_adjusted_in_a_prior_term_due_to_the_missed_term_exception",
         "athletics_status",
         "transfer_institution_type",
         "transferable_credits",
         "x2_year_credits_transferable_toward_degree",
         "gpa_for_transferable_credit",
         "full_time_terms_at_most_recent_two_year_institution_attended_full_time"
    )
    col_names <- c(
        #
        "student_athlete_last_name",
        #
        "student_athlete_first_name",
        #
        "student_athlete_middle_initial",
        # (If foreign student flag is set to Y, the program will match this student based on this Identifier instead of the SSN.)
        "student_athlete_school_id_number",
        # (10 digit number assigned to the student.  Include leading 0 if applicable such as 0000112233).
        "ncaa_id",
        # (M=Male, F=Female)
        "student_athlete_gender",
        # (1=American Indian/Alaskan Native, 2=Asian, 2.1=Native Hawaiian/Pacific Islander, 3=Black/African American, 4=Hispanic/Latino, 5=White/Non-Hispanic, 6=Non-Resident Alien, 7=Unknown, 7.1=Two or More Races)
        "student_athlete_ethnicity",
        #(Format 2004) NUMBER (2004 = 2003-04 academic year, 2003 = 2002-03 academic year etc)
        "first_year_any_university",
        # (Semester schools: S1= Fall, S2=Spring; Quarter schools: Make sure academic calendar is set to quarter on the member setup page before the import: Q1=Fall, Q2=Winter, Q3=Spring)
        "first_term_any_university",
        # (Format 2004) NUMBER (2004 = 2003-04 academic year, 2003 = 2002-03 academic year etc.)
        "first_year_your_university",
        #(Semester schools: S1- Fall, S2-Spring) (Quarter schools: Make sure academic calendar is set to quarter on the member setup page before the import: Q1-Fall, Q2-Winter, Q3-Spring)
        "first_term_your_university",
        # (2= transferring from a 2-year institution, 4=transferring from a 4-year institution) Only valid if first term any university and first term your university are different terms.
        "transfer_institution_type",
        # NUMBER (Number of credits that transferred into your institution.)
        "transferable_credits",
        # NUMBER (Number of credits that transferred into your institution toward degree.)
        "x2_year_credits_transferable_toward_degree",
        # Grade-point-average in two-year college courses that transferred into the institution. NUMBER.
        "gpa_for_transferable_credit",
        # ? (Y=Yes, N=No).
        "earned_associates_degree",
        # : Total number of terms of full-time enrollment by the student-athlete at the two-year institution where he or she graduated/most recently attended as a full-time student.  NUMBER
        #"Full time terms at most recent two year institution attended full time",
        "full_time_terms_at_most_recent_two_year_institution_attended_full_time",
        #(Y=Yes, N=No).  Only required if the first term at your institution is the year for which you are submitting data.  Indicate whether student-athlete attended the summer term prior to initial full-time enrollment and received aid per NCAA Bylaw 15.2.8.1.4.
        "summer_bridge_program",
        # (Use CIP Codes)
        # "Degree Code",
        "primary_major_cip_code",
        # NUMBER
        "cumulative_credit_hours_earned_towards_degree",
        # NUMBER
        "total_hours_required_for_degree",
        # NUMBER
        "ap_credits",
        # NUMBER
        # "CLEP Credits",
        "total_cumulative_clep_credits_earned",
        # NUMBER
        "credits_earned_prior_to_full_time_enrollment",
        # (Semester schools: S1 = Fall, S2=Spring, SU=Summer, SP=Summer Bridge Quarter schools: Q1=Fall, Q2=Winter, Q3=Spring, SU=Summer, SP=Summer Bridge Interim term codes: I1, I2, I3)
        "this_term_code",
        # (Y=Yes, N=No)
        "met_cohort_definition",
        # NUMBER
        "hours_attempted",
        # NUMBER
        "hours_earned",
        # NUMBER
        "remedial_hours",
        # NUMBER
        "degree_applicable_hours",
        # NUMBER
        "gpa",
        #  NUMBER
        "cumulative_gpa",
        # (Y=Yes, N=No, M= Medical Absence Waiver)
        "eligibility_status",
        #(Y=Yes, N=No, A=Allowable Exclusion/Legislated Exception, G=Graduated this term, C=Continuing Post BA/BS, T=Transfer (only if student-athlete meets criteria for transfer adjustment), P=Professional Athletics (only if student-athlete meets criteria for professional athletics adjustment, M= Medical Absence Waiver, J= Missed Term)
        "retention_status",
        # [Ineligible due to 6-hour rule] (Y=Yes, N=No)
        "x6_hour_rule",
        # [Ineligible due to 18 / 27-hour rule] (Y=Yes, N=No)
        "x18_27_hour_rule",
        # [Ineligible due to 24 / 36-hour rule] (Y=Yes, N=No)
        "x24_26_hour_rule",
        # [Ineligible due to percentage of degree requirement] (Y=Yes, N=No)
        "percentage_of_degree",
        # [Ineligible due to not graduating after 10 / 15 terms] (Y=Yes, N=No)
        "did_not_graduate_after_10_15_terms",
        # [Ineligible due to not meeting an NCAA GPA requirement] (Y=Yes, N=No)
        "gpa_requirement",
        # [Ineligible due to not meeting a conference requirement] (Y=Yes, N=No)
        "conference_requirement",
        # [Ineligible due to not meeting an institutional requirement] (Y=Yes, N=No)
        "institutional_requirement",
        # [Ineligible due to a reason not listed] (Text description of reason)
        "other_reason_for_ineligibility",
        # (S=Left institution by choice (know not to transfer), T=Transfer to another institution (does not meet adjustment criteria), G=Suspended/Dismissed, E=enrolled part-time, U=Unknown/Other)
        "reason_for_not_returning_to_institution",
        # (P=Professional Athletics, does not meet adjustment criteria, F=Family Circumstances, H=Health of Student-Athlete, U=Unknown/Other)
        "reason_for_leaving_institution_by_choice",
        # (M=Armed service, C=Official church mission, R=Recognized foreign aid services, P=Pregnancy, A=Athletics Activities Waiver, D=Death or permanently disabled)
        "reason_for_allowable_exclusion_legislated_exception",
        # (Y=Yes, N=No)
        "transfer_transferred_immediately_to_another_four_year_institution",
        # (Y=Yes, N=No)
        "transfer_documentation_exists",
        # (Y=Yes, N=No)
        "transfer_attended_your_institution_for_at_least_one_academic_year",
        # (Y=Yes, N=No)
        "transfer_was_academically_elig",
        # (Y=Yes, N=No)
        # "Transfer meeting applicable percentage of degree requirement at time of departure per APR policy",
        "transfer_cum_gpa_2_6_or_higher",
        # (Y=Yes, N=No)
        "professional_athletics_documentation_exists",
        # (Y=Yes, N=No)
        "professional_athletics_was_academically_elig_at_end_of_this_term",
        # (Y=Yes, N=No)
        "medical_absence_did_the_student_athlete_receive_a_medical_absence_waiver",
        # (Y=Yes, N=No)
        "medical_absence_is_there_documentation",
        # (Y=Yes, N=No)
        "medical_absence_does_the_medical_absence_waiver_apply_to_the_term_in_which_the_retention_and_or_eligibility_point_was_lost",
        # (Y=Yes, N=No)
        "missed_term_did_the_student_athlete_qualify_for_the_missed_term_exception_pursuant_to_bylaw_14_4_3_5_a",
        # (Y=Yes, N=No)
        "missed_term_is_there_documentation",
        # (Y=Yes, N=No)
        "missed_term_did_the_student_athlete_later_return_to_your_institution_as_a_full_time_student_during_a_regular_academic_term",
        # (Y=Yes, N=No)
        "missed_term_did_the_student_athlete_earn_the_eligibility_point_in_the_last_term_of_enrollment_prior_to_departure",
        # (Y=Yes, N=No)
        "missed_term_the_student_athlete_has_not_had_a_retention_point_adjusted_in_a_prior_term_due_to_the_missed_term_exception",
        # (Use NCAA Sport Codes)
        "sport_code",
        # (Y=Yes, N=No)
        #"Received Athletics Aid (or Recruited – if your school does not grant athletics aid or does not grant aid for this sport)",
        "received_athletics_aid",
        # (C=Competed, N=Did not compete)
        "athletics_status",
        # (Y=Yes, N=No)
        # "Exhausted Eligibility for this sport this year"
        "exhausted_eligibility"
    )
    colnames(apr_uploadable) <- col_names
    apr_uploadable[,common_names] <- sql_results[,common_names]
    return(apr_uploadable)
}

