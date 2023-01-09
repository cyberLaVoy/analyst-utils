
# the following this is NOT what the intention is:
if not_same(sgbstdn_student_type, calculated_student_type):
    choose calculated_student_type
# the intention is to compare these values and validate them in our system of record


# GROUP: High School ####
# if the expected high school graduation date is greater than the passed in parameter date, 
# then the student is a high school student on the provided parameter date
if expected_high_school_graduation_date_as_term > parameter_term:
    return "High School"

# GROUP Undergraduate ####
# first_term_enrolled, last_term_enrolled after expected_high_school_graduation_date
# and only looking at UNDERGRADUATE LEVEL transcript records for local institution.
  
## Freshman ####
if first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term:
  return "Freshman"

## Transfer ####
# has_transfer_undergraduate_credits after expected_high_school_graduation_date.
if has_transfer_undergraduate_credits_after_hs_grad
   AND (first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term):
  return "Transfer Undergraduate"

## Continuing ####
if last_term_enrolled_as_undergraduate_after_hs_grad >= (parameter_term - two_terms):
  return "Continuing Undergraduate"

## Readmit ####
if last_term_enrolled_as_undergraduate_after_hs_grad < (parameter_term - two_terms):
  return "Readmit Undergraduate"


# GROUP Graduate ####
# first_term_enrolled and last_term_enrolled only looking at GRADUATE LEVEL transcript records for local institution
## New Graduate ####
if first_term_enrolled_as_graduate == parameter_term:
    return "New Graduate"

## Transfer Graduate ####
if has_transfer_graduate_credits & ( first_term_enrolled_as_graduate == parameter_term )
    return "Transfer Graduate"

## Continuing Graduate ####
if last_term_enrolled_as_graduate >= (parameter_term - two_terms)
    return "Continuing Graduate"

## Readmit Graduate ####
if last_term_enrolled_as_graduate < (parameter_term - two_terms)
    return "Readmit Graduate"


# GROUP MISC ####
## Personal Interest, Non-Degree ####
# can only be determined by their current major
# non-high school
if current_major == "ND-CE" | current_major == "ND-ESL":
  return "Personal Interest, Non-Degree Seeking"

## TODO: add these to audit report?
## Undeclared ####
# select * from sgbstdn
# where sgbstdn_styp_code = '0';
# used 14 times, last used: 201540

## Special ####
# select * from sgbstdn
# where sgbstdn_styp_code = 'S';
# used 76 times, last used: 201930 - used for student level of NC (non-credit), last used for student level code of 201340
# U of U students
# TODO: need a classification for students from other institutions

## New Freshman from HS ####
# TODO: question, should we delete? data governance question


