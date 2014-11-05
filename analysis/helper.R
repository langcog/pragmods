read.turk <- function (fname) {
  d <- read.table(fname,
                  header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
  names(d) <- str_replace(tolower(names(d)),"answer.","")
  d <- d %>%
    select(-hitid, -hittypeid, -title, -description, -keywords,
           -reward, -creationtime, -assignments, -numavailable, -numpending,
           -numcomplete, -hitstatus, -reviewstatus, -annotation,
           -assignmentduration, -autoapprovaldelay, -hitlifetime,
           -viewhit, -assignmentid, -assignmentstatus, -autoapprovaltime,
           -assignmentaccepttime, -assignmentapprovaltime,
           -assignmentrejecttime, -deadline, -feedback, -reject) %>%
    select(-about, -comment,          
           -familiarization_present_in_study,
           -participant_feature_count_condition, 
           -target_filler_sequence_condition, 
           -linguistic_framing_condition, 
           -question_type_condition) # substantive 
  
  types <- sapply(d, class)
  
  for (i in 1:length(types)) {
    if (types[[i]] == "character") {
      d[,names(types[i])] <- str_replace_all(d[,names(types[i])], 
                                                          "\"", "")
    }
  }
  
  d <- d %>% rename(time = assignmentsubmittime, 
                    targ.prop = target_property,
                    familiarization = familiarization_cond, 
                    response.cond = participant_response_type_condition,
                    targ.bet = money_allocated_to_target, 
                    dist.bet = money_allocated_to_logical,
                    foil.bet = money_allocated_to_foil,                    
                    mc.targ = manip_check_target, 
                    mc.dist = manip_check_dist, 
                    targ.pos = target_position, 
                    mc.name = name_check_correct, 
                    targ.likert = likert_value_target,
                    dist.likert = likert_value_foil,
                    foil.likert = likert_value_logical,
                    targ.2afc = choice_correct)
}