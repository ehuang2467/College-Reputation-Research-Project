state_demo = read.csv("state_demographic_data.csv", fileEncoding="UTF-8-BOM")
public_schools = data.frame(
  school_name = c(
    "UCLA",
    "UC Berkeley",
    "U Michigan Ann Arbor",
    "U Virginia",
    "UC Santa Barbara",
    "U Florida",
    "UNC Chapel Hill",
    "UC San Diego",
    "UC Irvine",
    "Georgia Tech",
    "UC Davis",
    "UT Austin",
    "William and Mary",
    "U Wisconsin Madison",
    "U Illinois Urbana",
    "U Georgia",
    "Ohio State",
    "Purdue",
    "Florida State",
    "U Maryland",
    "U Pittsburg",
    "U Washington",
    "Penn State",
    "Rutgers",
    "UConn",
    "Indiana U"
  ),
  state = c("CA", "CA", "MI", "VA", "CA", "FL", "NC", "CA", "CA", "GA", "CA", "TX", "VA",
            "WI", "IL", "GA", "OH", "IN", "FL", "MD", "PA", "WA", "PA", "NJ", "CT", "IN"), 
  public_rank = c(1:26)
)
public_schools["state_full"] = state.name[match(public_schools$state, state.abb)]
state_demo["top_rank"] = public_schools[state_demo$State == public_schools$state_full, "public_rank"]
top_rank = rep(NA, length(state_demo$State))
for(i in 1:length(state_demo$State)){
  state = state_demo[i, "State"]
  if(state %in% public_schools$state_full)
    top_rank[i] = min(public_schools[match(state, public_schools$state_full), "public_rank"])
  else
    top_rank[i] = 50
   
}
state_demo["top_rank"] = top_rank
logit = function(x) {
  m = max(x)+1
  log(x / (m - x))
}
state_demo["logit_toprank"] = -logit(top_rank)

public_regr = lm(logit_toprank ~ . -State -top_rank -Total, data = state_demo)
summary(public_regr)
