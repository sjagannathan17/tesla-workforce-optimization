# ============================================================
# TESLA OPTIMIZATION -Group 5
# ============================================================

library(alabama)

cat("============================================================\n")
cat("TESLA SUPPORT STAFFING OPTIMIZATION \n")
cat("============================================================\n\n")

# ============================================================
# PART A & B: MONOLINGUAL AGENTS (SEPARATE ENGLISH/SPANISH)
# ============================================================

cat("============================================================\n")
cat("PART A & B: MONOLINGUAL AGENTS\n")
cat("============================================================\n\n")

# -----------------------------------------------------------
# ENGLISH-SPEAKING AGENTS (80% of calls)
# -----------------------------------------------------------

cat("English-speaking agents (80% of calls):\n")
cat("-----------------------------------------------------------\n")

f = function(x) (120*x[1]+120*x[2]+120*x[3]+150*x[4]+150*x[5]+150*x[6]+180*x[7])

g = function(x){
  h = 0
  h[1] = 6*x[1]-32
  h[2] = 6*x[2]-68
  h[3] = 6*(x[1]+x[3])-56
  h[4] = 6*(x[2]+x[4])-76
  h[5] = 6*(x[3]+x[5]+x[6])-64
  h[6] = 6*(x[4]+x[6]+x[7])-28
  h[7] = 6*(x[5]+x[7])-8
  h[8] = x[1]
  h[9] = x[2]
  h[10] = x[3]
  h[11] = x[4]
  h[12] = x[5]
  h[13] = x[6]
  h[14] = x[7]
  return(h)
}

p0 = c(15,15,15,15,15,15,15)

y_eng = constrOptim.nl(p0, f, hin=g)

cat("\nOptimal solution:", round(y_eng$par,3), "\n")
cat("Optimal cost:", round(y_eng$value,3), "\n\n")

cat("FT starting 7 AM:  ", round(y_eng$par[1]), "agents\n")
cat("FT starting 9 AM:  ", round(y_eng$par[2]), "agents\n")
cat("FT starting 11 AM: ", round(y_eng$par[3]), "agents\n")
cat("FT starting 1 PM:  ", round(y_eng$par[4]), "agents\n")
cat("FT starting 3 PM:  ", round(y_eng$par[5]), "agents\n")
cat("PT starting 3 PM:  ", round(y_eng$par[6]), "agents\n")
cat("PT starting 5 PM:  ", round(y_eng$par[7]), "agents\n")
cat("English cost: $", round(y_eng$value, 2), "\n\n")

# -----------------------------------------------------------
# SPANISH-SPEAKING AGENTS (20% of calls)
# -----------------------------------------------------------

cat("Spanish-speaking agents (20% of calls):\n")
cat("-----------------------------------------------------------\n")

f_span = function(x) (120*x[1]+120*x[2]+120*x[3]+150*x[4]+150*x[5])

g_span = function(x){
  h = 0
  h[1] = 6*x[1]-8
  h[2] = 6*x[2]-17
  h[3] = 6*(x[1]+x[3])-14
  h[4] = 6*(x[2]+x[4])-19
  h[5] = 6*(x[3]+x[5])-16
  h[6] = 6*x[4]-7
  h[7] = 6*x[5]-2
  h[8] = x[1]
  h[9] = x[2]
  h[10] = x[3]
  h[11] = x[4]
  h[12] = x[5]
  return(h)
}

p0_span = c(5,5,5,5,5)

y_span = constrOptim.nl(p0_span, f_span, hin=g_span)

cat("\nOptimal solution:", round(y_span$par,3), "\n")
cat("Optimal cost:", round(y_span$value,3), "\n\n")

cat("FT starting 7 AM:  ", round(y_span$par[1]), "agents\n")
cat("FT starting 9 AM:  ", round(y_span$par[2]), "agents\n")
cat("FT starting 11 AM: ", round(y_span$par[3]), "agents\n")
cat("FT starting 1 PM:  ", round(y_span$par[4]), "agents\n")
cat("FT starting 3 PM:  ", round(y_span$par[5]), "agents\n")
cat("Spanish cost: $", round(y_span$value, 2), "\n\n")

# -----------------------------------------------------------
# PART B: TOTAL COST
# -----------------------------------------------------------

total_cost_b = y_eng$value + y_span$value

cat("============================================================\n")
cat("ANSWER TO PART B:\n")
cat("Total minimum cost: $", round(total_cost_b, 2), "\n")
cat("============================================================\n\n")

# ============================================================
# PART C & D: MONOLINGUAL WITH CONSTRAINT
# ============================================================

cat("============================================================\n")
cat("PART C & D: MONOLINGUAL WITH HIRING CONSTRAINT\n")
cat("============================================================\n\n")

cat("English-speaking agents (with constraint):\n")
cat("-----------------------------------------------------------\n")

f_eng_c = function(x) (120*x[1]+120*x[2]+120*x[3]+150*x[4]+150*x[5]+150*x[6]+180*x[7])

g_eng_c = function(x){
  h = 0
  h[1] = 6*x[1]-32
  h[2] = 6*x[2]-68
  h[3] = 6*(x[1]+x[3])-56
  h[4] = 6*(x[2]+x[4])-76
  h[5] = 6*(x[3]+x[5]+x[6])-64
  h[6] = 6*(x[4]+x[6]+x[7])-28
  h[7] = 6*(x[5]+x[7])-8
  h[8] = x[1]
  h[9] = x[2]
  h[10] = x[3]
  h[11] = x[4]
  h[12] = x[5]
  h[13] = x[6]
  h[14] = x[7]
  h[15] = 1-x[4]
  h[16] = 1-x[5]
  return(h)
}

p0_eng_c = c(25, 25, 25, 0.1, 0.1, 30, 30)

y_eng_c = constrOptim.nl(p0_eng_c, f_eng_c, hin=g_eng_c)

cat("\nOptimal solution:", round(y_eng_c$par,3), "\n")
cat("Optimal cost:", round(y_eng_c$value,3), "\n\n")

cat("FT starting 7 AM:  ", round(y_eng_c$par[1]), "agents\n")
cat("FT starting 9 AM:  ", round(y_eng_c$par[2]), "agents\n")
cat("FT starting 11 AM: ", round(y_eng_c$par[3]), "agents\n")
cat("FT starting 1 PM:  ", round(y_eng_c$par[4]), "agents\n")
cat("FT starting 3 PM:  ", round(y_eng_c$par[5]), "agents\n")
cat("PT starting 3 PM:  ", round(y_eng_c$par[6]), "agents\n")
cat("PT starting 5 PM:  ", round(y_eng_c$par[7]), "agents\n")
cat("English cost: $", round(y_eng_c$value, 2), "\n\n")

cat("Spanish-speaking agents (unchanged from Part A):\n")
cat("-----------------------------------------------------------\n")
cat("Spanish cost: $", round(y_span$value, 2), "\n\n")

# -----------------------------------------------------------
# PART D: TOTAL COST WITH CONSTRAINT
# -----------------------------------------------------------

total_cost_d = y_eng_c$value + y_span$value

cat("============================================================\n")
cat("ANSWER TO PART D:\n")
cat("Total minimum cost with constraint: $", round(total_cost_d, 2), "\n")
cat("Cost increase from Part B: $", round(total_cost_d - total_cost_b, 2), "\n")
cat("============================================================\n\n")

# ============================================================
# PART E & F: BILINGUAL AGENTS
# ============================================================

cat("============================================================\n")
cat("PART E & F: BILINGUAL AGENTS\n")
cat("============================================================\n\n")

cat("Bilingual agents (100% of calls):\n")
cat("-----------------------------------------------------------\n")

f_bilingual = function(x) (120*x[1]+120*x[2]+120*x[3]+150*x[4]+150*x[5]+150*x[6]+180*x[7])

g_bilingual = function(x){
  h = 0
  h[1] = 6*x[1]-40
  h[2] = 6*x[2]-85
  h[3] = 6*(x[1]+x[3])-70
  h[4] = 6*(x[2]+x[4])-95
  h[5] = 6*(x[3]+x[5]+x[6])-80
  h[6] = 6*(x[4]+x[6]+x[7])-35
  h[7] = 6*(x[5]+x[7])-10
  h[8] = x[1]
  h[9] = x[2]
  h[10] = x[3]
  h[11] = x[4]
  h[12] = x[5]
  h[13] = x[6]
  h[14] = x[7]
  return(h)
}

p0_bilingual = c(25,25,25,25,25,25,25)

y_bilingual = constrOptim.nl(p0_bilingual, f_bilingual, hin=g_bilingual)

cat("\nOptimal solution:", round(y_bilingual$par,3), "\n")
cat("Optimal cost:", round(y_bilingual$value,3), "\n\n")

cat("FT starting 7 AM:  ", round(y_bilingual$par[1]), "agents\n")
cat("FT starting 9 AM:  ", round(y_bilingual$par[2]), "agents\n")
cat("FT starting 11 AM: ", round(y_bilingual$par[3]), "agents\n")
cat("FT starting 1 PM:  ", round(y_bilingual$par[4]), "agents\n")
cat("FT starting 3 PM:  ", round(y_bilingual$par[5]), "agents\n")
cat("PT starting 3 PM:  ", round(y_bilingual$par[6]), "agents\n")
cat("PT starting 5 PM:  ", round(y_bilingual$par[7]), "agents\n")
cat("Bilingual cost: $", round(y_bilingual$value, 2), "\n\n")

# -----------------------------------------------------------
# PART F: BILINGUAL MINIMUM COST
# -----------------------------------------------------------

cat("============================================================\n")
cat("ANSWER TO PART F:\n")
cat("Minimum cost with bilingual agents: $", round(y_bilingual$value, 2), "\n")
cat("Savings vs monolingual: $", round(total_cost_b - y_bilingual$value, 2), "\n")
cat("============================================================\n\n")

# ============================================================
# PART G: MAXIMUM WAGE INCREASE FOR BILINGUAL
# ============================================================

cat("============================================================\n")
cat("PART G: MAXIMUM WAGE INCREASE FOR BILINGUAL AGENTS\n")
cat("============================================================\n\n")

cat("Cost comparison:\n")
cat("-----------------------------------------------------------\n")
cat("Monolingual cost (Part B): $", round(total_cost_b, 2), "\n")
cat("Bilingual cost (Part F):   $", round(y_bilingual$value, 2), "\n")
cat("Budget room:               $", round(total_cost_b - y_bilingual$value, 2), "\n\n")

max_multiplier = total_cost_b / y_bilingual$value
max_percentage_increase = (max_multiplier - 1) * 100

cat("============================================================\n")
cat("PART G:\n")
cat("Maximum wage increase: ", round(max_percentage_increase, 1), "%\n")
cat("============================================================\n\n")

cat("New wage rates at maximum increase:\n")
cat("-----------------------------------------------------------\n")
cat("Before 5 PM: $30.00 -> $", round(30 * max_multiplier, 2), "/hour\n")
cat("After 5 PM:  $45.00 -> $", round(45 * max_multiplier, 2), "/hour\n\n")

new_total_cost = y_bilingual$value * max_multiplier
cat("Verification:\n")
cat("New bilingual cost at ", round(max_percentage_increase, 1), "% increase: $", 
    round(new_total_cost, 2), "\n")
cat("Equals monolingual cost: ", 
    ifelse(abs(new_total_cost - total_cost_b) < 1, "YES", "NO"), "\n\n")

# ============================================================
# FINAL SUMMARY OF ALL ANSWERS
# ============================================================

cat("============================================================\n")
cat("FINAL SUMMARY - ALL ANSWERS\n")
cat("============================================================\n\n")

cat("PART A: Optimal staffing for monolingual agents\n")
cat("  English: FT(7AM)=", round(y_eng$par[1]), 
    " FT(9AM)=", round(y_eng$par[2]),
    " FT(11AM)=", round(y_eng$par[3]),
    " FT(1PM)=", round(y_eng$par[4]),
    " FT(3PM)=", round(y_eng$par[5]),
    " PT(3PM)=", round(y_eng$par[6]),
    " PT(5PM)=", round(y_eng$par[7]), "\n")
cat("  Spanish: FT(7AM)=", round(y_span$par[1]),
    " FT(9AM)=", round(y_span$par[2]),
    " FT(11AM)=", round(y_span$par[3]),
    " FT(1PM)=", round(y_span$par[4]),
    " FT(3PM)=", round(y_span$par[5]), "\n\n")

cat("PART B: Total minimum cost (monolingual)\n")
cat("  Answer: $", round(total_cost_b, 2), "\n\n")

cat("PART C: Optimal staffing with constraint\n")
cat("  English: FT(7AM)=", round(y_eng_c$par[1]),
    " FT(9AM)=", round(y_eng_c$par[2]),
    " FT(11AM)=", round(y_eng_c$par[3]),
    " FT(1PM)=", round(y_eng_c$par[4]),
    " FT(3PM)=", round(y_eng_c$par[5]),
    " PT(3PM)=", round(y_eng_c$par[6]),
    " PT(5PM)=", round(y_eng_c$par[7]), "\n\n")

cat("PART D: Total minimum cost with constraint\n")
cat("  Answer: $", round(total_cost_d, 2), "\n\n")

cat("PART E: Optimal staffing for bilingual agents\n")
cat("  Bilingual: FT(7AM)=", round(y_bilingual$par[1]),
    " FT(9AM)=", round(y_bilingual$par[2]),
    " FT(11AM)=", round(y_bilingual$par[3]),
    " FT(1PM)=", round(y_bilingual$par[4]),
    " FT(3PM)=", round(y_bilingual$par[5]),
    " PT(3PM)=", round(y_bilingual$par[6]),
    " PT(5PM)=", round(y_bilingual$par[7]), "\n\n")

cat("PART F: Minimum cost with bilingual agents\n")
cat("  Answer: $", round(y_bilingual$value, 2), "\n\n")

cat("PART G: Maximum wage increase for bilingual agents\n")
cat("  Answer: ", round(max_percentage_increase, 1), "%\n\n")


cat("END OF SOLUTION\n")
