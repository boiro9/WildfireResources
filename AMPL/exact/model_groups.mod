# EXACT

# =============================================================================
# Sets
# =============================================================================

param m;

set I;
set G;
set T := 1..m;

set G_I {G};
set T_int {t1 in {0} union T, t2 in {0} union T} := {t1..t2};


# =============================================================================
# Parameters
# =============================================================================

# Resources
# =========
param C    {I};
param P    {I};
param BPR  {I};
param A    {I};
param CFP  {I};
param CRP  {I};
param CTFP {I};
param FBRP {I};
param FP   {I};
param RP   {I};
param DFP  {I};
param ITW  {I} binary, default 0;  # If 1 resource is working in this wildfire
param IOW  {I} binary, default 0;  # If 1 resource is working in other wildfire

# Groups
# ======
param nMax {G, T};
param nMin {G, T};

# Wildfire
# ========
param PER  {T};
param NVC {T};
param EF  {I, T} default 1;

# Auxiliar
# ========
param PR {i in I, t in T} := BPR[i]*EF[i,t];
param M_prime      		  := 100*(sum{i in I}C[i] + sum{t in T}NVC[t]);
param M            		  := sum{t in T} PER[t] + sum{i in I, t in T} PR[i,t];


# =============================================================================
# Variables
# =============================================================================

# Resources
# =========
var s  {I, T} binary;
var fl {I, T} binary;
var r  {I, T} binary;
var er {I, T} binary;
var e  {I, T} binary;

# Wildfire
# ========
var y  {{0} union T} binary;
var mu {G, T}        integer, >=0;

# Auxiliary
# =========
var u {i in I, t in T} = + sum{t1 in T_int[1, t]}   s[i, t1] 
					     - sum{t2 in T_int[1, t-1]} e[i, t2];
var w {i in I, t in T} = u[i, t] - r[i, t] - fl[i, t];
var z {i in I}         = sum{t in T} e[i, t];


# =============================================================================
# Model
# =============================================================================

# Objective function
# ==================

# Auxiliary variables
# -------------------
var Res_Var_Cost  = sum{i in I, t in T} C[i] * u[i, t];
var Res_Fix_Cost  = sum{i in I} P[i]*z[i];
var WildFire_Cost = sum{t in T} NVC[t]*y[t-1];

var Cost = Res_Var_Cost + Res_Fix_Cost + WildFire_Cost;

var Penalty = sum{g in G, t in T} M_prime*mu[g, t] + y[m];

# Total Cost
# ----------
minimize Total_Cost: 
	Cost + Penalty
;

# Constraints
# ===========

# Wildfire containment
# --------------------
subject to wildfire_containment_1:
	sum{t in T} PER[t]*y[t-1] <= sum{i in I, t in T} PR[i,t]*w[i,t]
;

subject to wildfire_containment_2 {t in T}:
	sum{t1 in T_int[1,t]} PER[t1]*y[t-1] 
	<=
	sum{i in I, t1 in T_int[1,t]} PR[i,t1]*w[i,t1] + M*y[t]
;


# Start of activity
# -----------------
subject to start_of_activity_1 {i in I, t in T}:
	A[i]*w[i,t] <= sum{t1 in T_int[1,t]} fl[i,t1]
;

subject to start_of_activity_2 {i in I}:
	if ITW[i] == 1 then
		s[i,1] + sum{t in T_int[2,m]} (m+1)*s[i,t] - m*z[i]
	else
		sum{t in T} s[i,t] - z[i]
	<= 0
;


# End of activity
# ---------------
subject to end_of_activity {i in I, t in T}:
	sum{t1 in T_int[max(1, min(m, t-FBRP[i]+1)),t]} fl[i,t1] >= FBRP[i]*e[i,t]
;


# Breaks
# ------

# Auxiliary variables
# ···················
var ic {i in I, t in T} = 
	if (ITW[i] == 0) and (IOW[i] == 0) then
		+ sum{t1 in T_int[1,t]} (t+1-t1)*s[i,t1]
	else
		+ (t+CFP[i]-CRP[i])*s[i,1]
		+ sum{t1 in T_int[2,t]} (t+1-t1+FP[i])*s[i,t1]
;

var cr {i in I, t in T} = + ic[i,t]
						  - sum{t1 in T_int[1,t]} (t-t1)*e[i,t1]
						  - sum{t2 in T_int[1,t]} r[i,t2]
						  - sum{t3 in T_int[1,t]} FP[i]*er[i,t3]
;


# Constraints
# ···········
subject to breaks_1 {i in I, t in T}:
	0 <= cr[i,t] <= FP[i]
;

subject to break_2 {i in I, t in T}:
	if t-RP[i] >= 0 then
		sum{t1 in T_int[max(1, t-RP[i]+1),t]} r[i,t1] 
	else
		CRP[i]*s[i,1] + sum{t1 in T_int[1,t]} r[i,t1]
	
	>= RP[i]*er[i,t]
;

subject to break_3 {i in I, t in T}:
	sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} (r[i,t1]+fl[i,t1])
	>= sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} r[i,t]
;

# Maximum number of usage periods in a day
# ----------------------------------------

subject to maximum_num_usage_periods {i in I}:
	sum{t in T} u[i,t] <= DFP[i] - CTFP[i]
;


# Maximum and minimum number of resources of a group
# --------------------------------------------------

subject to min_num_res_group {t in T, g in G}:
	nMin[g,t]*y[t-1] <= sum{i in G_I[g]} w[i,t] + mu[g,t]
;

subject to max_num_res_group {t in T, g in G}:
	sum{i in G_I[g]} w[i,t] <= nMax[g,t]*y[t-1]
;


# Logical
# -------

subject to logical_1 {i in I}:
	sum{t in T} t*e[i,t] >= sum{t in T} t*s[i,t]
;

subject to logical_2 {i in I}:
	sum{t in T} s[i,t] <= 1
;

subject to logical_3 {i in I}:
	sum{t in T} e[i,t] <= 1
;

subject to logical_4 {i in I, t in T}:
	r[i,t] + fl[i,t] <= u[i,t]
;

subject to logical_5:
	y[0] = 1
;

#subject to logical_1_aux {i in I, t in T}:
#	s[i,t] <= w[i,t] + fl[i,t]
#;

#subject to logical_2_aux {i in I}:
#	z[i] <= 1
#;

#subject to logical_3_aux {i in I}:
#	sum{t in T} s[i,t] <= 1
#;

#subject to logical_4_aux {i in I}:
#	sum{t in T} w[i,t] >= z[i]
#;