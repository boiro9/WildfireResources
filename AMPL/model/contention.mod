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
param CWP  {I}; # CWP: Current work periods, antes CFP
param CRP  {I};
param CUP  {I}; # CUP: Current usage periods, antes CTFP
param TRP  {I}; # TRP: Time to rest point, antes FBRP
param WP   {I}; # WP: work periods, antes FP
param RP   {I};
param UP   {I}; # UP: usage periods, antes DFP
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
var tr {I, T} binary; # tr: travel, antes fl
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
var w {i in I, t in T} = u[i, t] - r[i, t] - tr[i, t];
var z {i in I}         = sum{t in T} e[i, t];


# =============================================================================
# Model
# =============================================================================

# Objective function
# ==================

# Auxiliary variables
# -------------------
var Cost = + sum{i in I, t in T} C[i]*u[i, t] 
		   + sum{i in I} P[i]*z[i] 
		   + sum{t in T} NVC[t]*y[t-1];

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
subject to cont_1:
	sum{t in T} PER[t]*y[t-1] <= sum{i in I, t in T} PR[i,t]*w[i,t]
;

subject to cont_2 {t in T}:
	sum{t1 in T_int[1,t]} PER[t1]*y[t-1] 
	<=
	sum{i in I, t1 in T_int[1,t]} PR[i,t1]*w[i,t1] + M*y[t]
;


# Start of activity
# -----------------
subject to start_act_1 {i in I, t in T}:
	A[i]*w[i,t] <= sum{t1 in T_int[1,t]} tr[i,t1]
;

subject to start_act_2 {i in I}:
	if ITW[i] == 1 then
		s[i,1] + sum{t in T_int[2,m]} (m+1)*s[i,t] - m*z[i]
	else
		sum{t in T} s[i,t] - z[i]
	<= 0
;


# End of activity
# ---------------
subject to end_act {i in I, t in T}:
	sum{t1 in T_int[max(1, min(m, t-TRP[i]+1)),t]} tr[i,t1] >= TRP[i]*e[i,t]
;


# Breaks
# ------

# Auxiliary variables
# ···················
var cr {i in I, t in T} = 
	if (ITW[i] == 0) and (IOW[i] == 0) then
		+ sum{t1 in T_int[1,t]} (t+1-t1)*s[i,t1]
	    - sum{t2 in T_int[1,t]} (t-t2)*e[i,t2]
		- sum{t3 in T_int[1,t]} r[i,t3]
		- sum{t4 in T_int[1,t]} WP[i]*er[i,t4]
	else
		+ (t+CWP[i]-CRP[i])*s[i,1]
		+ sum{t1 in T_int[2,t]} (t+1-t1+WP[i])*s[i,t1]
	    - sum{t2 in T_int[1,t]} (t-t2)*e[i,t2]
		- sum{t3 in T_int[1,t]} r[i,t3]
		- sum{t4 in T_int[1,t]} WP[i]*er[i,t4]
;


# Constraints
# ···········
subject to breaks_1 {i in I, t in T}:
	0 <= cr[i,t] <= WP[i]
;

subject to break_2 {i in I, t in T}:
	if t-RP[i] >= 0 then
		sum{t1 in T_int[max(1, t-RP[i]+1),t]} r[i,t1] 
	else
		CRP[i]*s[i,1] + sum{t1 in T_int[1,t]} r[i,t1]
	
	>= min(t, RP[i])*er[i,t]
;

subject to break_3 {i in I, t in T}:
	sum{t1 in T_int[t, min(t+RP[i]-1), m)]} er[i,t1] >= r[i,t]
;

subject to break_4 {i in I, t in T}:
	sum{t1 in T_int[max(1,t-TRP[i]),min(m,t+TRP[i])]} (r[i,t1]+tr[i,t1])
	>= sum{t1 in T_int[max(1,t-TRP[i]),min(m,t+TRP[i])]} r[i,t]
;

# Maximum number of usage periods in a day
# ----------------------------------------

subject to max_num_usage {i in I}:
	sum{t in T} u[i,t] <= UP[i] - CUP[i]
;


# Maximum and minimum number of resources of a group
# --------------------------------------------------

subject to min_group {g in G, t in T}:
	nMin[g,t]*y[t-1] <= sum{i in G_I[g]} w[i,t] + mu[g,t]
;

subject to max_group {g in G, t in T}:
	sum{i in G_I[g]} w[i,t] <= nMax[g,t]*y[t-1]
;


# Logical
# -------

subject to logical_1 {i in I}:
	sum{t in T} t*e[i,t] >= sum{t in T} t*s[i,t]
;

subject to logical_2 {i in I}:
	sum{t in T} e[i,t] <= 1
;

subject to logical_3 {i in I, t in T}:
	r[i,t] + tr[i,t] <= u[i,t]
;

subject to logical_4 {i in I}:
	sum{t in T} w[i,t] >= z[i]
;

subject to logical_5:
	y[0] = 1
;

#subject to logical_2 {i in I}:
#	sum{t in T} s[i,t] <= 1
#;

#subject to logical_1_aux {i in I, t in T}:
#	s[i,t] <= w[i,t] + tr[i,t]
#;

#subject to logical_2_aux {i in I}:
#	z[i] <= 1
#;

#subject to logical_3_aux {i in I}:
#	sum{t in T} s[i,t] <= 1
#;

