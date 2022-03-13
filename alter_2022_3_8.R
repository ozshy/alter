### Alternating moves paper

### R packages needed
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library("xtable") #exporting to LaTeX

### The Ultimatum Game (Table 1 in the paper)
(t.vec = 0:-49)# time going backwards with 50 potential pre-bargaining
length(t.vec)
evenodd.vec=t.vec
(evenodd.vec =  ifelse(evenodd.vec %% 2 == 0, "even", "odd"))
#
# Define surplus at t
(s.vec = seq(98, 0, -2))
length(s.vec)
#
#profita
profita.vec = rep(NA, length(t.vec))
(profita.vec = ifelse(evenodd.vec == "even", 99+t.vec, -1*t.vec +1))
#profitb
(profitb.vec = 100-profita.vec)
#
# column of who's the offerer
(offerer.vec = ifelse(evenodd.vec == "even", "A", "B"))

# Finalizing Ultimatum table (Table 1)
(ultimatum.df = data.frame(t.vec, offerer.vec, profita.vec, profitb.vec, s.vec))


### Figure 1 (profits and declining surplus)
phi1 = 0.8# share in surplus of first mover
phi2 = 0.9# share in surplus of first mover (not plotted)
#
(t_phi.vec = t.vec)
(evenodd_phi.vec = evenodd.vec)
#
s_phi1.vec = rep(NA, length(t_phi.vec))# initialize
s_phi2.vec = rep(NA, length(t_phi.vec))# initialize
profita_phi1.vec = rep(NA, length(t_phi.vec))#
profita_phi2.vec = rep(NA, length(t_phi.vec))# 
profitb_phi1.vec = rep(NA, length(t_phi.vec))#
profitb_phi2.vec = rep(NA, length(t_phi.vec))# 
#
(s_phi1.vec[1] = 2*phi1-1)# surplus at t
(s_phi2.vec[1] = 2*phi2-1)
(profita_phi1.vec[1] = phi1)
(profita_phi2.vec[1] = phi2)
(profitb_phi1.vec[1] = 1-phi1)
(profitb_phi2.vec[1] = 1-phi2)
#
for (i in t_phi.vec) { 
 s_phi1.vec[-i+1] = (s_phi1.vec[1])^(-i+1);
 s_phi2.vec[-i+1] = (s_phi2.vec[1])^(-i+1)
}
s_phi1.vec
s_phi2.vec
#
for (i in t_phi.vec) { 
profita_phi1.vec[-i+2] = ifelse(evenodd.vec[-i+2]=="odd",  profita_phi1.vec[-i+1] -phi1*s_phi1.vec[-i+1], profita_phi1.vec[-i+1] +phi1*s_phi1.vec[-i+1]);
profitb_phi1.vec[-i+2] = 1-profita_phi1.vec[-i+2];
profita_phi2.vec[-i+2] = ifelse(evenodd.vec[-i+2]=="odd",  profita_phi2.vec[-i+1] -phi2*s_phi2.vec[-i+1], profita_phi2.vec[-i+1] +phi2*s_phi2.vec[-i+1]);
profitb_phi2.vec[-i+2] = 1-profita_phi2.vec[-i+2];
}
# the profit vectors are trimmed to fit the time variable
(profita_phi1.vec = profita_phi1.vec[1:length(t_phi.vec)])
(profitb_phi1.vec = profitb_phi1.vec[1:length(t_phi.vec)])
#
(profita_phi2.vec = profita_phi2.vec[1:length(t_phi.vec)])
(profitb_phi2.vec = profitb_phi2.vec[1:length(t_phi.vec)])

# finalizing Figure 1: profits in the general model
(general.df = data.frame(Pregame_time=t_phi.vec, Offerer=offerer.vec, Profit_A1=profita_phi1.vec, Profit_B1=profitb_phi1.vec, s_phi1.vec, Profit_A2=profita_phi2.vec, Profit_B2=profitb_phi2.vec, s_phi2.vec))
# trim to 10 rows
(general.df = general.df[1:10, ])
#
ggplot(general.df, aes(x=Pregame_time)) +geom_point(aes(y=Profit_A1), size=4) + geom_line(aes(y=Profit_A1), linetype="solid", size=1.2) +geom_point(aes(y=Profit_B1), size=4, color="blue") + geom_line(aes(y=Profit_B1), linetype="longdash", size=1.2, color="blue") +geom_point(aes(y=s_phi1.vec), size=4, color="red") + geom_line(aes(y=s_phi1.vec), color="red", linetype="dotdash", size=1.2) + scale_x_continuous(breaks = seq(0,-10,-1), labels = t_phi.vec[1:11]) + scale_y_continuous(breaks = seq(0,1,0.05)) + theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + xlab("Bargaining pregame (t)") + ylab("Surpluses and players' bargaining payoffs") + annotate("text", x = -0.4, y = 0.75, label =TeX("$\\pi_t^A"), size = 10, color="black") + annotate("text", x = -0.4, y = 0.25, label =TeX("$\\pi_t^B"), size = 10, color="blue") + annotate("text", x = -2.0, y = 0.049, label ="game-generated surplus", size = 8, color="red") + annotate("text", x = -4, y = 0.04, label =TeX("$s_t="), size = 10, color="red") + annotate("text", x = 0, y = -0.02, label ="A", size = 6, color="black") + annotate("text", x = -1, y = -0.02, label ="B", size = 6, color="black") + annotate("text", x = -2, y = -0.02, label ="A", size = 6, color="black") + annotate("text", x = -3, y = -0.02, label ="B", size = 6, color="black") + annotate("text", x = -4, y = -0.02, label ="A", size = 6, color="black") + annotate("text", x = -5, y = -0.02, label ="B", size = 6, color="black") + annotate("text", x = -6, y = -0.02, label ="A", size = 6, color="black")+ annotate("text", x = -7, y = -0.02, label ="B", size = 6, color="black")+ annotate("text", x = -8, y = -0.02, label ="A", size = 6, color="black") + annotate("text", x = -9, y = -0.02, label ="B", size = 6, color="black")

### Table 2: Number of pregames
(phi.vec = seq(0.6,0.9,0.1))# first-mover share
(phi_2nd.vec = 1-phi.vec) # second-mover share
# below this surplus bargains do not hold a pre-game
(s02 = 0.2)
(s01 = 0.1)
(s005 = 0.05)
(s001 = 0.01)
# number of pregames under a threshold
(n02.vec =  -ceiling(log(s02)/log(2*phi.vec-1)-1))
(n01.vec =  -ceiling(log(s01)/log(2*phi.vec-1)-1))
(n005.vec = -ceiling(log(s005)/log(2*phi.vec-1)-1))
(n001.vec = -ceiling(log(s001)/log(2*phi.vec-1)-1))

# finalizing table 2
(ngames.df = data.frame(phi.vec, phi_2nd.vec, n001.vec, n005.vec, n01.vec,  n02.vec))
#
dim(ngames.df)

(digitm = matrix(c(1, 1, 1, 0, 0, 0, 0), nrow = 4, ncol = 6+1, byrow = T))# 1st column is adhoc (required)
#

print(xtable(ngames.df, digits = digitm), include.rownames = F, hline.after = c(0))
# Starts Line 197 Table 1 printed on line 467
