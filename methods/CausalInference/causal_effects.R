# https://cran.r-project.org/web/packages/causaleffect/vignettes/causaleffect.pdf

library(causaleffect)
library(igraph)

# helper functions
get_latex_expr <- function(ce) {
  cat("Paste the following in https://www.codecogs.com/latex/eqneditor.php:\n")
  cat(ce, '\n')
}

# A.2.1 From Causal Inference and Data-Fusion in Econometrics
 # Paul Hunermund & Elias Bareinboim (2019)

college_wage <- graph.formula(
  # Directed Edges 
  W -+ Y,
  W -+ H,
  H -+ Y,
  C -+ Y,
  C -+ W,
  E -+ C,
  E -+ Y,

  # Bidirected edges
  W -+ Y,
  Y -+ W,

  C -+ E,
  E -+ C,

  simplify = FALSE)

# Set our U's for all bidirected edges
# Note: Any bidirected edge corresponding to an unobserved variable must be denoted
# by using two unidirected edges with a description attribute of value "U". 
# (Page 2 of the reference manual):
# https://cran.r-project.org/web/packages/causaleffect/causaleffect.pdf

college_wage <- set.edge.attribute(graph = college_wage,
	     		name = "description", index = 8:11, value = "U")

print(college_wage)
plot(college_wage)

ce <- causal.effect(y = "Y", x = "C", z = NULL, G = college_wage,
		    expr = TRUE, simp = TRUE, prune = TRUE)
# Pr(y | do(C)) is
get_latex_expr(ce)

