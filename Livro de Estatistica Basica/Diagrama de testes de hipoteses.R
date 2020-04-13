library(DiagrammeR)

# B. W. Yap & C. H. Sim (2011) Comparisons of various types of normality tests, Journal of Statistical Computation and Simulation, 81:12, 2141-2155, DOI: 10.1080/00949655.2010.520163 
# Shapiro-Wilk will almost always reject on large sample sizes. https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless

mermaid("
graph TB
  A[Qual teste usar?]
  B(A distribuição dos dados é normal? <br> Teste de normalidade. <br> Teste de Shapiro Wilk.)
  C((Sim))
  D((Não))
  E(Os dados têm igualdade de variância? <br> Teste de Bartlett.)
  F((Sim))
  G((Não))
  H[ANOVA]
  H2[Teste t]
  H3[Teste de Comparações Múltiplas <br> Teste de Tukey/Teste de Scheffe]
  I[Teste de Welch]
  I2[Teste t de Welch]
  J(A variável qualitativa tem quantas categorias?)
  K(Duas categorias)
  L(Três ou mais categorias)
  M[Teste de Wilcoxon]
  N[Teste de Kruskal-Wallis]
  N2[Teste de Comparações Múltiplas de Wilcoxon]
  A-->B
  B-->C
  B-->D
  C-->E
  E-->F
  E-->G
  F-->H
  F-->H2
  H-->H3  
  G-->I
  I-->H3
  G-->I2  
  D-->J
  J-->K
  J-->L
  K-->M
  L-->N
  N-->N2
  style A fill:#fff;
  style B fill:#fdf6e3;
  style C fill:#ffe981;
  style D fill:#ffe981;  
  style E fill:#f8fbff;
  style F fill:#eaf4ff;
  style G fill:#eaf4ff;
  style H fill:#add6ff;
  style H2 fill:#add6ff;
  style H3 fill:#84c1ff;
  style I fill:#83b2ff;
  style I2 fill:#83b2ff;
  style J fill:#fff6e9;
  style K fill:#ffd3b6;
  style L fill:#ffd3b6;
  style M fill:#ffaaa5;
  style N fill:#ffaaa5;
  style N2 fill:#ff8b94;
        ")

#  Name: Sunset Orange Hex: #ff555e
#  Name: Coral Hex: #ff8650
#  Name: Yellow (Crayola) Hex: #ffe981
#  Name: Light Green Hex: #8bf18b
#  Name: French Sky Blue Hex: #83b2ff
#  Name: Violets Are Blue Hex: #9b6ef3
  

#----------------------------------------------------------




#----------------------------------------------------------

t.test(extra ~ group, data = sleep)
t.test(extra ~ group, data = sleep,var.equal=FALSE)

data("iris")
pc<-aov(Sepal.Length ~ Species, data = iris)
tukey.test <- TukeyHSD(pc)
tukey.test


### Finally, you might want to compare at the 75th percentile of income for the two towns.  This could be done using quantile regression.
### quantile regression considering the 75th percentile

library(quantreg)
TwoTowns = read.table("http://rcompanion.org/documents/TwoTowns.csv",
                      header=TRUE, sep=",")
model.q = rq(Income ~ Town,
             data = TwoTowns,
             tau = 0.75)

model.null = rq(Income ~ 1,
                data = TwoTowns,
                tau = 0.75)

anova(model.q, model.null)





















library(onewaytests)
out <- welch.test(Sepal.Length ~ Species, data = iris)
paircomp(out)





