library(DiagrammeR)

# B. W. Yap & C. H. Sim (2011) Comparisons of various types of normality tests, Journal of Statistical Computation and Simulation, 81:12, 2141-2155, DOI: 10.1080/00949655.2010.520163 
# Shapiro-Wilk will almost always reject on large sample sizes. https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless

mermaid("
graph LR
  A[Qual teste usar?]
  B(A distribuição dos dados é normal? <br> Teste de normalidade. <br> Teste de Shapiro Wilk.)
  C((Sim))
  D((Não))
  E((Pearson  .))
  F((Spearman))
  A-->B
  B-->C
  B-->D
  C-->E
  D-->F
  style A fill:#fff;
  style B fill:#fff;
  style C fill:#008080
  style D fill:#008080;  
  style E fill:#8FD8D8;
  style F fill:#8FD8D8;
        ")

#  Name: Sunset Orange Hex: #ff555e
#  Name: Coral Hex: #ff8650
#  Name: Yellow (Crayola) Hex: #ffe981
#  Name: Light Green Hex: #8bf18b
#  Name: French Sky Blue Hex: #83b2ff
#  Name: Violets Are Blue Hex: #9b6ef3
  

#----------------------------------------------------------

# Associação entre duas variáveis qualitativas 

#----------------------------------------------------------


mermaid("
graph TB
  A[Os pressupostos do teste Qui-Quadrado foi atendido?]
  B[Nenhuma célula deve ter valor esperado menor que cinco.]
  C[Todas são acima de cinco]
  D[Tem pelo menos uma abaixo de cinco]
  E[Teste Qui-Quadrado]
  F[Teste Exato de Fisher]
  G[Tabela 2 X 2 <br> fisher.test]
  H[Mais de duas categorias  <br>  em uma variável <br>  fisher.test, hybrid=TRUE']
  A-->B
  B-->C
  B-->D
  C-->E
  D-->F
  F-->G
  F-->H
")



M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))

library("gplots")
balloonplot(t(M), main ="Exemplo do Agresti", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

Xsq <- chisq.test(M)  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
