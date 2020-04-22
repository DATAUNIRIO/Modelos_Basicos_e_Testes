#----------------------------------------------------------------
#
#   Teste para duas variáveis qualitativas 
#   Prof. Steven Ross
#   DMQ/UNIRIO
#
#----------------------------------------------------------------

library(DiagrammeR)

#----------------------------------------------------------
# Associação entre duas variáveis qualitativas 
#----------------------------------------------------------


mermaid("
graph TB
  A[Os pressupostos do teste Qui-Quadrado foi atendido?]
  B[Nenhuma célula deve ter valor esperado menor que cinco.]
  C[Todas são acima de cinco]
  D[Tem pelo menos uma abaixo de cinco]
  E[Teste Qui-Quadrado <br> chisq.test]
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
  style A fill:#00c48d;
  style B fill:#00c48d;
  style C fill:#00c48d;  
  style D fill:#00c48d;
  style E fill:#fffac2;
  style F fill:#fffac2;
  style G fill:#fffac2;
  style H fill:#fffac2;
")


