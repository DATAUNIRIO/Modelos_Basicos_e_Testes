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
  style A fill:#fff;
  style B fill:#fff;
  style C fill:#fff;  
  style D fill:#fff;
  style E fill:#8FD8D8;
  style F fill:#8FD8D8;
  style G fill:#8FD8D8;
  style H fill:#8FD8D8;
")

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

