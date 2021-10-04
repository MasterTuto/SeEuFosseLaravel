module TestQuestao5 where

import Formula
import InterfaceFormula

{-
- acao:       funcao de testes.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  passa uma formula jah feita para o montarTabelaCompleta e verifica se a mesma eh tautologia.
-}
teste :: IO()
teste =
    do
      let formulaString = "Or (Var \"A\") (And (Var \"A\") (Var \"B\"))"
      let formulaN = (read formulaString :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaString = "Or (Var \"A\") (And (Var \"B\") (Var \"C\"))"
      let formulaN = (read formulaString :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaString = "Or (Var \"A\") (Not (Var\"A\"))"
      let formulaN = (read formulaString :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaString = "Or (Var \"A\") (Not (Var\"B\"))"
      let formulaN = (read formulaString :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaString = "Or (Var \"A\") (Not (And (Var \"B\") (Var \"C\")))"
      let formulaN = (read formulaString :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaString = "Or (Var \"A\") (Not (And (Var \"B\") (Or (Var \"C\") (Var \"D\") )))"
      let formulaN = (read formulaString :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

