module TestQuestao5 where

import Formula
import InterfaceFormula

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

