module TestQuestao5 where

import Formula
import InterfaceFormula

main :: IO()
main =
    do
      let formulaN = (read "(Or(Var "A") (And (Var "A") (Var "B"))))" :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaN = (read "(Or(Var "A") (Not (Var"A")))" :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

      let formulaN = (read "(Or(Var "A") (Not (Var"B")))" :: Formula)
      tabela <- montarTabelaCompleta formulaN
      tabelaString <- imprimirTabela tabela
      putStrLn tabelaString
      ehTautologia <- tautologia tabela
      putStrLn ehTautologia

