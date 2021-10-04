module TestQuestao4 where

import DicionarioMain

main :: IO()
main =
    do
      writeFile ("caminho" ++ ".txt") "mensagem de dicionario"                          
      writeFile ("contagem_" ++ "caminho" ++ ".txt") (imprimir "mensagem de dicionario" ) 