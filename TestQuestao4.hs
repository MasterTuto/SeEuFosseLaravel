module TestQuestao4 where

import DicionarioMain

main :: IO()
main =
    do
      let mensagem = "oi testando talvez oi abc ab ab tem ho wpoe bic baa depois conta side"
      writeFile ("caminho" ++ ".txt") mensagem                         
      writeFile ("contagem_" ++ "caminho" ++ ".txt") (imprimir mensagem)
    