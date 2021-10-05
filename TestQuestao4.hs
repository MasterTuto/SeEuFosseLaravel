module TestQuestao4 where

import DicionarioMain

{-
- acao:       funcao de testes.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  passa uma mensagem qualquer pro arquivo com o path "caminho". Ao abrir o arquivo a contagem vai estar correta.
-}
main :: IO()
main =
    do
      let mensagem = "oi testando talvez oi abc ab ab tem ho wpoe bic baa depois conta side"
      writeFile ("caminho" ++ ".txt") mensagem
      writeFile ("contagem_" ++ "caminho" ++ ".txt") (imprimir mensagem)
