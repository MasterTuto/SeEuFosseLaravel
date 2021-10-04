module Questao2 where

-- importando o modulo Pilha
import Pilha

{-
- acao:       verificar o balanceamento dos parenteses numa String recebida.  
- entrada:    mensagem (String).  
- saida:      Bool (resultado da analise); 
- suposicoes: supoe-se que a expressao seja passada corretamente no input. 
- algoritmo:  vai avaliar a mensagem passada utilizando a funcao verificarParentesesAux e uma pilha. 
- Que passa a mensagem e a pilha vazia para avaliacao. 
-}
verificarParenteses :: String -> Bool
verificarParenteses entrada = verificarParentesesAux entrada pilhaVazia

{-
- acao:       verificar o balanceamento dos parenteses utilizando uma pilha vazia passada.  
- entrada:    mensagem (String), PilhaData Char (Pilha vazia para operacao).  
- saida:      Bool (resultado da analise); 
- suposicoes: supoe-se que a expressao seja passada corretamente no input e a pilha passada esteja vazia. 
- algoritmo:  Na linha 31, verificamos, se a String estah vazia, verificamos se a pilha estah vazia tambem.
- Se ela estiver vazia, eh porque estah balanceada, se nao, ela nao estah balanceada.
- Na linha 32 para baixo eh a avaliacao da mensagem. Se encontrar o CHAR '(', entao vai inserir o 'X' na pilhar. 
- Se encontrar o CHAR ')' e a pilha estiver vazia, significa que nao esta balanceado,
- Se encontrar o CHAR ')' e a pilha nao estiver vazia, entao vai retirar um 'X' da pilha.
- Se nao encontrar nenhuma das condicoes, entao continua retirando os simbolos da String.
- O processo vai se repetir ateh a String estiver vazia, para assim fazer a avaliacao do balanceamento.
-}
verificarParentesesAux :: String -> PilhaData Char -> Bool
verificarParentesesAux   []  pilha = isEmpty pilha
verificarParentesesAux (caractere:restante) pilha
    | caractere == '(' = verificarParentesesAux restante (push 'X' pilha)
    | caractere == ')' && (isEmpty pilha) = False
    | caractere == ')' && (not (isEmpty pilha)) = verificarParentesesAux restante (pop pilha)
    | otherwise = verificarParentesesAux restante pilha

-- main :: IO()
-- main = 
--     do
--         putStrLn "Digite uma sequencia:"
--         sequenciaDeCaracteres <- getLine
--         parentesesValidos <- verificarParenteses sequenciaDeCaracteres
--         if parentesesValidos then
--             putStrLn "Tudo correto!"
--         else
--             putStrLn "Deu errado pai kkk"
        