module Questao1 where

-- Importando o modulo Pilha 
import Pilha

{-
- acao:       fazer as operacoes de soma, subtracao, multiplicacao e divisao.  
- entrada:    operando1 (Double), operando2 (Double) e operador (Char que representa o operador).  
- saida:      resultado (valor resultante, Double); 
- suposicoes: supoe-se que o operador seja passado corretamente como CHAR. 
- algoritmo:  vai fazer uma operacao simples, recebendo dois operandos e um operador.
-}

operar :: Double -> Double -> Char -> Double
operar operando1 operando2 '+' = operando1 + operando2
operar operando1 operando2 '-' = operando1 - operando2
operar operando1 operando2 '*' = operando1 * operando2
operar operando1 operando2 '/' = operando1 / operando2

{-
- acao:       verificar se o parametro eh operador.  
- entrada:    operador (Char que representa o operador).  
- saida:      Bool (se eh ou nao operador); 
- suposicoes: supoe-se que o operador seja passado corretamente como CHAR. 
- algoritmo:  vai avaliar o CHAR com as possibilidades. Se nao for um operador, vai retornar FALSE.
-}

ehOperador :: Char -> Bool
ehOperador '+' = True
ehOperador '-' = True
ehOperador '*' = True
ehOperador '/' = True
ehOperador  _  = False

{-
- acao:       verificar se o parametro eh um digito.  
- entrada:    operador (Char que representa o digito).  
- saida:      Bool (se eh ou nao um digito); 
- suposicoes: supoe-se que o digito seja passado corretamente como CHAR. 
- algoritmo:  vai avaliar se CHAR passado eh um digito. 
- Utiliza o "isDigit", funcao que retorna True se o decimal numerico esta entre (0..9).
-}

ehDigito :: Char -> Bool
ehDigito simbolo = isDigit simbolo || simbolo == '.'

{-
- acao:       fazer a avaliacao da expressao.  
- entrada:    expressao (String).  
- saida:      IO Double (resultado da expressao); 
- suposicoes: supoe-se que a expressao seja passada corretamente no input. 
- algoritmo:  vai avaliar a expressao passada utilizando a funcao avaliarAux. 
- Que passa a expressao, a pilha vazia para resolver a expressao e a String. 
-}
avaliar :: String -> IO Double
avaliar expressao = avaliarAux expressao pilhaVazia ""

{-
- acao:       avalia a expressao para retornar um valor.  
- entrada:    expressao (String), PilhaData Double (pilha para realizar as operacoes), String (acumulador).  
- saida:      IO Double (resultado da expressao); 
- suposicoes: supoe-se que a expressao seja passada corretamente no input. 
- algoritmo:  Na linha 66 vai avaliar se a nao foi passada uma expressao, retornando a mensagem error. 
- Na linha 69 se a expressao estiver vazia e a pilha nao, entao ele vai retornar o elemento do topo da pilha com o resultado.
- Da linha 70 para baixo eh o funcionamento da pilha na resolucao da expressao. 
- Se ele for Digito, entao ele acumula na String acumuladora, que vai segurar o numero para colocar na pilha quando o proximo simbolo for igual a espaco, limpando o acumulador quando colocar na pilha.
- Se ele for Operador, entao ele vai colocar na pilha o resultado das expressoes acumuladas na pilha (dirOp e esqOP), limpando a pilha e o acumulador tbm.
- Se nao identificar nenhum dos padroes para fazer a operacao, ele ignora o simbolo e continua a expressao (linha 78).
- O processo se repete ateh sobrar apenas o resultado na pilha, retornando o valor da expressao.
-}

avaliarAux :: String -> PilhaData Double -> String -> IO Double
avaliarAux "" (Pilha []) _ = error "Expressao vazia"
avaliarAux "" pilha _ = return (top pilha)
avaliarAux (simbolo:expressao) pilha numero
  | ehOperador simbolo = avaliarAux expressao (push (operar esqOp dirOp simbolo) (pop (pop pilha))) ""
  | ehDigito simbolo = avaliarAux expressao pilha (numero++[simbolo])
  | simbolo == ' ' && (numero /= "") = avaliarAux expressao (push (read numero::Double) pilha) ""
  | otherwise = avaliarAux expressao pilha ""
    where
      dirOp = top pilha
      esqOp = top (pop pilha)


-- main :: IO()
-- main =
--     do
--         putStrLn "Digite uma expressao pos-fixa (+,-,*,/):"
--         expressao <- getLine
--         resultadoExp <- avaliar expressao
--         putStrLn ("Resultado da expressao: " ++ (show resultadoExp))
