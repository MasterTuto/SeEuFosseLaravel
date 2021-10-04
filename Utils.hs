module Utils (
    validarCpf,
    ehDigito
) where

-- Importando os tipos utilizados
import TiposAgenda

{-
- acao:       verificar se o parametro eh um digito.  
- entrada:    operador (Char que representa o digito).  
- saida:      Bool (se eh ou nao um digito); 
- suposicoes: supoe-se que o digito seja passado corretamente como CHAR. 
- algoritmo:  vai avaliar se CHAR passado eh um digito. 
-}
ehDigito :: Char -> Bool
ehDigito simbolo = (simbolo >= '0' && simbolo <= '9')

{-
- acao:       verificar se repete simbolo.  
- entrada:    String (vai ser avaliada).  
- saida:      Bool; 
- suposicoes: supoe-se que os dados passados estao corretos. 
- algoritmo:  vai avaliar se repete o simbolo. Utiliza a funcao naoRepeteSimboloAux.
-}
naoRepeteSimbolo :: String -> Bool
naoRepeteSimbolo [] = True
naoRepeteSimbolo (simbolo:str) = naoRepeteSimboloAux str simbolo

{-
- acao:       verificar se os simbolos sao diferentes, se nao for, continua a iteracao.  
- entrada:    String (vai ser avaliada), simbolo (Char que vai ver se repete).  
- saida:      Bool; 
- suposicoes: supoe-se que os dados passados estao corretos.
- algoritmo:  vai avaliar se repete o simbolo. Se ele for igual, continua a iteracao ate encontrar um caracter diferente.
-}
naoRepeteSimboloAux :: String -> Char -> Bool
naoRepeteSimboloAux [] _ = False
naoRepeteSimboloAux (simbAtual:str) simbolo
    | simbAtual /= simbolo = True
    | simbAtual == simbolo = naoRepeteSimboloAux str simbolo


{-
- acao:       faz a validacao do cpf utilizando a funcao validarCpfAux e naoRepeteSimbolo.  
- entrada:    CPF.  
- saida:      Bool; 
- suposicoes: nenhuma.
- algoritmo:  vai avaliar se o cpf esta corrento, verificando seus digitos e utilizando a funcao validarCpfAux.
- cpfSoNumeros esta passando o CPF apenas com digitos, utilizando o filter.
-}
validarCpf :: CPF -> Bool
validarCpf cpf = (validarCpfAux cpfSoNumeros 10 0 0) && (naoRepeteSimbolo cpfSoNumeros)
    where
        cpfSoNumeros = (filter ehDigito cpf)

{-
- acao:       faz a validacao do cpf fazendo a verificacao dos digitos.  
- entrada:    CPF, divisor (Int), acumulador1 (Int), acumulador2 (Int).  
- saida:      Bool; 
- suposicoes: as entradas estejam corretas.
- algoritmo:  Na linha 69 vai avaliar se o CPF nao possuir mais elementos, entao ele retorna false, jah que nao entrou nos outros cases.
- Na linha 70 para baixo, vamos fazer as outras verificacoes. Se houver dois digitos no CPF (os digitos verificadores), 
- vamos verificar se eles sao iguais aos  respectivos acumuladores (que passou pelo processo). Se forem iguais, entao o CPF eh valido, se nao, o CPF eh invalido.
- Se nao houver dois elementos no CPF, entao vamos alterar os acumuladores de acordo com o digito atual, passar para o proximo digitor e diminuir o divisor. 
- Continuando o processo ateh o divisor for = a 1 e houver dois elementos, como explicado na linha 70.
-}
validarCpfAux :: CPF -> Int -> Int -> Int -> Bool
validarCpfAux [] _ _ _ = False
validarCpfAux [a, b] 1 acc1 acc2 = verificacaoDigito1 && verificacaoDigito2
    where
            aInt = (read [a])::Int
            bInt = (read [b])::Int
            verificacaoDigito1 = ((acc1)*10 `mod` 11) == aInt
            verificacaoDigito2 = ((acc2 + (aInt*2))*10 `mod` 11) == bInt
validarCpfAux (digito:cpf) divisor acc1 acc2 = validarCpfAux cpf (divisor-1) acumulador1 acumulador2
    where
        digitoInt = read [digito]::Int
        acumulador1 = (acc1 + (divisor*digitoInt))
        acumulador2 = (acc2 + ((divisor+1)*digitoInt))