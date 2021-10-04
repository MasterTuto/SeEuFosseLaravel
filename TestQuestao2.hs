module TestQuestao2 where

-- Importando os modulos para o teste
import Questao2
import TestLib

{-
- acao:       funcao de testes.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  utilizando o do, vai verificar a validade das expressoes passada, utilizando a funcao "verificarParenteses" do modulo "Questao2".
-}
main :: IO()
main =
    do
        assertTrue (verificarParenteses "") "\"\" should be valid"

        assertTrue (verificarParenteses "()") "\"()\" should be valid"

        assertTrue (verificarParenteses "(())()(())") "\"(())()(())\" should be valid"

        assertTrue (verificarParenteses "(eu) (ja(nem)) (sei qm) ()(sou)") "\"(eu) (ja(nem)) (sei qm) ()(sou)\" should be valid"

        ------------------------------------------------------------------
        assertFalse (verificarParenteses ")()(") "\")()(\" should be invalid"

        assertFalse (verificarParenteses "()(") "\"()(\" should be invalid"

        assertFalse (verificarParenteses ")()(") "\")()(\" should be invalid"

        assertFalse (verificarParenteses ")()()()(") "\")()()()(\" should be invalid"

