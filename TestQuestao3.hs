module TestQuestao3 where

import TiposAgenda
import TestLib
import Agenda


main :: IO()
main =
    do
        --- Limpar arquivo ---
        writeFile "agenda.csv" ""

        -- criar contatos ---
        let contato1 = criarContato "08633069504" "Breno Carvalho" "11111111" "breno@gmail.com"
        let contato2 = criarContato "08633069504" "Jotinha Cabecao" "22222222" "jota@gmail.com"
        let contato3 = criarContato "41194324754" "Lucas Boquinha" "33333333" "lucas@gmail.com"
        let contato4 = criarContato "08633069505" "Gato Preto" "44444444" "gato@gmail.com"

        assertEqual contato1 (Contato "08633069504" "Breno Carvalho" "11111111" "breno@gmail.com") "criarContato with CPF 08633069504 should be valid"
        assertEqual contato2 (Contato "08633069504" "Jotinha Cabecao" "22222222" "jota@gmail.com") "criarContato with CPF 08633069504 should be valid"
        assertEqual contato3 (Contato "08633069504" "Lucas Boquinha" "33333333" "lucas@gmail.com") "criarContato with CPF 41194324754 should be valid"
        assertEqual contato4 Invalido "criarContato with CPF 08633069505 should be invalid"

        -- carregar agenda vazia --

        agendaCarregada <- carregarAgenda

        assertEqual agendaCarregada [] "loaded agenda should be empty"

        -- adicionarContato --
        let agendaModificada1 = adicionarContato contato1 agendaCarregada
        assertEqual agendaModificada1 (agendaCarregada++[contato1]) "adding valid contact should add contact"

        let agendaModificada2 = adicionarContato contato3 agendaModificada1

        let agendaModificada3 = adicionarContato contato4 agendaModificada1
        assertEqual agendaModificada3 agendaModificada1 "adding invvalid contact should NOT add contact"


        -- pesquisarContato --
        let posicaoContato1 = pesquisarContatoPorCpf "08633069504" agendaModificada2
        assertTrue (posicaoContato1 > -1) "searching valid contact should return position greater than -1"

        let posicaoContato2 = pesquisarContatoPorCpf "08633069505" agendaModificada2
        assertEqual posicaoContato2 (-1) "searching invalid contact should return -1"

        -- atualizarContato --
        let contato5 = criarContato "41194324754" "Oxe mankkk" "444 teto" "gato@gmail.com"
        
        let agendaModificada3 = alterarContatoPorCpf contato5 agendaModificada2 "41194324754"
        let posCtt = pesquisarContatoPorCpf "41194324754" agendaModificada3

        assertEqual (agendaModificada3 !! posCtt) contato5 "updating valid contact should be accepted"
        
        -- apagarContato --
        let agendaModificada4 = apagarContatoPorCpf "41194324754" agendaModificada3
        let posCtt = pesquisarContatoPorCpf "41194324754" agendaModificada4

        assertEqual posCtt (-1) "deleting valid contact should be accepted"

        -- salvarAgenda --
        salvarAgenda agendaModificada4
        agendaModificada5 <- carregarAgenda

        assertEqual agendaModificada5 agendaModificada4 "saving contacts should work"

