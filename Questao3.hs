module Questao3 where

-- Os importes utilizados para o menu
import Agenda
import Utils
import TiposAgenda

{-
- acao:       funcao de testes.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  utilizando o do, vai carregar a agenda e chamar a funcao loopPrincipal para apresentar o menu.
-}
main :: IO()
main =
    do
        agenda <- carregarAgenda
        loopPrincipal agenda

{-
- acao:       exibir o menu como um loop ate o loop for encerrado na escolha. 
- entrada:    Agenda (Lista de contatos que foi carregado para o menu).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: supoe-se que os dados de entrada estejam de acordo com o que se pede. 
- algoritmo:  vai exibir o menu com a ajuda a funcao mostrarMenu e realizar as devidas acoes escolhidas nos cases, por meio das funcoes dos modulos. 
- Volta para o loop principal depois de escolher uma opcao, fora a opcao de "Sair" - "0."
-}
loopPrincipal :: Agenda -> IO()
loopPrincipal agenda =
    do
        mostrarMenu
        opcao <- getLine
        case opcao of
            "1" -> do
                agendaAlterada <- cadastrarContato agenda
                salvarAgenda agendaAlterada
                loopPrincipal agendaAlterada
            "2" -> do
                listarContatos agenda
                loopPrincipal agenda
            "3" -> do
                buscarContato agenda
                loopPrincipal agenda
            "4" -> do
                agendaAlterada <- atualizarContato agenda
                salvarAgenda agendaAlterada
                loopPrincipal agendaAlterada
            "5" -> do
                agendaAlterada <- apagarContato agenda
                salvarAgenda agendaAlterada
                loopPrincipal agendaAlterada
            "0" -> return ()
            _ -> loopPrincipal agenda --(mostrarErro agenda)

{-
- acao:       mostrar as Strings do menu.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  utilizando o do, vai usar o putStrLn para imprimir o menu.
-}
mostrarMenu :: IO()
mostrarMenu =
    do
        putStrLn "\n******* MENU *******\n"
        putStrLn "\t[1] Cadastrar contato"
        putStrLn "\t[2] Mostrar Contatos"
        putStrLn "\t[3] Procurar Contato"
        putStrLn "\t[4] Atualizar Contato"
        putStrLn "\t[5] Apagar contato"
        putStrLn "\t[0] Sair"
        putStr "Digite o numero desejado: "

{-
- acao:       uma forma otimizada para receber uma entrada. 
- entrada:    mensagem (String).  
- saida:      IO String (String recebida). 
- suposicoes: nenhuma. 
- algoritmo:  recebe uma mensagem com o requerimento da entrada (usando getLine para obter);
-}
receberEntrada :: String -> IO String
receberEntrada texto =
    do
        putStrLn texto
        getLine

{-
- acao:       cadastrar o contato. 
- entrada:    Agenda (agenda para o cadastro do contato).  
- saida:      IO Agenda (Agenda com o cadastro feito). 
- suposicoes: os dados de entrada estejam corretos. 
- algoritmo:  vai receber os dados do contato utilizando a funcao receberEntrada.
- Se o CPF for valido, entao o cadastro eh feito. Se nao, entao uma mensagem eh mostrada.
-}
cadastrarContato :: Agenda -> IO Agenda
cadastrarContato agenda =
    do
        cpf <- receberEntrada "Digite seu CPF: "
        nome <- receberEntrada "Digite seu nome: "
        telefone <- receberEntrada "Digite seu telefone: "
        email <- receberEntrada "Digite seu e-mail: "

        if validarCpf cpf then
            do
                let agendaAlterada = adicionarContato (criarContato cpf nome telefone email) agenda
                salvarAgenda agendaAlterada
                return agendaAlterada
        else
            do
                putStr "\n======= Cpf invalido! Inscricao indeferida ======\n"
                return agenda

{-
- acao:       buscar contato. 
- entrada:    Agenda (agenda para a busca do contato).  
- saida:      acao de IO() (input/output).  
- suposicoes: os dados de entrada estejam corretos. 
- algoritmo:  vai receber uma pergunta, se deseja realizar a busca por nome ou contato utilizando as funcoes do modulo Agenda.
- Por meio da funcao mostrarContato, vai exibir o contato buscado.
-}
buscarContato :: Agenda -> IO()
buscarContato agenda =
    do
        opcao <- receberEntrada "\n[0] Pesquisar por nome\n[1] Pesquisar por CPF\nDigite sua opcao: "
        case opcao of
            "0" -> do
                nome <- receberEntrada "Digite o nome: "
                putStrLn (mostrarContato (agenda !! pesquisarContatoPorNome nome agenda) (-1))
            "1" -> do
                cpf <- receberEntrada "Digite o CPF: "
                putStrLn (mostrarContato (agenda !! pesquisarContatoPorCpf cpf agenda) (-1))

{-
- acao:       mudanca dos dados de um contato. 
- entrada:    ContatoData (Contato que vai ser alterado).  
- saida:      IO ContatoData (Contato alterado). 
- suposicoes: os dados de entrada estejam corretos. 
- algoritmo:  Vai perguntar a opcao do que deseja mudar no contato e depois receber a mudanca, salvando ela no contato.
-}
pedirPraMudarContato :: ContatoData -> IO ContatoData
pedirPraMudarContato (Contato cpf nome telefone email) = 
    do
        putStrLn "O que voce pode mudar:"
        putStrLn "\t[1] Nome"
        putStrLn "\t[2] Telefone"
        putStrLn "\t[3] Email"
        opcao <- receberEntrada "Digite a opcao desejada: "
        case opcao of
            "1" -> do
                novoNome <- receberEntrada "Digite o novo nome: "
                let contato = Contato cpf novoNome telefone email
                putStrLn (mostrarContato contato (-1))
                pedirPraMudarContato contato
            "2" -> do
                novoTelefone <- receberEntrada "Digite o novo Telefone: "
                let contato = Contato cpf novoTelefone telefone email
                putStrLn (mostrarContato contato (-1))
                pedirPraMudarContato contato
            "3" -> do
                novoEmail <- receberEntrada "Digite o novo Email: "
                let contato = Contato cpf novoEmail telefone email
                putStrLn (mostrarContato contato (-1))
                pedirPraMudarContato contato
            _ -> do
                putStrLn "Entrada invalida!"
                pedirPraMudarContato (Contato cpf nome telefone email)
            
{-
- acao:       atualizar o contato da agenda. 
- entrada:    Agenda(Lista de contatos).  
- saida:      IO Agenda(Com o contato atualizado). 
- suposicoes: os dados de entrada estejam corretos. 
- algoritmo:  Vai ser pedido por onde deseja alterar, se encontrar (existir), e vai atualizar.
-}
atualizarContato :: Agenda -> IO Agenda
atualizarContato agenda =
    do
        putStrLn "\n========--  ALTERAR CONTATO --========="
        putStrLn "==== Listagem de contatos ===="
        listarContatos agenda
        putStrLn "Opcoes disponiveis:"
        putStrLn "\t[0] Alterar por nome"
        putStrLn "\t[1] Alterar por CPF"
        putStrLn "\t[2] Alterar por posicao"
        opcao <- receberEntrada "Digite sua opcao: "
        case opcao of
            "0" -> do
                nome <- receberEntrada "\nDigite seu nome: "
                let contato = pesquisarContatoPorNome nome agenda
                if contato == -1 then
                    do
                        putStrLn "\n===== CONTATO NAO ENCONTRADO ==== \n"
                        return agenda
                else
                    do
                        putStrLn (mostrarContato (agenda !! contato) (-1))
                        novoContato <- pedirPraMudarContato (agenda !! contato)
                        return (alterarContatoPorNome novoContato agenda nome)
            "1" -> do
                cpf <- receberEntrada "\nDigite seu CPF: "
                let contato = pesquisarContatoPorCpf cpf agenda
                if contato == -1 then
                    do
                        putStrLn "\n===== CONTATO NAO ENCONTRADO ==== \n"
                        return agenda
                else
                    do
                        putStrLn (mostrarContato (agenda !! contato) (-1))
                        novoContato <- pedirPraMudarContato (agenda !! contato)
                        return (alterarContatoPorNome novoContato agenda cpf)
            "2" -> do
                posicao <- receberEntrada "\nDigite a posicao: "
                let posicaoInt = read posicao
                if (posicaoInt >= length agenda) || (posicaoInt < 0) then
                    do
                        putStrLn "\n===== CONTATO NAO ENCONTRADO ==== \n"
                        return agenda
                else
                    do
                        putStrLn (mostrarContato (agenda !! posicaoInt) (-1))
                        novoContato <- pedirPraMudarContato (agenda !! posicaoInt)
                        return (alterarContatoPorPosicao novoContato agenda posicaoInt)
            _ -> return agenda

{-
- acao:       apagar o contato da agenda. 
- entrada:    Agenda(Lista de contatos).  
- saida:      IO Agenda(Com o contato apagado). 
- suposicoes: os dados de entrada estejam corretos. 
- algoritmo:  Vai ser pedido por onde deseja apagar, se encontrar (existir), e vai apagar o contato.
-}
apagarContato :: Agenda -> IO Agenda
apagarContato agenda =
    do
        putStrLn "\n========--  APAGAR CONTATO --========="
        putStrLn "==== Listagem de contatos ===="
        listarContatos agenda
        putStrLn "Opcoes disponiveis:"
        putStrLn "\t[0] Apagar por nome"
        putStrLn "\t[1] Apagar por CPF"
        putStrLn "\t[2] Apagar por posicao"
        opcao <- receberEntrada "Digite sua opcao: "
        case opcao of
            "0" -> do
                nome <- receberEntrada "\nDigite o nome: "
                return (apagarContatoPorNome nome agenda)
            "1" -> do
                cpf <- receberEntrada "\nDigite o CPF: "
                return (apagarContatoPorCpf cpf agenda)
            "2" -> do
                posicao <- receberEntrada "\nDigite a posicao: "
                return (apagarContatoPorPosicao (read posicao) agenda)
            _ -> return agenda





