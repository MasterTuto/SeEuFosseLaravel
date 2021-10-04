module TiposAgenda where

type CPF = String
type Nome = String
type Telefone = String
type Email = String
type Dado = String
data ContatoData = Invalido | Contato CPF Nome Telefone Email deriving Show
type Agenda = [ContatoData]

{-
- acao:       recebe o ContatoData (com todas as informacoes) e retorna o CPF.  
- entrada:    ContatoData (possui todas as informacoes do contato);  
- saida:      CPF do contato;
- suposicoes: o contato eh valido. 
- algoritmo:  vai retornar a String no posicao cpf, os "_" sao usados como 'dont care'.
-}
cpfContato :: ContatoData -> CPF
cpfContato (Contato cpf _ _ _) = cpf

{-
- acao:       recebe o ContatoData (com todas as informacoes) e retorna o Nome.  
- entrada:    ContatoData (possui todas as informacoes do contato);  
- saida:      Nome do contato;
- suposicoes: o contato eh valido. 
- algoritmo:  vai retornar a String no posicao nome, os "_" sao usados como 'dont care'.
-}
nomeContato :: ContatoData -> Nome
nomeContato (Contato _ nome _ _) = nome

{-
- acao:       recebe o ContatoData (com todas as informacoes) e retorna o Telefone.  
- entrada:    ContatoData (possui todas as informacoes do contato);  
- saida:      Telefone do contato;
- suposicoes: o contato eh valido. 
- algoritmo:  vai retornar a String no posicao telefone, os "_" sao usados como 'dont care'.
-}
telefoneContato :: ContatoData -> Telefone
telefoneContato (Contato _ _ telefone _) = telefone

{-
- acao:       recebe o ContatoData (com todas as informacoes) e retorna o Email.  
- entrada:    ContatoData (possui todas as informacoes do contato);  
- saida:      Email do contato;
- suposicoes: o contato eh valido. 
- algoritmo:  vai retornar a String no posicao email, os "_" sao usados como 'dont care'.
-}
emailContato :: ContatoData -> Email
emailContato (Contato _ _ _ email) = email