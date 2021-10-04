{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ABB (ABB, insert, search, emptyTree, printTree, searchValue) where
--                  arvEsquerda valor arvDireita 
data ABB a = Vazio | Node (ABB a) a (ABB a) deriving (Show, Eq, Ord)

{-
- acao:       inserir um elemento na arvore.  
- entrada:    Node (arvore passada sem o elemento inserido) e elemento (elemento passado como valor a).  
- saida:      Node (arvore passada com o elemento inserido); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente, e seja uma arvore binaria. 
- algoritmo:  Typeclass Ord, onde valores devem ser membros de Ord, podendo fazer a comparacoes entre eles.
- Na linha 20, o elemento virou uma folha, sem Node direito nem esquerdo.
- Na linha 21 para baixo eh onde comeca o processo de comparacao. 
- Se o valor do nodulo atual (do que esta por conta da busca) for igual ao do elemento, entao ele retorna o Node encontrado.
- Se o valor do nodulo atual (do que esta por conta da busca) for maior do que elemento, entao ele vai continuar a busca pelo nodulo esquerdo.
- Se o valor do nodulo atual (do que esta por conta da busca) for menor do que elemento, entao ele vai continuar a busca pelo nodulo direito.
- O processo eh feito ateh encontrar o lugar de insercao do Node. Nao importa qual ira adicionar em caso geral, mas para o problema em questao importa.
-}
insert :: Ord a => ABB a -> a -> ABB a
insert Vazio element = Node Vazio element Vazio
insert (Node left value right) element
  | value == element = Node left element right -- nao importa qual ira adicionar em caso geral, mas para o provlema em questao importa
  | value > element  = Node (insert left element) value right
  | value < element  = Node left value (insert right element)

{-
- acao:       buscar um elemento na arvore.  
- entrada:    Node (arvore - Node de busca inicial) e elemento (elemento passado como valor a).  
- saida:      Bool (se foi encontrado); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente, e seja uma arvore binaria.. 
- algoritmo:  Typeclass Ord, onde valores devem ser membros de Ord, podendo fazer a comparacoes entre eles.
- Na linha 40, o elemento nao foi encontrado, jah que chegou num dos extremos de uma folha. Retorna False.
- Na linha 41 para baixo eh onde comeca o processo de comparacao na busca. 
- Se o valor do nodulo atual (do que esta por conta da busca) for igual ao do elemento, entao ele retorna True. O Node foi encontrado.
- Se o valor do nodulo atual (do que esta por conta da busca) for maior do que elemento, entao ele vai continuar a busca pelo nodulo esquerdo. Retorna False.
- Se o valor do nodulo atual (do que esta por conta da busca) for menor do que elemento, entao ele vai continuar a busca pelo nodulo direito. Retorna False.
- O processo eh feito ateh encontrar o Node. 
-}
search :: Ord a => ABB a -> a -> Bool
search Vazio element = False
search (Node left value right) element
  | value == element = True
  | value > element  = search left element
  | value < element  = search right element

{-
- acao:       procurar o valor de um Node.  
- entrada:    Node (arvore - Node de busca inicial) e elemento (elemento passado como valor a).  
- saida:      valor; 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente, e seja uma arvore binaria.. 
- algoritmo:  Typeclass Ord, onde valores devem ser membros de Ord, podendo fazer a comparacoes entre eles.
- Na linha --, o elemento nao foi encontrado, jah que chegou num dos extremos de uma folha. Retorna mensagem de erro.
- Na linha -- para baixo eh onde comeca o processo de comparacao na busca. 
- Se o valor do nodulo atual (do que esta por conta da busca) for igual ao do elemento, o valor do Node foi encontrado.
- Se o valor do nodulo atual (do que esta por conta da busca) for maior do que elemento, entao ele vai continuar a busca pelo nodulo esquerdo.
- Se o valor do nodulo atual (do que esta por conta da busca) for menor do que elemento, entao ele vai continuar a busca pelo nodulo direito.
- O processo eh feito ateh encontrar o valor. 
-}
searchValue :: Ord a => ABB a -> a -> a
searchValue Vazio element = error "Value not found"
searchValue (Node left value right) element
  | value == element = value -- nao importa qual ira retornar em caso geral, mas para o problema em questao importa
  | value > element  = searchValue left element
  | value < element  = searchValue right element

{-
- acao:       usar como arvore (Node inicial) vazia. 
- entrada:    nenhuma.
- saida:      ABB a (arvore vazia);
- suposicoes: nenhuma.
- algoritmo:  retorna uma arvore vazia.
-}
emptyTree :: ABB a
emptyTree = Vazio

{-
- acao:       imprimir a arvore.  
- entrada:    Node (arvore - Node de busca inicial).  
- saida:      String (com todos os nodulos como String); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente, e seja uma arvore binaria.. 
- algoritmo:  Typeclass Show, onde valores devem ser membros de Show.
- Se o elemento nao foi encontrado, jah que chegou num dos extremos de uma folha. Entao eh o fim da concatenacao.
- Se o Node nao for vazio, entao concatena as informacoes desse node. Faz o processo ateh chegar num node vazio. 
-}
printTree :: Show a => ABB a -> String
printTree Vazio = ""
printTree (Node left value right) = (show value) ++ "\n" ++ (printTree left) ++ (printTree right)

-- Arvore de inteiros para testes
{-
- acao:       usar como arvore (Node inicial) de Inteiros para teste. 
- entrada:    nenhuma.
- saida:      ABB a (arvore de inteiros;
- suposicoes: nenhuma.
- algoritmo:  retorna uma arvore de inteiros.
-}
teste :: ABB Int
teste = Node (Node (Node Vazio 2 Vazio) 5 Vazio) 10 (Node Vazio 20 Vazio)