module Pilha where

data PilhaData a = Pilha [a] deriving Show

{-
- acao:       usar como pilhar vazia. 
- entrada:    nenhuma; 
- saida:      Pilha []; (pilha vazia)
- suposicoes: nenhuma.
- algoritmo:  retorna uma Pilha vazia.
-}

pilhaVazia :: PilhaData a
pilhaVazia = Pilha []

{-
- acao:       verificar se a pilha esta vazia.  
- entrada:    PilhaData a; 
- saida:      Bool; (se a pilha esta vazia ou nao)
- suposicoes: supoe-se que a entrada seja uma Pilha. 
- algoritmo:  recebe PilhaData a e retorna se ela esta vazia.
-}

isEmpty :: PilhaData a -> Bool
isEmpty (Pilha [])  = True
isEmpty _ = False

{-
- acao:       retirar o ultimo elemento da pilha (primeiro da lista).  
- entrada:    PilhaData a; (Pilha sem alteracoes) 
- saida:      PilhaData a; (Pilha com a retirada do elemento do topo da pilha, que eh o primeiro elemento da lista)
- suposicoes: supoe-se que a entrada seja uma Pilha. 
- algoritmo:  recebe PilhaData a e retira o elemento do topo. Se a pilha estiver vazia, retorar ela mesma.
-}

pop :: PilhaData a -> PilhaData a
pop (Pilha [])    = Pilha []
pop (Pilha (cabeca:lista)) = Pilha lista

{-
- acao:       retornar o elemento do topo da pilha, (primeiro da lista).  
- entrada:    PilhaData a; (Pilha sem alteracoes) 
- saida:      a; (elemento do topo da pilha, que eh o primeiro elemento da lista)
- suposicoes: supoe-se que a entrada seja uma Pilha nao vazia. 
- algoritmo:  recebe PilhaData a e retorna o elemento do topo. Se a pilha estiver vazia, retorna uma mensagem de erro.
-}

top :: PilhaData a -> a
top (Pilha []) = error "Pilha vazia"
top (Pilha (cabeca:lista)) = cabeca

{-
- acao:       inserir um elemento no topo da pilha, (primeiro da lista).  
- entrada:    a (elemento da insercao),PilhaData a (Pilha sem alteracoes);  
- saida:      PilhaData a; (pilha com o elemento inserido)
- suposicoes: supoe-se que a entrada seja uma Pilha. 
- algoritmo:  recebe PilhaData a e o elemento de insercao. Retorna a pilha com o elemento inserido.
-}

push :: a -> PilhaData a -> PilhaData a
push elemento (Pilha lista) = Pilha (elemento:lista)