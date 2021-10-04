module FilaSR(FilaSR, add, removerPrimeiro, pegarPrimeiro, filaSRVazia, tamanho) where

data FilaSR a = Fila [a] deriving (Show, Eq)

{-
- acao:       adicionar um elemento na fila.  
- entrada:    FilaSR (fila onde vai inserir o elemento) e elemento (elemento passado como valor a).  
- saida:      FilaSR (fila passada com elemento inserido); 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  Typeclass Eq, onde valores devem ser membros de Eq, podendo fazer a comparacoes de equivalencia entre eles.
- Se o elemento jah pertence a lista, entao soh devolve a fila passada. Se nao, entao adiciona o elemento na fila.
- Foi utilizada a funcao: pertence para reconhecer o elemento.
-}
add :: Eq a => FilaSR a -> a -> FilaSR a
add (Fila fila) elemento
  | pertence (Fila fila) elemento = Fila fila -- se o elemento ja esta na fila
  | otherwise = Fila (fila ++ [elemento]) -- adiciona o elemnento nao esta na fila

{-
- acao:       verificar se o elemento estah na fila.  
- entrada:    FilaSR (fila onde vai verificar o elemento) e elemento (elemento passado como valor a).  
- saida:      Bool; 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  Typeclass Eq, onde valores devem ser membros de Eq, podendo fazer a comparacoes de equivalencia entre eles.
- Se a fila estiver vazia, entao o elemento nao pertence. 
- Se ela nao estiver vazia, vai analisar seus elementos. Se um elemento for igual, retorna true, se nao, continua analisando os elementos.
-}
pertence :: Eq a => FilaSR a -> a -> Bool
pertence (Fila [])    elemento = False
pertence (Fila (a:b)) elemento
  | a == elemento = True
  | otherwise = pertence (Fila b) elemento

{-
- acao:       remover o primeiro elemento da fila.  
- entrada:    FilaSR (fila onde vai eliminar o elemento).  
- saida:      FilaSR (fila com a modificacao). 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  Se a fila estiver vazia, entao uma mensagem error vai ser exibida, como nao eh possivel a remocao. 
- Se a fila houver elementos, entao remove o primeiro e retorna.
-}
removerPrimeiro :: FilaSR a -> FilaSR a
removerPrimeiro (Fila []) = error "Fila vazia"
removerPrimeiro (Fila (a:b)) = Fila b

{-
- acao:       retorna o primeiro elemento da fila.  
- entrada:    FilaSR.  
- saida:      a (primeiro elemento da fila). 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  Se a fila estiver vazia, entao uma mensagem error vai ser exibida, já que nao existe o primeiro elemento. 
- Se a fila houver elementos, entao retorna o primeiro da lista.
-}
pegarPrimeiro :: FilaSR a -> a
pegarPrimeiro (Fila []) = error "Fila vazia"
pegarPrimeiro (Fila (a:b)) = a

{-
- acao:       usar como fila vazia. 
- entrada:    nenhuma; 
- saida:      Fila []; (fila vazia)
- suposicoes: nenhuma.
- algoritmo:  retorna uma fila vazia.
-}
filaSRVazia :: FilaSR a
filaSRVazia = Fila []

{-
- acao:       retorna o tamanho da fila.  
- entrada:    FilaSR.  
- saida:      a (inteiro com length da lista). 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  pega a lista da fila e retorna seu length.
-}
tamanho :: FilaSR a -> Int 
tamanho (Fila fila) = length fila