module TestLib(
    assertTrue,
    assertFalse,
    assertEqual,
    assertNotEqual
) where

{-
- acao:       avisar que eh True. Utilizando o showSucess e showFail.  
- entrada:    Bool, String (mensagem a ser mostrada).  
- saida:      tem tipo (), que é uma tupla vazia. 
- Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: que os parametros foram passados corretamente. 
- algoritmo:  se Bool = True, mostra uma menssagem de sucesso, no outro caso, uma mensagem de falha.
-}
assertTrue :: Bool -> String -> IO()
assertTrue True message  = showSuccess message
assertTrue False message = showFail message

{-
- acao:       avisar que eh False. Utilizando o showSucess e showFail.  
- entrada:    Bool, String (mensagem a ser mostrada).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: que os parametros foram passados corretamente.  
- algoritmo:  se Bool = False, mostra uma menssagem de sucesso, no outro caso, uma mensagem de falha.
-}
assertFalse :: Bool -> String -> IO()
assertFalse False message  = showSuccess message
assertFalse True message = showFail message

{-
- acao:       avisar que sao equivalentes. Utilizando o showSucess e showFail.  
- entrada:    a (primeiro valor), b (segundo valor) e message (mensagem a ser mostrada).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: que os parametros foram passados corretamente.  
- algoritmo:  se a == b, mostra uma menssagem de sucesso, no outro caso, uma mensagem de falha.
-}
assertEqual :: (Eq a) => a -> a -> String -> IO()
assertEqual a b message
    | a == b = showSuccess message
    | otherwise = showFail message

{-
- acao:       avisar que nao sao equivalentes. Utilizando o showSucess e showFail.  
- entrada:    a (primeiro valor), b (segundo valor) e message (mensagem a ser mostrada).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: que os parametros foram passados corretamente.  
- algoritmo:  se a for diferente de b, mostra uma menssagem de sucesso, no outro caso, uma mensagem de falha.
-}
assertNotEqual :: (Eq a) => a -> a -> String -> IO()
assertNotEqual a b message
    | a /= b = showSuccess message
    | otherwise = showFail message

{-
- acao:       imprimir a mensagem [PASS] concatenada com a mensagem recebida.  
- entrada:    message (mensagem para ser concatenada).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: que os parametros foram passados corretamente.  
- algoritmo:  concatena a mensagem recebida para output.
-}
showSuccess :: String -> IO()
showSuccess message = putStrLn ("[PASS] " ++ message)

{-
- acao:       imprimir a mensagem [FAIL] concatenada com a mensagem recebida.  
- entrada:    message (mensagem para ser concatenada).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: que os parametros foram passados corretamente.  
- algoritmo:  concatena a mensagem recebida para output.
-}
showFail :: String -> IO()
showFail message = putStrLn ("   [FAIL] " ++ message)