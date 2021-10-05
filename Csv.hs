module Csv (
    Caminho,
    Csv,
    Linha,
    CsvObj,

    carregarArquivo,
    escreverEmArquivo,
) where

type Caminho = String
type Csv = String
type Linha = [String]
type CsvObj = [Linha]

{-
- acao:       vai interpretar o conteudo passado e vai retornar um objetov Csv.  
- entrada:    conteudo (String), acumulador (String), linha (Lista de String), csvObjt (Objeto Csv).  
- saida:      csvObj (executa entrada/saída (IO) e retorna um valor, valor CsvObj);
- suposicoes: todos os dados passados estao corretos e o conteudo nao estah vazio. 
- algoritmo:  Na linha 29 ele verifica se todo conteudo foi lido, se foi, ele concatena tudo que foi interpretado para retornar.
- Na linha 30 para baixo comeca a interpretacao. Se o simbolo lido nao for ',' ou '\n', entao ele continua armazenando o conteudo lindo no acumulador.
- Se o simbolo lido for ',', entao ele armazena o dado lido do acumulador na linha, zerando o acumulador e continuando assim ateh pular a linha.
- Se o simbolo lido for '\n', entao ele armazena os dados armazenados na linha ate agora no objcsv, que vai continuar assim ateh acabar o conteudo e for retornado, 
- quando faz isso, zera o acumulador e a linha.
-}

interpretarArquivo :: String -> String -> [String] -> CsvObj -> CsvObj
interpretarArquivo [] [] [] [] = return []
interpretarArquivo [] acumulador linha csvObj = csvObj++[linha++[acumulador]]
interpretarArquivo (simbolo:conteudo) acumulador linha csvObj
    | simbolo == ','  = interpretarArquivo conteudo "" (linha++[acumulador]) csvObj
    | simbolo == '\n' = interpretarArquivo conteudo "" [] (csvObj++[linha++[acumulador]])
    | otherwise = interpretarArquivo conteudo (acumulador++[simbolo]) linha csvObj

{-
- acao:       vai carregar o arquivo por meio do nomedoArquivo passado.  
- entrada:    caminho (String);  
- saida:      csvObj (executa entrada/saída (IO) e retorna um valor, valor CsvObj);
- suposicoes: o dado passado esta correto e o caminho nao estah vazio. 
- algoritmo:  por meio do caminho e da funcao readFile(FilePath -> IO String), atribui o valor em "conteudo" e faz a interpretacao do mesmo.
-}

carregarArquivo :: Caminho -> IO CsvObj
carregarArquivo nomeArquivo =
    do
        conteudo <- readFile nomeArquivo
        return (interpretarArquivo conteudo "" [] [])

{-
- acao:       recebe uma linha (lista de String) e transforma numa String que representa a linha.  
- entrada:    linha (lista de componentes da linha, Strings);  
- saida:      String com todos os conteudos da linha;
- suposicoes: o dado passado esta correto. 
- algoritmo:  vai percorrer a linha, concatenando todos os elementos e retornando essa String.
-}

csvLinhaParaString :: Linha -> String
csvLinhaParaString [] = ""
csvLinhaParaString [ultimoItem] = ultimoItem
csvLinhaParaString (item:linha) = item++","++(csvLinhaParaString linha)

{-
- acao:       recebe um objetoCsv(lista de linhas) e transforma numa String que representa o Csv.  
- entrada:    ObjetoCsv (lista de componentes do CsvObj, Linhas);  
- saida:      String com todos os conteudos do objeto;
- suposicoes: o dado passado esta correto. 
- algoritmo:  vai percorrer o objeto Csv, concatenando todos os elementos (linhas) e retornando essa String.
- Utiliza a funcao csvLinhaParaString.
-}

csvObjParaString :: CsvObj -> String
csvObjParaString [] = ""
csvObjParaString [ultimaLinha] = csvLinhaParaString ultimaLinha
csvObjParaString (linha:obj) = (csvLinhaParaString linha)++"\n"++(csvObjParaString obj)

{-
- acao:       recebe um caminho e um objeto Csv, e escreve o conteudo do objeto no arquivo(caminho).  
- entrada:    ObjetoCsv (lista de componentes do CsvObj, Linhas) e o caminho (String);  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O;
- suposicoes: os dados passados estao corretos. 
- algoritmo:  vai escrever o conteudo do objeto CsvObj no arquivo passado pelo Caminho. 
- Utiliza a funcao writeFile(FilePath -> String -> IO ()) e a funcao csvObjParaString(usada para transformar o conteudo do objeto numa String para ser passada).
-}

escreverEmArquivo :: Caminho -> CsvObj -> IO()
escreverEmArquivo nomeArquivo csvObj = writeFile nomeArquivo (csvObjParaString csvObj)