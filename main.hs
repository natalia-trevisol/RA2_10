-- Inventario.hs
-- Projeto RA2 Inventário - Programação Lógica e Funcional
-- Sistema de gerenciamento de inventário com persistência em arquivo e auditoria

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Importações necessárias para o sistema
import qualified Data.Map.Strict as Map  -- Map para armazenamento eficiente
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)  -- Timestamp para logs
import System.IO (hFlush, stdout)  -- Controle de I/O
import Control.Exception (catch, IOException)  -- Tratamento de exceções
import System.Directory (doesFileExist)  -- Verificação de arquivos
import Data.List (foldl', maximumBy)  -- Operações em listas
import Data.Ord (comparing)  -- Comparação para ordenação

-- =============================================================================
-- DEFINIÇÕES DE TIPOS DE DADOS
-- =============================================================================

-- | Representa um item no inventário com identificador único
data Item = Item
  { itemID   :: String    -- Identificador único do item
  , nome     :: String    -- Nome descritivo do item
  , quantidade :: Int     -- Quantidade em estoque
  , categoria :: String   -- Categoria para organização
  } deriving (Show, Read, Eq)  -- Permite serialização e comparação

-- | Inventário como um mapa de IDs para Itens
-- Usa newtype para poder derivar Show/Read (type synonym não permite)
newtype Inventario = Inventario (Map String Item)
  deriving (Show, Read, Eq)

-- | Função auxiliar para extrair o Map interno do Inventario
-- Necessária porque newtype cria um wrapper que precisa ser desembrulhado
unInventario :: Inventario -> Map String Item
unInventario (Inventario m) = m

-- | Tipos de ações que podem ser registradas no log
data AcaoLog = Add          -- Adicionar item
            | Remove       -- Remover quantidade
            | Update       -- Atualizar quantidade
            | QueryFail    -- Consulta que falhou
            | ListItems    -- Listagem de itens
            | Report       -- Geração de relatório
  deriving (Show, Read, Eq)

-- | Status do resultado de uma operação
data StatusLog = Sucesso               -- Operação bem-sucedida
              | Falha String          -- Operação falhou com mensagem de erro
  deriving (Show, Read, Eq)

-- | Entrada de log para auditoria do sistema
data LogEntry = LogEntry
  { timestamp :: UTCTime    -- Quando a ação ocorreu
  , acao      :: AcaoLog    -- Tipo de ação realizada
  , detalhes  :: String     -- Descrição detalhada da ação
  , status    :: StatusLog  -- Resultado da operação
  } deriving (Show, Read, Eq)

-- | Tipo que representa o resultado de uma operação no sistema
-- Contém o novo estado do inventário e a entrada de log correspondente
type ResultadoOperacao = (Inventario, LogEntry)

-- =============================================================================
-- FUNÇÕES PURAS DE LÓGICA DE NEGÓCIO 
-- =============================================================================

-- | Adiciona um novo item ao inventário
-- Falha se já existir um item com o mesmo ID
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem now novo inv =
  if Map.member (itemID novo) (unInventario inv)
    then Left $ "Item com ID já existe: " ++ itemID novo  -- Erro: ID duplicado
    else
      let invMap = unInventario inv
          inv' = Inventario (Map.insert (itemID novo) novo invMap)  -- Novo inventário
          logE = LogEntry now Add ("Add: " ++ itemID novo ++ " - " ++ nome novo) Sucesso
       in Right (inv', logE)  -- Sucesso: retorna novo estado e log

-- | Remove quantidade de um item existente
-- Falha se item não existe, quantidade inválida ou estoque insuficiente
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem now idQtd qtd inv =
  case Map.lookup idQtd (unInventario inv) of
    Nothing -> Left $ "Item não encontrado: " ++ idQtd  -- Erro: item não existe
    Just it ->
      let atual = quantidade it
       in if qtd <= 0
            then Left $ "Quantidade inválida para remoção: " ++ show qtd  -- Erro: qtd negativa
            else if atual < qtd
              then Left $ "Estoque insuficiente para " ++ idQtd ++ " (tem " ++ show atual ++ ", pediu " ++ show qtd ++ ")"  -- Erro: estoque insuficiente
              else
                let novoItem = it { quantidade = atual - qtd }  -- Atualiza quantidade
                    invMap = unInventario inv
                    -- Remove item se quantidade chegar a zero, caso contrário atualiza
                    inv' = if quantidade novoItem <= 0
                             then Inventario (Map.delete idQtd invMap)
                             else Inventario (Map.insert idQtd novoItem invMap)
                    detalhes = "Remove: " ++ show qtd ++ " de " ++ idQtd
                    logE = LogEntry now Remove detalhes Sucesso
                 in Right (inv', logE)  -- Sucesso

-- | Atualiza a quantidade de um item para um valor específico
-- Falha se item não existe ou quantidade é negativa
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty now idItem novaQtd inv =
  case Map.lookup idItem (unInventario inv) of
    Nothing -> Left $ "Item não encontrado: " ++ idItem  -- Erro: item não existe
    Just it ->
      if novaQtd < 0
        then Left $ "Quantidade negativa não permitida: " ++ show novaQtd  -- Erro: qtd negativa
        else
          let novoItem = it { quantidade = novaQtd }  -- Cria item com nova quantidade
              inv' = Inventario (Map.insert idItem novoItem (unInventario inv))  -- Atualiza inventário
              detalhes = "UpdateQty: " ++ idItem ++ " -> " ++ show novaQtd
              logE = LogEntry now Update detalhes Sucesso
           in Right (inv', logE)  -- Sucesso

-- =============================================================================
-- FUNÇÕES PURAS DE RELATÓRIO 
-- =============================================================================

-- | Filtra logs pelo histórico de um item específico
-- Busca o ID do item nos detalhes de cada entrada de log
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem logs =
  filter (\le -> idItem `elem` words (detalhes le)) logs

-- | Filtra apenas as entradas de log que representam falhas
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isFalha
  where 
    -- Função auxiliar que verifica se um LogEntry representa uma falha
    isFalha le = case status le of
                   Falha _ -> True  -- É uma falha
                   _       -> False -- Não é uma falha

-- | Identifica o item mais movimentado baseado na frequência nos logs
-- Usa uma simplificação: conta palavras nas descrições dos logs
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  if null itens
    then Nothing  -- Nenhum dado disponível
    else Just $ maximumBy (comparing snd) itens  -- Item com maior contagem
  where
    -- Extrai todas as palavras de todos os detalhes dos logs
    palavras = concatMap (words . detalhes) logs
    -- Conta frequência de cada palavra usando Map
    contador = foldl' (\m p -> Map.insertWith (+) p 1 m) Map.empty palavras
    -- Converte Map para lista de pares (palavra, contagem)
    itens = Map.toList contador

-- =============================================================================
-- SISTEMA DE PERSISTÊNCIA E I/O 
-- =============================================================================

-- | Caminhos dos arquivos de persistência
inventarioFile :: FilePath
inventarioFile = "Inventario.dat"  -- Arquivo do estado do inventário

auditoriaFile :: FilePath
auditoriaFile = "Auditoria.log"    -- Arquivo de log de auditoria

-- | Carrega o inventário do arquivo, ou retorna vazio se arquivo não existe
-- Usa catch para tratar exceções de I/O graciosamente
loadInventario :: IO Inventario
loadInventario = do
  exists <- doesFileExist inventarioFile
  if not exists
    then return (Inventario Map.empty)  -- Estado inicial vazio
    else do
      content <- readFile inventarioFile `catch` handler  -- Lê com tratamento de erro
      case reads content :: [(Inventario, String)] of
        [(inv, _)] -> return inv  -- Sucesso na desserialização
        _ -> return (Inventario Map.empty)  -- Falha na desserialização
  where
    -- Handler para exceções de I/O (arquivo corrompido, permissões, etc.)
    handler :: IOException -> IO String
    handler _ = return ""  -- Retorna string vazia em caso de erro

-- | Carrega o log de auditoria do arquivo
-- Cada linha do arquivo representa um LogEntry serializado
loadAuditLog :: IO [LogEntry]
loadAuditLog = do
  exists <- doesFileExist auditoriaFile
  if not exists
    then return []  -- Lista vazia se arquivo não existe
    else do
      content <- readFile auditoriaFile `catch` handler
      let ls = lines content  -- Divide em linhas
          parsed = mapMaybeRead ls  -- Parseia cada linha
      return parsed
  where
    handler :: IOException -> IO String
    handler _ = return ""
    -- Função auxiliar para parsear linhas, ignorando linhas inválidas
    mapMaybeRead :: [String] -> [LogEntry]
    mapMaybeRead = foldr (\s acc -> case reads s :: [(LogEntry, String)] of
                                      [(le, _)] -> le : acc  -- Linha válida
                                      _         -> acc) []   -- Linha inválida, ignora

-- | Salva o inventário no arquivo (sobrescreve)
saveInventario :: Inventario -> IO ()
saveInventario inv = writeFile inventarioFile (show inv)  -- Serializa e salva

-- | Adiciona uma entrada ao log de auditoria (append)
appendAudit :: LogEntry -> IO ()
appendAudit le = appendFile auditoriaFile (show le ++ "\n")  -- Serializa e adiciona

-- =============================================================================
-- SISTEMA DE INTERAÇÃO E LOOP PRINCIPAL
-- =============================================================================

-- | Exibe prompt e lê comando do usuário
prompt :: IO String
prompt = do
  putStr "> "      -- Exibe prompt
  hFlush stdout    -- Garante que o prompt é exibido imediatamente
  getLine          -- Lê entrada do usuário

-- | Exibe ajuda com todos os comandos disponíveis
printHelp :: IO ()
printHelp = do
  putStrLn "Comandos disponíveis (formatos):"
  putStrLn " add <id> <nome> <quantidade> <categoria>"
  putStrLn " remove <id> <quantidade>"
  putStrLn " update <id> <novaQuantidade>"
  putStrLn " list                -- lista itens no inventario"
  putStrLn " report              -- executa relatórios sobre Auditoria.log"
  putStrLn " historico <id>      -- histórico de logs por item"
  putStrLn " populateSample      -- popula inventario com 10 itens de exemplo (útil para testes)"
  putStrLn " help"
  putStrLn " exit"

-- | Divide string em tokens (comando e argumentos)
-- Simplificação: não lida com aspas ou espaços em nomes
tokenize :: String -> [String]
tokenize = words

-- | Processa um comando do usuário e executa a ação correspondente
-- Esta é a função principal que orquestra toda a lógica do sistema
processCommand :: Inventario -> [LogEntry] -> String -> IO (Inventario, [LogEntry], Bool)
processCommand inv logs linha =
  case tokenize linha of
    -- COMANDO: add - Adiciona novo item
    ("add":idn:nome:qtdStr:catParts) -> do
      now <- getCurrentTime  -- Timestamp para o log
      let categoriaStr = unwords catParts  -- Junta partes restantes como categoria
      case safeReadInt qtdStr of
        Nothing -> do  -- Erro: quantidade inválida
          let le = LogEntry now Add ("Add falha: parse qtd " ++ qtdStr) (Falha "parse quantidade")
          appendAudit le
          putStrLn "ERRO: Quantidade invalida. Use um numero inteiro."
          return (inv, le:logs, False)
        Just qtd ->
          let novo = Item idn nome qtd categoriaStr
           in case addItem now novo inv of  -- Chama função pura
                Left err -> do  -- Erro da lógica de negócio
                  let le = LogEntry now Add ("Add falha: " ++ err) (Falha err)
                  appendAudit le
                  putStrLn $ "ERRO: " ++ err
                  return (inv, le:logs, False)
                Right (inv', le) -> do  -- Sucesso
                  saveInventario inv'   -- Persiste novo estado
                  appendAudit le        -- Registra no log
                  putStrLn "SUCESSO: Item adicionado ao inventario."
                  return (inv', le:logs, False)

    -- COMANDO: remove - Remove quantidade de item
    ("remove":idn:qtdStr:_) -> do
      now <- getCurrentTime
      case safeReadInt qtdStr of
        Nothing -> do  -- Erro: quantidade inválida
          let le = LogEntry now Remove ("Remove falha: parse qtd " ++ qtdStr) (Falha "parse quantidade")
          appendAudit le
          putStrLn "ERRO: Quantidade invalida. Use um numero inteiro positivo."
          return (inv, le:logs, False)
        Just qtd ->
          case removeItem now idn qtd inv of
            Left err -> do  -- Erro da lógica
              let le = LogEntry now Remove ("Remove falha: " ++ err) (Falha err)
              appendAudit le
              putStrLn $ "ERRO: " ++ err
              return (inv, le:logs, False)
            Right (inv', le) -> do  -- Sucesso
              saveInventario inv'
              appendAudit le
              putStrLn "SUCESSO: Quantidade removida do inventario."
              return (inv', le:logs, False)

    -- COMANDO: update - Atualiza quantidade de item
    ("update":idn:qtdStr:_) -> do
      now <- getCurrentTime
      case safeReadInt qtdStr of
        Nothing -> do  -- Erro: quantidade inválida
          let le = LogEntry now Update ("Update falha: parse qtd " ++ qtdStr) (Falha "parse quantidade")
          appendAudit le
          putStrLn "ERRO: Quantidade invalida. Use um numero inteiro nao negativo."
          return (inv, le:logs, False)
        Just novaQtd ->
          case updateQty now idn novaQtd inv of
            Left err -> do  -- Erro da lógica
              let le = LogEntry now Update ("Update falha: " ++ err) (Falha err)
              appendAudit le
              putStrLn $ "ERRO: " ++ err
              return (inv, le:logs, False)
            Right (inv', le) -> do  -- Sucesso
              saveInventario inv'
              appendAudit le
              putStrLn "SUCESSO: Quantidade atualizada no inventario."
              return (inv', le:logs, False)

    -- COMANDO: list - Lista todos os itens
    ("list":_) -> do
      now <- getCurrentTime
      let le = LogEntry now ListItems "List items" Sucesso
      appendAudit le
      putStrLn "Inventario atual:"
      mapM_ (putStrLn . showItem) (Map.elems (unInventario inv))  -- Exibe cada item
      return (inv, le:logs, False)

    -- COMANDO: report - Gera relatórios dos logs
    ("report":_) -> do
      now <- getCurrentTime
      let le = LogEntry now Report "Report requested" Sucesso
      appendAudit le
      putStrLn "---- Relatorios ----"
      putStrLn "Erros registrados (logsDeErro):"
      mapM_ (putStrLn . show) (logsDeErro logs)  -- Exibe logs de erro
      putStrLn "Item mais movimentado (palavras nas descricoes):"
      case itemMaisMovimentado logs of
        Nothing -> putStrLn "Nenhum dado de movimentacao."
        Just (it, c) -> putStrLn $ it ++ " -> " ++ show c  -- Exibe item mais frequente
      return (inv, le:logs, False)

    -- COMANDO: historico - Histórico de um item específico
    ("historico":idn:_) -> do
      now <- getCurrentTime
      let le = LogEntry now QueryFail ("Historico de " ++ idn) Sucesso
      appendAudit le
      let h = historicoPorItem idn logs  -- Filtra logs do item
      putStrLn $ "Historico para " ++ idn ++ ":"
      mapM_ (putStrLn . show) h  -- Exibe histórico
      return (inv, le:logs, False)

    -- COMANDO: populateSample - Adiciona 10 itens de exemplo
    ("populateSample":_) -> do
      now <- getCurrentTime
      -- Gera lista de 10 itens de exemplo com IDs, nomes, quantidades e categorias
      let sampleList =
            [ Item ("item" ++ show i) ("nome" ++ show i) (10 + i) ("cat" ++ show (i `mod` 3))
            | i <- [1..10]
            ]
          -- Adiciona todos os itens ao inventário atual
          inv' = Inventario (foldl' (\m it -> Map.insert (itemID it) it m) (unInventario inv) sampleList)
          le = LogEntry now Add "populateSample: 10 itens adicionados" Sucesso
      saveInventario inv'
      appendAudit le
      putStrLn "SUCESSO: 10 itens de exemplo adicionados ao inventario."
      return (inv', le:logs, False)
      
    -- COMANDO: help - Exibe ajuda
    ("help":_) -> do
      printHelp
      return (inv, logs, False)

    -- COMANDO: exit - Sai do programa
    ("exit":_) -> do
      putStrLn "Saindo..."
      return (inv, logs, True)  -- Flag para terminar o loop

    -- Linha vazia - ignora
    [] -> return (inv, logs, False)

    -- COMANDO inválido
    _ -> do
      now <- getCurrentTime
      let le = LogEntry now QueryFail ("Comando invalido: " ++ linha) (Falha "Comando invalido")
      appendAudit le
      putStrLn "ERRO: Comando invalido. Digite 'help' para ver comandos disponiveis."
      return (inv, le:logs, False)

-- =============================================================================
-- FUNÇÕES AUXILIARES
-- =============================================================================

-- | Tenta converter String para Int de forma segura
-- Retorna Nothing se a string não representar um número inteiro válido
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s :: [(Int, String)] of
                  [(n, "")] -> Just n  -- Sucesso: string é um número
                  _         -> Nothing -- Falha: string não é número

-- | Formata um Item para exibição amigável
showItem :: Item -> String
showItem it = itemID it ++ " | " ++ nome it ++ " | qtd: " ++ show (quantidade it) ++ " | cat: " ++ categoria it

-- =============================================================================
-- LOOP PRINCIPAL E FUNÇÃO MAIN
-- =============================================================================

-- | Loop principal do programa
-- Mantém estado do inventário e logs entre comandos
mainLoop :: Inventario -> [LogEntry] -> IO ()
mainLoop inv logs = do
  linha <- prompt  -- Lê comando
  (inv', logs', shouldExit) <- processCommand inv logs linha  -- Processa comando
  if shouldExit
    then return ()  -- Termina se recebeu comando exit
    else mainLoop inv' logs'  -- Continua loop com novo estado

-- | Ponto de entrada do programa
-- Inicializa sistema e inicia loop principal
main :: IO ()
main = do
  putStrLn "Iniciando Inventario (RA2) - carregando dados..."
  -- Carrega estado persistente
  inv <- loadInventario
  logs <- loadAuditLog
  -- Exibe status inicial
  putStrLn $ "Itens carregados: " ++ show (Map.size (unInventario inv))
  putStrLn $ "Entradas de log carregadas: " ++ show (length logs)
  putStrLn "Digite 'help' para ver os comandos disponiveis."
  -- Inicia loop de interação
  mainLoop inv logs