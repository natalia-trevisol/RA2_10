# Sistema de Inventário em Haskell

## Aluna
**Natália Moritani Trevisol**  
**Usuário no GitHub:** [natalia-trevisol](https://github.com/natalia-trevisol)

## Instituição
**Pontifícia Universidade Católica do Paraná (PUCPR)**  
**Disciplina:** Programação Lógica e Funcional  
**Professor:** Frank Coelho de Alcantara  

---

## Resumo do Projeto
Este projeto implementa um **sistema de inventário em Haskell**, desenvolvido como atividade avaliativa (RA2) da disciplina de Programação Lógica e Funcional.  

O sistema é capaz de:
- Gerenciar itens de um inventário (adição, remoção, atualização e listagem);
- Registrar cada operação (com sucesso ou falha) em um **log de auditoria**;
- **Persistir o estado** em disco através dos arquivos `Inventario.dat` e `Auditoria.log`;
- **Carregar automaticamente** os dados gravados em execuções anteriores;
- Gerar **relatórios** com base no histórico de logs, como erros ou movimentações por item.

Todo o programa foi construído com **funções puras** para a lógica de negócio e **funções impuras (IO)** apenas para interação e persistência, conforme os princípios da programação funcional.

---

## Estrutura do Projeto
inventario-haskell/
│
├── src/
│ ├── main.hs # Código-fonte completo
│
├── Inventario.dat # Persistência do inventário
├── Auditoria.log # Registro das operações
└── README.md # Documentação

---

## Requisitos

- [Haskell GHC](https://www.haskell.org/ghc/) (versão 9.x recomendada)
- [Online GDB](https://www.onlinegdb.com/) **ou** [Replit](https://replit.com/) para execução online (clique nos links para acessar os ambientes de execução)
- Módulos padrão do Haskell:
  - `Data.Map.Strict`
  - `Data.Time`
  - `System.IO`
  - `Control.Exception`

---

## Como Executar

### Executar no **Online GDB** ou **Replit**
1. Acesse [https://www.onlinegdb.com/](https://www.onlinegdb.com/).
2. Crie um novo projeto e selecione a linguagem **Haskell**.
3. Copie o conteúdo do arquivo `Inventario.hs` da pasta `src/` para o ambiente ou baixe e faça o upload de `Inventario.hs` no ambiente online.
4. Execute o programa clicando em **Run**.
5. O programa iniciará o loop interativo. Para visualizar os comandos basta digitar no terminal "help".
6. Siga as instruções no terminal para adicionar, remover, atualizar itens ou gerar relatórios digitando os comandos no terminal.
7. Após encerrar, verifique que os arquivos Inventario.dat e Auditoria.log foram criados automaticamente.

> Link direto para execução:
> **[Executar no Online GDB](COLE_O_LINK_AQUI_APÓS_PUBLICAR)**

---

## Uso Interativo (Terminal)
O programa inicia mostrando no terminal:
```
Iniciando Inventario (RA2) - carregando dados...
Itens carregados: 0
Entradas de log carregadas: 0
Digite 'help' para ver os comandos disponíveis.
```

O programa aceita comandos digitados. Para ver todos os disponíveis, basta digitar:
```
help
```

### Comandos disponíveis
| Comando | Exemplo | Descrição |
|----------|----------|-----------|
| `add <id> <nome> <quantidade> <categoria>` | `add item01 teclado 10 periferico` | Adiciona um novo item |
| `remove <id> <quantidade>` | `remove item01 5` | Remove certa quantidade (ou exclui se zerar) |
| `update <id> <novaQtd>` | `update item01 15` | Atualiza diretamente a quantidade |
| `list` | `list` | Lista todos os itens atuais do inventário |
| `report` | `report` | Exibe relatórios: erros e item mais movimentado |
| `historico <id>` | `historico item01` | Mostra histórico de logs para o item |
| `populateSample` | `populateSample` | Popula o inventário com 10 itens de exemplo |
| `help` | `help` | Mostra os comandos disponíveis |
| `exit` | `exit` | Encerra o programa |

---

## Estrutura de Dados

### `Item`
```haskell
data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read, Eq)
```
### `Inventario`
```haskell
newtype Inventario = Inventario (Map String Item)
```

### `AcaoLog e StatusLog`
```haskell
data AcaoLog = Add | Remove | Update | QueryFail | ListItems | Report
data StatusLog = Sucesso | Falha String
```

### `LogEntry`
```haskell
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)
```

---

## Principais Funções Puras

| Função             | Descrição                                  | Tipo                                                                        |
| ------------------ | ------------------------------------------ | --------------------------------------------------------------------------- |
| `addItem`          | Adiciona um novo item ao inventário        | `UTCTime -> Item -> Inventario -> Either String ResultadoOperacao`          |
| `removeItem`       | Remove um item (ou quantidade)             | `UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao` |
| `updateQty`        | Atualiza a quantidade de um item existente | `UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao` |
| `listarItens`      | Lista os itens do inventário               | `Inventario -> [Item]`                                                      |
| `logsDeErro`       | Filtra apenas as falhas do log             | `[LogEntry] -> [LogEntry]`                                                  |
| `historicoPorItem` | Retorna o histórico de um item             | `String -> [LogEntry] -> [LogEntry]`                                        |
| `itemMaisMovimentado` | Conta movimentações por item e retorna o mais citado  | `[LogEntry] -> Maybe (String, Int)`                           |

---

## Arquivos de Persistência

| Arquivo          | Descrição                                                |
| ---------------- | -------------------------------------------------------- |
| `Inventario.dat` | Contém o inventário atual serializado (via `Show/Read`). |
| `Auditoria.log`  | Registra todas as operações (append-only).               |

- Os arquivos são criados automaticamente na primeira execução.
- São atualizados a cada operação realizada.
  
---

## Cenários de Teste Manuais
### Cenário 1: Persistência de Estado (Sucesso)
 - Iniciar o programa (sem arquivos de dados).
 - Adicionar 3 itens.
 - Fechar o programa.
 - Verificar se Inventario.dat e Auditoria.log foram criados.
 - Reiniciar o programa.
 - Executar um comando de "listar" (a ser criado) ou verificar se o estado carregado em memória contém os 3 itens.
   
**Resultados Observados:**
(Preencher após o teste)

### Cenário 2: Erro de Lógica (Estoque Insuficiente)
 - Adicionar item “Teclado” com 10 unidades.
 - Tentar remover 15 unidades.
 - Verificar se o programa exibiu uma mensagem de erro clara.
 - Verificar se o Inventario.dat (e o estado em memória) ainda mostra 10 unidades.
 - Verificar se o Auditoria.log contém uma LogEntry com StatusLog (Falha ...)
   
**Resultados Observados:**
(Preencher após o teste)

### Cenário 3: Geração de Relatório de Erros
 - Após o cenário 2, executar comando report.
 - Verificar se o relatório gerado (especificamente pela função logsDeErro) exibe a entrada de log referente à falha registrada no Cenário 2 (a tentativa de remover estoque insuficiente).
   
**Resultados Observados:**
(Preencher após o teste)

--- 

## Relatórios Disponíveis

O comando `report` permite gerar:
- Logs de erro: histórico de operações que falharam.
- Item mais movimentado: com base nas ações registradas (nas palavras dos detalhes do log).

O comando `historico <id>` permite gerar:
- Histórico de operações de um item específico: retorna logs do item (sucessos e falhas). 

---

## Observações Finais
- O sistema foi testado em execução contínua no Online GDB.
- O inventário inicial pode ser criado automaticamente com populateSample (adiciona 10 itens).
- Foram removidos acentos gráficos de strings para evitar incompatibilidades.
- Toda a lógica segue a separação entre funções puras e IO (efeitos colaterais controlados).
