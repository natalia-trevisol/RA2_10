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
Este projeto implementa um **sistema de inventário em Haskell**, desenvolvido como **atividade avaliativa (RA2)** da disciplina de Programação Lógica e Funcional.  

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
│ ├── Main.hs # Loop principal (I/O, comandos, persistência)
│
├── Inventario.dat # Persistência do inventário
├── Auditoria.log # Registro das operações
└── README.md # Documentação

---

## Requisitos

- [Haskell GHC](https://www.haskell.org/ghc/) (versão 9.x recomendada)  
- [Online GDB](https://www.onlinegdb.com/) **ou** [Replit](https://replit.com/) para execução online  
- Módulos padrão do Haskell:
  - `Data.Map.Strict`
  - `Data.Time`
  - `System.IO`
  - `Control.Exception`

---

## Como Executar

### Executar no **Online GDB** ou **Replit**
1. Acesse [https://www.onlinegdb.com/](https://www.onlinegdb.com/).
2. Crie um novo projeto em **Haskell**.
3. Copie o conteúdo de todos os arquivos `.hs` da pasta `src/` para o ambiente.
4. Execute o programa clicando em **Run**.

> Após publicação no GitHub, o link direto para execução deve ser inserido aqui:
> **[Executar no Online GDB](COLE_O_LINK_AQUI_APÓS_PUBLICAR)**

---

## Uso Interativo (Terminal)

Ao iniciar o programa (`main`), o usuário verá um menu como este:

=== SISTEMA DE INVENTÁRIO ===

Adicionar item

Remover item

Atualizar quantidade

Listar itens

Gerar relatório

Sair


Cada operação gera uma entrada no log (`Auditoria.log`) com **timestamp**, **ação**, **detalhes** e **status (Sucesso ou Falha)**.

Exemplo de entrada no log:
2025-11-08 15:23:41 | ADD | Item: item01 - Teclado | Sucesso

---

## Estrutura dos Tipos de Dados

### `Item`
```haskell
data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read, Eq)
Inventario
h
Copy code
type Inventario = Map String Item
AcaoLog e StatusLog
h
Copy code
data AcaoLog = Add | Remove | Update | QueryFail deriving (Show, Read)
data StatusLog = Sucesso | Falha String deriving (Show, Read)
LogEntry
haskell
Copy code
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)

Principais Funções Puras
Função	Descrição	Tipo
addItem	Adiciona um novo item ao inventário	UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
removeItem	Remove um item (ou quantidade)	UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty	Atualiza a quantidade de um item existente	UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
listarItens	Retorna a lista atual de itens	Inventario -> [Item]
logsDeErro	Filtra logs com falha	[LogEntry] -> [LogEntry]
historicoPorItem	Filtra logs de um item específico	String -> [LogEntry] -> [LogEntry]

🧾 Arquivos de Persistência
Inventario.dat → Contém o inventário atual serializado (Show/Read).

Auditoria.log → Contém todas as operações realizadas (append-only).

Os arquivos são criados automaticamente na primeira execução.

🧪 Cenários de Teste Manuais
✅ Cenário 1: Persistência de Estado (Sucesso)
 Iniciar o programa (sem arquivos de dados).

 Adicionar 3 itens.

 Fechar o programa.

 Verificar se Inventario.dat e Auditoria.log foram criados.

 Reiniciar o programa.

 Verificar se os 3 itens permanecem no inventário.

📋 Resultados Observados:
(Preencher após o teste)

⚠️ Cenário 2: Erro de Lógica (Estoque Insuficiente)
 Adicionar item “Teclado” com 10 unidades.

 Tentar remover 15 unidades.

 Verificar mensagem de erro e conteúdo do log.

 Conferir se a quantidade no inventário continua 10.

📋 Resultados Observados:
(Preencher após o teste)

📊 Cenário 3: Geração de Relatório de Erros
 Após o cenário 2, executar comando report.

 Verificar se o relatório mostra a falha de estoque insuficiente.

📋 Resultados Observados:
(Preencher após o teste)

🧰 Relatórios Disponíveis
O comando report permite gerar:

Histórico por item: movimentações de um produto específico.

Logs de erro: lista de operações que falharam.

Item mais movimentado: com base nas ações registradas.

🧑‍🔧 Autoria e Ética
Este projeto foi integralmente desenvolvido pela aluna Natália Moritani Trevisol,
seguindo as diretrizes da atividade avaliativa e respeitando o código de ética proposto:

“Você pode usar ferramentas de IA para dúvidas, mas o trabalho deve ser seu.”

🏁 Observações Finais
O sistema foi testado em execução contínua no Online GDB.

O inventário inicial foi populado com 10 itens distintos para validação.

Foram removidos acentos gráficos de strings para evitar incompatibilidades.

Toda a lógica segue a separação entre funções puras e impuras (IO).

📎 Link para o repositório GitHub:
https://github.com/natalia-trevisol/inventario-haskell

📎 Link para execução no Online GDB:
[COLE O LINK DO PROJETO AQUI DEPOIS DE PUBLICAR]
