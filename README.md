# hs2json

Projeto em Haskell para representar valores JSON e convertê-los em texto, com suporte a formatação e testes baseados em propriedades usando QuickCheck.

## Objetivos

- Representar valores JSON por meio de um tipo algébrico recursivo.
- Disponibilizar acessores seguros para os diferentes tipos de valores.
- Implementar a renderização de valores JSON.
- Controlar a formatação e a indentação dos documentos.
- Verificar o comportamento da implementação por meio de testes com QuickCheck.

## Tecnologias

- Haskell
- GHC
- Cabal
- QuickCheck

## Arquitetura do projeto

O projeto está organizado nos seguintes módulos:

| Arquivo | Responsabilidade |
|---|---|
| `src/SimpleJSON.hs` | Define o tipo `JValue` e os acessores seguros para strings, números, booleanos, objetos, arrays e valores nulos. |
| `src/PutJSON.hs` | Implementa a renderização de valores JSON para `String`. |
| `src/Prettify.hs` | Define o tipo `Doc` e os combinadores utilizados na composição, formatação e indentação de documentos. |
| `src/PrettyJSON.hs` | Converte valores `JValue` em documentos formatados, incluindo o tratamento de caracteres especiais em strings. |
| `app/Main.hs` | Executa um exemplo de criação e renderização de um objeto JSON. |
| `test/Spec.hs` | Contém o gerador `Arbitrary JValue` e as propriedades verificadas com QuickCheck. |

## Como clonar o repositório

```powershell
git clone https://github.com/liahcolins/hs2json.git
cd hs2json
```

## Como compilar

Na raiz do projeto, execute:

```powershell
cabal build
```

## Como executar

Execute o programa principal com:

```powershell
cabal run
```

O programa demonstra a renderização de um objeto JSON. A saída atual é semelhante a:

```json
{"foo": 1.0, "bar": false}
```

## Como executar os testes

Execute:

```powershell
cabal test
```

A suíte utiliza QuickCheck para verificar propriedades relacionadas aos acessores de `JValue`, ao reconhecimento de valores nulos e à renderização de valores JSON.

Entre os comportamentos testados estão:

- Recuperação dos valores por meio dos acessores.
- Conversão de números para inteiros por truncamento.
- Reconhecimento de valores nulos.
- Renderização de números, booleanos, valores nulos, arrays e objetos vazios.
- Escape de caracteres especiais em strings.

## Documentação da API

Para gerar e abrir a documentação da API, execute:

```powershell
cabal haddock --open
```

## Vídeo de demonstração

**Duração exigida:** 10 a 15 minutos.

**Link do vídeo:** (https://drive.google.com/drive/folders/1WUCYHf0bj1NKVKwFk8h5TUSNqUa6qPqJ?usp=sharing)

O vídeo deverá demonstrar a arquitetura do projeto, a compilação com `cabal build`, a execução dos testes com `cabal test`, a execução do programa com `cabal run` e as dificuldades encontradas durante o desenvolvimento.

Após publicar o vídeo, substitua esta indicação pelo link real.

## Repositório

[GitHub — liahcolins/hs2json](https://github.com/liahcolins/hs2json)