# Transmissão da Política Monetária: uma análise com modelo VAR

Trabalho desenvolvido no Instituto de Economia da Universidade Federal do Rio de Janeiro (UFRJ), em junho de 2025, com o objetivo de analisar os principais canais de transmissão da política monetária no Brasil.

## Objetivo

O trabalho investiga como alterações na taxa básica de juros (Selic) afetam variáveis macroeconômicas como inflação, atividade econômica e taxa de câmbio.

A análise utiliza dados mensais entre janeiro de 2014 e dezembro de 2024 e aplica técnicas de econometria de séries temporais para avaliar a dinâmica entre as variáveis.

## Metodologia

O exercício econométrico inclui:

- análise gráfica das séries;
- testes de raiz unitária Augmented Dickey-Fuller (ADF);
- teste de Zivot-Andrews para possíveis quebras estruturais;
- teste de cointegração;
- seleção do número de defasagens;
- estimação de um modelo VAR;
- testes de diagnóstico dos resíduos;
- funções impulso-resposta.

As principais variáveis utilizadas são:

- IPCA;
- taxa Selic;
- IBC-Br;
- taxa de câmbio efetiva real.

A série do IPCA foi construída a partir da variação mensal do IPCA, série 433 do Sistema Gerenciador de Séries Temporais do Banco Central, por meio de mudança de base.

As séries do IBC-Br e da taxa de câmbio foram transformadas em logaritmos antes dos testes e diferenciações.

## Modelo

Após os testes de estacionaridade, o modelo final foi estimado como um VAR(2), utilizando:

- primeira diferença do IPCA;
- segunda diferença da Selic;
- primeira diferença do log do IBC-Br;
- primeira diferença do log da taxa de câmbio.

Também foram incluídas variáveis dummy para tratar quebras observadas no fim de 2015 e durante o período da pandemia.

## Resultados

O modelo apresentou evidência de persistência inflacionária e dinâmica autorregressiva em algumas das variáveis analisadas.

O teste Portmanteau não indicou autocorrelação dos resíduos, embora tenha sido encontrada evidência de heterocedasticidade.

As funções impulso-resposta estimadas não apresentaram respostas estatisticamente significativas nos horizontes analisados. Dessa forma, os resultados não permitem identificar de maneira robusta os canais de transmissão da política monetária por meio da especificação utilizada.

## Estrutura do repositório

```text
.
├── README.md
├── codigo/
│   └── trabalho-macroeconometria.R
├── dados/
│   └── ipca_selic_pib.xlsx
├── trabalho/
│   └── transmissao-politica-monetaria-var.pdf
└── .gitignore
