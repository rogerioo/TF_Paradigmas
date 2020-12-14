# Trabalho Final de Paradigmas de Programação

- Aluno: Rogério S. dos Santos Júnior
- Professor: [Edson Alves](https://github.com/edsomjr)
- [Repositório da Disciplina](https://github.com/edsomjr/Paradigmas)

## Porte do WC de C para Haskell

Este trabalho consistiu no porte do comando [wc](https://github.com/wertarbyte/coreutils/blob/master/src/wc.c) do linux de C, linguagem imperativa, para Haskell que é uma linguagem funcional.

O wc é uma ferramenta usada para, por padrão, a contagem de linhas, palavras e caracteres de um arquivo. Para alterar o comportamento padrão basta usar uma das seguintes flags:

|            Flags            |                                      Função                                       |
|:---------------------------:|:---------------------------------------------------------------------------------:|
|        -c / --bytes         |                          Conta os caracteres do arquivo                           |
|        -m / --chars         |                           Conta os caracteres do arquivo                          |
|        -l / --lines         |                             Conta as linhas do arquivo                            |
|    -L / --max-line-length   |                     Conta o tamanho da maior linha do arquivo                     |
|        -w / --words         |                            Conta as palavras do arquivo                           |
|            --help           |                               Mostra o manual do wc                               |
|          --version          |                               Mostra a versão do wc                               |
| --files0-from=**file_path** |         Usa como arquivos de entrada os definido no arquivo **file_path**         |

Para que a flag *--files0-from=* possa funcionar corretamente, o arquivo de entrada deve usar como caractere separador entre os nomes dos arquivos o caractere nulo *\0*. Um exemplo de arquivo aceito pelo comando está [disponível](https://github.com/rogerioo/TF_Paradigmas/blob/master/file.txt) no repositório, e pode ser usado como modelo para outros.

## Como executar

Primeiramente, certifique-se de ter instalado o pacote de desenvolvimento do Haskell.

```bash
$ ghc --version
```

Você deve ver algo assim:

```bash
The Glorious Glasgow Haskell Compilation System, version 8.6.5
```

Caso não veja, rode o comando abaixo (Linux):

```bash
$ sudo apt-get install haskell-platform
```

Dentro do repositório, execute o seguinte comando:

```bash
$ ghc -o mywc wc.hs
```

Serão gerados os arquivos auxiliares e o executável. Para executar o programa basta chama-lo com o *./mywc* e passar os parâmetros com a mesma interface do Unix e os mesmo usados no comando original, como indicado anteriormente. Veja os exemplo abaixo.

```bash
$ ./mywc wc.c
$ ./mywc wc.c -wl
$ ./mywc wc.c -w -l -c
$ ./mywc wc.c -w -l -c file.txt
$ ./mywc --files0-from=file.txt
```

## Questionário Teórico

1. Qual é o paradigma predominante do porte?

> Paradigma Funcional

2. Quais foram os pontos do código que sofreram as maiores modificações?

> Principalmente, a forma como foi lidado com o arquivo. No código original é até complicado de entender como é feito o processamento de contagem pois é uma codificação muito baixo nível. Nessa nova versão é mais intuitivo o que está sendo realizado, pois ao invés de mexer com o arquivo é feita a manipulação dos seus dados em uma string.

3. Qual foi o ponto mais difícil/complicado do porte?

> Lidar com as Monads do Haskell! Como foi necessário trabalhar com a leitura de arquivo e leitura de argumentos do terminal, foi necessário usar o bloco *do* e do operador *<-* que foi bem confuso para mim, por conta da programação imperativa. As vezes queria fazer toda a lógica em uma instrução, mas foi necessário separa por conta do IO.

4. Quais características da linguagem-alvo facilitaram/dificultaram o porte?

> A facilidade é o alto nível do Haskell, tem muita coisa pronta que torna simples seu uso, como a leitura de arquivos, a captura de argumentos do terminal

> A dificuldade foi a sintaxe do Haskell que é bem diferente do que estamos acostumados na programação imperativa. Outro ponto é o IF-THEN-Else que torna mandatório a implementação do ELSE e ambos os retornos devem possuir o mesmo tipo. Foi necessária certa desconstrução de ideias e adaptação de lógicas para realizar o porte.

5. Quais características fundamentais do paradigma do porte tiveram de ser violadas/adaptadas/modificadas para que o porte fosse possível?

> Ainda que o Haskell possua a permissividade de trabalhar com IO, isso quebra um dos principais princípios das linguagens funcionais que é são as funções puras, que garante que o código fique isento de efeitos colaterais. Como funções de IO se comunicam com o terminal, a múltipla chamada de uma mesma função com os mesmo argumento pode ter um resultado diverso.

6. Quais foram as principais características do paradigma do porte que foram utilizadas/respeitadas?

> Esse trabalho foi uma ótima experiência, pois pude trabalhar com diversas características do Haskell:
> - Pattern Matching
> - Composição de funções
> - Recursão
> - Uso de guardas (foi removido da versão final por melhora do código, mas [inicialmente](https://github.com/rogerioo/TF_Paradigmas/blob/b175d4d09421778239895f6deb4d800daa40a882/wc.hs) foi usado)
> - Manipulação de Monads (do, <-)
> - Recursos alto nível (*interact*, captura de argumentos, execução de processo na máquina)
> - Uso de libs externas

7. A linguagem-alvo é adequada para o porte da ferramenta em questão ?

> Por conta da manipulação de argumentos do terminal e manipulação de arquivos, pode-se dizer que não, pois um dos princípios da programação funcional são as funções puras, e lidar com IO acaba quebrando essa filosofia.

> Olhando pelo lado da facilidade de implementar a ferramenta, pelo alto poder de programação que a linguagem proporciona (leitura de arquivo, interação com o terminal, captura de argumentos, funções alto nível) poderia se dizer que sim, pois o Haskell desenvolveu a estratégia de separar código puro de impuro, para que fique isento de efeitos colaterais.