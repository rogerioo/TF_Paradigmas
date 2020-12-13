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

Para que a flag *--files0-from=* possa funcionar corretamente, o arquivo de entrada deve usar como caractere separador entre os nomes dos arquivos o caractere nulo *\0*.

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