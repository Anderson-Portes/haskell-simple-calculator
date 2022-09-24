module Main (main) where

import System.IO (hFlush, stdout)

data TipoConta = Adicao | Subtracao | Multiplicacao | Divisao deriving (Show)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

parseTipoConta :: String -> TipoConta
parseTipoConta "+" = Adicao
parseTipoConta "-" = Subtracao
parseTipoConta "*" = Multiplicacao
parseTipoConta "/" = Divisao
parseTipoConta _ = error "Valor não aceito"

parseFuncao :: TipoConta -> (Int -> Int -> Int)
parseFuncao Adicao = (+)
parseFuncao Subtracao = (-)
parseFuncao Multiplicacao = (*)
parseFuncao Divisao = div

parseResultado :: String -> String -> String -> Int
parseResultado n1 sign n2 = (parseFuncao $ parseTipoConta sign) (read n1 :: Int) (read n2 :: Int)

continue :: String -> IO ()
continue "N" = pure ()
continue "n" = pure ()
continue _ = main

main :: IO ()
main = do
  n1 <- prompt "Digite o primeiro número: "
  sinal <- prompt "Opções:\n\n[+] - Adição\n[-] - Subtração\n[*] - Multiplicação\n[/] - Divisão\n\nEscolha: "
  n2 <- prompt "Digite o segundo número: "
  putStrLn ("\nResultado: " ++ show (parseResultado n1 sinal n2) ++ "\n")
  opcao <- prompt "Deseja continuar? [S/N]: "
  continue opcao
