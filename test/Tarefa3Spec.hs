module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ "basic example test" ~: (2 :: Int) ~=? 1 + 1,
        "another basic example" ~: True ~=? not False
      ]
