module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import LI12425

--verificar os testes todos
testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [
        tinimigosNoAlcance,
        tatingeInimigo,
        tativaInimigo,
        tterminouJogo
      ]


tinimigosNoAlcance :: Test
tinimigosNoAlcance =
  TestLabel "Testes para inimigosNoAlcance" $
    test
      [ "nenhum inimigo no alcance" ~: [] ~=? inimigosNoAlcance torre [],
        "um inimigo no alcance" ~: [inimigo1] ~=? inimigosNoAlcance torre [inimigo1],
        "um inimigo fora do alcance" ~: [] ~=? inimigosNoAlcance torre [inimigo2],
        "múltiplos inimigos, alguns no alcance" ~: [inimigo1, inimigo3] ~=? inimigosNoAlcance torre [inimigo1, inimigo2, inimigo3]
      ]
  where
    torre = Torre {posicaoTorre = (0, 0), alcanceTorre = 6}
    inimigo1 = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = [], direcaoInimigo = Norte , velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0 }
    inimigo2 = Inimigo {posicaoInimigo = (10, 10), vidaInimigo = 10, projeteisInimigo = [], direcaoInimigo = Sul , velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0 }
    inimigo3 = Inimigo {posicaoInimigo = (2, 2), vidaInimigo = 10, projeteisInimigo = [], direcaoInimigo = Este , velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0 }


tatingeInimigo :: Test
tatingeInimigo =
  TestLabel "Testes para atingeInimigo" $
    test
      [ "inimigo fora do alcance" ~: inimigo1 ~=? atingeInimigo torre inimigo1,
        "inimigo no alcance, vida reduzida" ~: inimigo2 {vidaInimigo = 5} ~=? atingeInimigo torre inimigo2,
        "inimigo no alcance , vida nula" ~: inimigo3 {vidaInimigo = 0} ~=? atingeInimigo torre inimigo3
      ]
  where
    torre =  Torre {posicaoTorre = (0, 0), alcanceTorre = 6, danoTorre = 5, projetilTorre = []}
    inimigo1 = Inimigo {posicaoInimigo = (10, 10), vidaInimigo = 10, projeteisInimigo = [],direcaoInimigo = Norte, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0 } 
    inimigo2 = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = [], direcaoInimigo = Sul, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0 }
    inimigo3 = Inimigo {posicaoInimigo = (2, 2), vidaInimigo = 10, projeteisInimigo = [],direcaoInimigo = Este, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0 }


tativaInimigo :: Test
tativaInimigo =
  TestLabel "Testes para ativaInimigo" $
    test
      [ "nenhuma onda ativa" ~: (portal, []) ~=? ativaInimigo portal [],
        "onda ativa com inimigos" ~: (portal {ondasPortal = [novaOnda]}, [novoInimigo]) ~=? ativaInimigo portal [novoInimigo],
        "onda ativa sem inimigos" ~: (portal {ondasPortal = []}, []) ~=? ativaInimigo portal []
      ]
  where
    portal = Portal {posicaoPortal = (0,0), ondasPortal = []}
    novaOnda = Onda {entradaOnda = 0, inimigosOnda = [] }
    novoInimigo = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = []}

tterminouJogo :: Test
tterminouJogo =
  TestLabel "Testes para terminouJogo" $
    test
      [ "jogo não terminou (ainda existem inimigos  e a base ainda tem vida)" ~: False ~=? terminouJogo jogo1,
        "jogo terminou (ganhou, sem inimigos)" ~: True ~=? terminouJogo jogo2,
        "jogo terminou (perdeu, base com vida <= 0)" ~: True ~=? terminouJogo jogo3
      ]
  where
    jogo1 = Jogo {inimigosJogo = [inimigo1], baseJogo = base1}
    jogo2 = Jogo {inimigosJogo = [], baseJogo = base1}
    jogo3 = Jogo {inimigosJogo = [inimigo1], baseJogo = base2}
    base1 = Base {vidaBase = 10}
    base2 = Base {vidaBase = 0}
    inimigo1 = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = []}
   
