
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12425

--verificar os testes todos
testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [
        tvalidaJogo,
        tvalidaPortaisBase,
        tvalidaTorreBase,
        tvalidaTorrePortal,
        tvalidaInimigosInativos,
        tvalidaInimigoTerra,
        tvalidaTorreInimigo,
        tvalidaInimigosVelocidade,
        tvalidaProjeteisInimigos,
        tvalidaTorreRelva,
        tvalidaAlcanceTorre,
        tvalidaRajadaTorre,
        tvalidaCicloTorre,
        tvalidaBaseTerra,
        tvalidaCreditoBase
      ]


tvalidaJogo :: Test
tvalidaJogo =
  TestLabel "Testes para validaJogo" $
    test
      [
        "jogo valido" ~: True ~=? validaJogo jogoValido,
        "jogo invalido" ~: False ~=? validaJogo jogoInvalido
      ]
    where
      jogoValido = Jogo {portaisJogo =[portal]}
      jogoInvalido = Jogo {portaisJogo = []}
      portal = Portal {posicaoPortal = (0,0), ondasPortal =[]}


tvalidaPortaisBase :: Test
tvalidaPortaisBase =
  TestLabel "Testes para validaPortaisBase" $
    test
      [
        "posicao do portal diferente da base" ~: True ~=? validaPortaisBase [portal] posicaoBase,
        " portal sobreposto à base" ~: False ~=? validaPortaisBase [portalSobreposto] posicaoBase
      ]
  where
     portal = Portal {posicaoPortal = (1,1), ondasPortal = []}
     portalSobreposto = Portal {posicaoPortal = (0,0), ondasPortal = []}
     posicaoBase = (0,0)


tvalidaTorreBase :: Test
tvalidaTorreBase =
  TestLabel "Testes para validaTorreBase" $
    test
     [
       "posicao da torre diferente da posicao da base" ~: True ~=? validaTorrePortal [torre] [portal],
       "torre sobreposta à base" ~: False ~=? validaTorreBase [torreSobreposta] posicaoBase
     ]
  where
     torre = Torre {posicaoTorre = (1,1), alcanceTorre = 0, danoTorre = 0 ,rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
     torreSobreposta = Torre {posicaoTorre = (0,0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, cicloTorre = 0 , tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
     posicaoBase = (0,0)
     portal = Portal {posicaoPortal = (0, 0), ondasPortal = []}


tvalidaTorrePortal :: Test
tvalidaTorrePortal =
  TestLabel "Testes para validaTorrePortal" $
    test
     [
        "posicao da torre diferente da posicao portal" ~: True ~=? validaTorrePortal [torre] [portal],
        "torre sobreposta ao portal" ~: False ~=? validaTorrePortal [torreSobreposta] [portal]
     ]
  where
     torre = Torre {posicaoTorre = (1, 1), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
     torreSobreposta = Torre {posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
     portal = Portal {posicaoPortal = (0, 0), ondasPortal = []}


tvalidaInimigosInativos :: Test
tvalidaInimigosInativos =
  TestLabel "Testes para validaInimigosInativos" $
    test
     [
        "inimigos inativos válidos" ~: True ~=? validaInimigosInativos inimigosInativos portais,
        "inimigo inativo com vida negativa" ~: False ~=? validaInimigosInativos [inimigoInvalido] portais
     ]
  where
     inimigosInativos = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = []}]
     inimigoInvalido = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = -5, projeteisInimigo = [], direcaoInimigo = Norte, velocidadeInimigo = 1}
     portais = [Portal {posicaoPortal = (0, 0), ondasPortal = []}]


tvalidaInimigoTerra :: Test
tvalidaInimigoTerra =
   TestLabel "Testes para validaInimigoTerra" $
     test
      [
         "inimigo em cima da terra" ~: True ~=? validaInimigoTerra [inimigo] mapaTerra,
         "inimigo em cima da água" ~: False ~=? validaInimigoTerra [inimigo] mapaAgua
      ]
   where
      inimigo = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = []}
      mapaTerra = [[Terra, Terra], [Terra, Terra]]
      mapaAgua = [[Agua, Agua], [Agua, Agua]]


tvalidaTorreInimigo :: Test
tvalidaTorreInimigo =
  TestLabel "Testes para validaTorreInimigo" $
    test
      [
         "torre e inimigo posicoes diferentes" ~: True ~=? validaTorreInimigo [torre] [inimigo],
         "torre e inimigo sobrepostos" ~: False ~=? validaTorreInimigo [torreSobreposta] [inimigo]
      ]
   where
      torre = Torre {posicaoTorre = (1, 1), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
      torreSobreposta = Torre {posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
      inimigo = Inimigo {posicaoInimigo = (0, 0), vidaInimigo = 10, projeteisInimigo = []}


tvalidaInimigosVelocidade :: Test
tvalidaInimigosVelocidade =
   TestLabel "Testes para validaInimigosVelocidade" $
     test
       [
          "inimigo com velocidade positiva" ~: True ~=? validaInimigosVelocidade [inimigoValido],
          "inimigo com velocidade negativa" ~: False ~=? validaInimigosVelocidade [inimigoInvalido],
          "inimigo com velocidade nula" ~: False ~=? validaInimigosVelocidade [inimigoParado]
       ]
    where
       inimigoValido = Inimigo {velocidadeInimigo = 5, posicaoInimigo = (0, 0), vidaInimigo = 10, projeteisInimigo = []}
       inimigoInvalido = Inimigo {velocidadeInimigo = -2, posicaoInimigo = (0, 0), vidaInimigo = 10, projeteisInimigo = []}
       inimigoParado = Inimigo {velocidadeInimigo = 0, posicaoInimigo = (0, 0), vidaInimigo = 10, projeteisInimigo = []}


tvalidaProjeteisInimigos :: Test
tvalidaProjeteisInimigos =
   TestLabel "Testes para validaProjeteisInimigos" $
    test
      [
         "inimigo com projeteis validos" ~: True ~=? validaProjeteisInimigos [inimigoValido],
         "inimigo com projeteis duplicados" ~: False ~=? validaProjeteisInimigos [inimigoInvalido1],
         "inimigos com projeteis combinados" ~: False ~=? validaProjeteisInimigos [inimigoInvalido2]
      ]
    where
       inimigoValido = Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}], posicaoInimigo = (0, 0), vidaInimigo = 10}
       inimigoInvalido1 = Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}, Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}], posicaoInimigo = (0, 0), vidaInimigo = 10}
       inimigoInvalido2 = Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 0}], posicaoInimigo = (0, 0), vidaInimigo = 10}


tvalidaTorreRelva :: Test
tvalidaTorreRelva =
  TestLabel "Testes para validaTorreRelva" $
    test
      [
         "torre em cima da relva" ~: True ~=? validaTorreRelva [torre] mapaRelva,
         "torre em cima da terra" ~: False ~=? validaTorreRelva [torre] mapaTerra
      ]
    where
       torre = Torre {posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
       mapaRelva = [[Relva, Relva], [Relva, Relva]]
       mapaTerra = [[Terra, Terra], [Terra, Terra]]


tvalidaAlcanceTorre :: Test
tvalidaAlcanceTorre =
  TestLabel "Testes para validaAlcanceTorre" $
    test
      [
         "torre com alcance positivo" ~: True ~=? validaAlcanceTorre [torreValida],
         "torre com alcance negativo" ~: False ~=? validaAlcanceTorre [torreInvalida],
         "torre com alcance nulo" ~: False ~=? validaAlcanceTorre [torreNula]
      ]
    where
      torreValida = Torre {alcanceTorre = 5, posicaoTorre = (0, 0), danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
      torreInvalida = Torre {alcanceTorre = -2, posicaoTorre = (0, 0), danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
      torreNula = Torre {alcanceTorre = 0, posicaoTorre = (0, 0), danoTorre = 0, rajadaTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}


tvalidaRajadaTorre :: Test
tvalidaRajadaTorre =
  TestLabel "Testes para validaRajadaTorre" $
   test
     [
        "torre com rajada positiva" ~: True ~=? validaRajadaTorre [torreValida],
        "torre com rajada negativa" ~: False ~=? validaRajadaTorre [torreInvalida],
        "torre com rajada nula" ~: False ~=? validaRajadaTorre [torreNula]
     ]
  where
    torreValida = Torre {rajadaTorre = 2, posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
    torreInvalida = Torre {rajadaTorre = -2, posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
    torreNula = Torre {rajadaTorre = 0, posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, cicloTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}


tvalidaCicloTorre :: Test
tvalidaCicloTorre =
  TestLabel "Testes para validaCicloTorre" $
    test
      [
         "torre com ciclo positivo" ~: True ~=? validaCicloTorre [torreValida],
         "torre com ciclo negativo" ~: False ~=? validaCicloTorre [torreInvalida],
         "torre com ciclo nulo" ~: False ~=? validaCicloTorre [torreNula]
      ]
    where
      torreValida = Torre {cicloTorre = 5, posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
      torreInvalida = Torre {cicloTorre = -2, posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}
      torreNula = Torre {cicloTorre = 0, posicaoTorre = (0, 0), alcanceTorre = 0, danoTorre = 0, rajadaTorre = 0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}}


tvalidaBaseTerra :: Test
tvalidaBaseTerra =
  TestLabel "Testes para validaBaseTerra" $
   test
     [
        "base em cima da terra" ~: True ~=? validaBaseTerra (0, 0) mapaTerra,
        "base em cima da relva" ~: False ~=? validaBaseTerra (0, 0) mapaRelva
     ]
  where
    mapaTerra = [[Terra, Terra], [Terra, Terra]]
    mapaRelva = [[Relva, Relva], [Relva, Relva]]


tvalidaCreditoBase :: Test
tvalidaCreditoBase =
  TestLabel "Testes para validaCreditoBase" $
    test
      [
        "creditos positivos" ~: True ~=? validaCreditoBase 10,
        "creditos negativos " ~: False ~=? validaCreditoBase (-20),
        "creditos nulos" ~: False ~=? validaCreditoBase 0
      ]