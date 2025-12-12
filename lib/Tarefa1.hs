{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Vasco Marques Machado <a109949@alunos.uminho.pt>
              Duarte Emanuel Soares Arruda <a109840@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425


-- | Função que verifica que todas as funções que se seguem retornam um resultado verdadeiro, validando assim que o jogo reune todas as condições necessárias para funcionar corretamente
validaJogo :: Jogo -> Bool
validaJogo Jogo{portaisJogo = []} = False
validaJogo Jogo{baseJogo = Base{posicaoBase = b, creditosBase = c}, portaisJogo = p@(x:y), mapaJogo = mapa, torresJogo = t, inimigosJogo = i} = validaPortaisBase p b && validaTorreBase t b && validaTorrePortal t p && validaInimigosInativos i p && validaInimigoTerra i mapa && validaTorreInimigo t i && validaInimigosVelocidade i && validaProjeteisInimigos i && validaProjeteisInimigos i && validaTorreRelva t mapa && validaAlcanceTorre t && validaRajadaTorre t && validaCicloTorre t && validaTorresSobrepostas t && validaBaseTerra b mapa && validaCreditoBase c

validaPortais :: Portal -> Mapa -> Posicao -> Bool
validaPortais Portal{posicaoPortal = (x,y)} mapa b = validaCaminhoTerra (x,y) mapa b

validaCaminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
validaCaminhoTerra (x,y) mapa b = (eTerra (x,y) mapa) && (caminhoTerra (x-1,y) mapa b Este || caminhoTerra (x+1,y) mapa b Oeste || caminhoTerra (x,y+1) mapa b Sul || caminhoTerra (x,y-1) mapa b Norte)

caminhoTerra :: Posicao -> Mapa -> Posicao -> Direcao -> Bool
caminhoTerra p@(x,y) mapa b Norte = if p == b then True else if rodeadoTerra p mapa && eTerra p mapa then caminhoTerra (x-1,y) mapa b Este || caminhoTerra (x+1,y) mapa b Oeste || caminhoTerra (x,y+1) mapa b Sul else False
caminhoTerra p@(x,y) mapa b Este = if p == b then True else if rodeadoTerra p mapa && eTerra p mapa then caminhoTerra (x+1,y) mapa b Oeste || caminhoTerra (x,y+1) mapa b Sul || caminhoTerra (x,y-1) mapa b Norte else False
caminhoTerra p@(x,y) mapa b Sul = if p == b then True else if rodeadoTerra p mapa && eTerra p mapa then caminhoTerra (x-1,y) mapa b Este || caminhoTerra (x+1,y) mapa b Oeste || caminhoTerra (x,y-1) mapa b Norte else False
caminhoTerra p@(x,y) mapa b Oeste = if p == b then True else if rodeadoTerra p mapa && eTerra p mapa then caminhoTerra (x-1,y) mapa b Este || caminhoTerra (x,y+1) mapa b Sul || caminhoTerra (x,y-1) mapa b Norte else False

rodeadoTerra :: Posicao -> Mapa -> Bool
rodeadoTerra (x,y) mapa@(h:t) = (length(mapa) > floor y) && (eTerra (x,y+1) mapa) ||
                                (y > 0) && (eTerra (x,y-1) mapa) ||
                                ((length(h) > floor x) && (eTerra (x+1,y) mapa)) ||
                                (x > 0) && (eTerra (x-1,y) mapa)

eTerra :: Posicao -> Mapa -> Bool
eTerra (x,y) mapa = if x < 0 || y < 0 then False
                    else (mapa !! floor y !! floor x) == Terra

-- | Verifica se os portais não estão sobrepostos à base
validaPortaisBase :: [Portal] -> Posicao -> Bool
validaPortaisBase [] _ = True
validaPortaisBase (Portal{posicaoPortal = (p1,p2)} : ps) (b1, b2) | p1 == b1 && p2 == b2 = False
                                                                  | otherwise = validaPortaisBase ps (b1, b2)
-- | Verifica que as torres não estão sobrepostas à base
validaTorreBase :: [Torre] -> Posicao -> Bool
validaTorreBase [] _ = True
validaTorreBase (Torre{posicaoTorre = (t1,t2)} : ts) (b1, b2) | t1 == b1 && t2 == b2 = False
                                                              | otherwise = validaTorreBase ts (b1, b2)
-- | Verifica que as torres não estão sobrepostas aos portais
validaTorrePortal :: [Torre] -> [Portal] -> Bool
validaTorrePortal _ [] = True
validaTorrePortal t (Portal{posicaoPortal = p} : ps) | validaTorrePortalAux t p = validaTorrePortal t ps
                                                     | otherwise = False
-- | Função auxiliar que verifica que as torres não estão sobrepostas a um portal apenas
validaTorrePortalAux :: [Torre] -> Posicao -> Bool
validaTorrePortalAux [] _ = True
validaTorrePortalAux (Torre{posicaoTorre = (t1,t2)} : ts) (p1,p2) | t1 == p1 && t2 == p2 = False
                                                                  | otherwise = validaTorrePortalAux ts (p1,p2) 

-- | Verifica que não existe mais de uma onda ativa por portal
validaOndasAtivas :: [Portal] -> Bool
validaOndasAtivas [] = True
validaOndasAtivas (p:ps) | validaOndasAtivasAux p = validaOndasAtivas ps
                         | otherwise = False

-- | Função auxiliar da função validaOndasAtivas que verifica a mesma condição para cada portal individualmente
validaOndasAtivasAux :: Portal -> Bool
validaOndasAtivasAux Portal{ondasPortal = o} = validaOndasAtivasAc o 0
    where
        validaOndasAtivasAc :: [Onda] -> Int -> Bool
        validaOndasAtivasAc [] 2 = False
        validaOndasAtivasAc [] _ = True
        validaOndasAtivasAc (Onda{entradaOnda = e} : os) ac | ac > 1 = False
                                                            | e == 0 = validaOndasAtivasAc os (ac+1)
                                                            | e /= 0 = validaOndasAtivasAc os ac

-- | Verifica que os inimigos por lançar têm a posição do respetivo portal, nível de vida positivo e lista de projeteis ativos vazia
validaInimigosInativos :: [Inimigo] -> [Portal] -> Bool
validaInimigosInativos [] _ = True
validaInimigosInativos (i : is) p | validaInimigosInativosAux i p = validaInimigosInativos is p
                                  | otherwise = False

-- | Auxiliar da função validaInimigosInativos que verifica para cada inimigo se as condições anteriores estão reunidas
validaInimigosInativosAux :: Inimigo -> [Portal] -> Bool
validaInimigosInativosAux i [] = True
validaInimigosInativosAux i (Portal{posicaoPortal = p, ondasPortal = o} : xs) | validaInimigosOnda i o && (inimigoParaPosicao i) /= p = False
                                                                              | validaInimigosOnda i o && validaVidaProjeteis i == False = False
                                                                              | validaInimigosOnda i o && (inimigoParaPosicao i) == p = True
                                                                              | validaInimigosOnda i o && validaVidaProjeteis i = True
                                                                              | otherwise = validaInimigosInativosAux i xs

-- | Verifica se um enimigo pertence a uma onda (utilizada na função validaInimigosInativos)
validaInimigosOnda :: Inimigo -> [Onda] -> Bool
validaInimigosOnda i [] = False
validaInimigosOnda i (Onda{inimigosOnda = l} : xs) | elem i l = True
                                                   | otherwise = validaInimigosOnda i xs

-- | Recebe um inimigo e retorna a sua posição (facilita a escrita da função validaInimigosInativosAux)
inimigoParaPosicao :: Inimigo -> Posicao
inimigoParaPosicao (Inimigo{posicaoInimigo = x}) = x

-- | Verifica se a vida do inimigo é positiva e se a lista de projeteis ativos se encontra vazia (utilizada como auxiliar da função validaInimigosInativosAux)
validaVidaProjeteis :: Inimigo -> Bool
validaVidaProjeteis (Inimigo{vidaInimigo = v, projeteisInimigo = l}) | v >= 0 && length l == 0 = True
                                                                     | otherwise = False
-- | Verifica que todos os inimigos se encontram sobre terra
validaInimigoTerra :: [Inimigo] -> Mapa -> Bool
validaInimigoTerra [] _ = True
validaInimigoTerra ((Inimigo{posicaoInimigo = (i1,i2)}):xs) mapa | (mapa !! floor i2 !! floor i1) == Terra = validaInimigoTerra xs mapa
                                                                 | otherwise = False
-- | Verifica que as torres não estão sobrepostas aos inimigos
validaTorreInimigo :: [Torre] -> [Inimigo] -> Bool
validaTorreInimigo t [] = True
validaTorreInimigo t ((Inimigo{posicaoInimigo = (p1,p2)}) : ps) | validaTorreInimigoAux t (p1,p2) = validaTorreInimigo t ps
                                                                | otherwise = False
-- | Auxliar da função validaTorreInimigo que verifica para cada inimigo se está sobreposto a alguma torre
validaTorreInimigoAux :: [Torre] -> Posicao -> Bool
validaTorreInimigoAux [] _ = True
validaTorreInimigoAux ((Torre{posicaoTorre = (t1,t2)}) : xs) (x,y) | t1 == x && t2 == y = False
                                                                   | otherwise = validaTorreInimigoAux xs (x,y)
-- | Verifica que a velocidade é posotiva para todos os inimigos
validaInimigosVelocidade :: [Inimigo] -> Bool
validaInimigosVelocidade [] = True
validaInimigosVelocidade ((Inimigo{velocidadeInimigo = x}) : xs) | x < 0 = False
                                                                 | otherwise = validaInimigosVelocidade xs

-- | Verifica que não existem dois ou mais projéteis do mesmo tipo em cada inimigo e verifica se não existe nenhuma combinação de Fogo e Resina ou Fogo e Gelo
validaProjeteisInimigos :: [Inimigo] -> Bool
validaProjeteisInimigos [] = True
validaProjeteisInimigos (Inimigo{projeteisInimigo = p} : xs) | validaProjeteisInimigosAux p = validaProjeteisInimigos xs
                                                             | otherwise = False
-- | Função auxiliar da função validaProjeteisInimigos que verifica para cada inimigo se as condições anteriormente descritas estão reunidas
validaProjeteisInimigosAux :: [Projetil] -> Bool
validaProjeteisInimigosAux [] = True
validaProjeteisInimigosAux p@(p1:ps) | elem x xs = False
                                     | x == Fogo && elem Resina xs = False
                                     | x == Fogo && elem Gelo xs = False 
                                     | otherwise = validaProjeteisInimigosAux ps
        where (x:xs) = projeteisInimigoConvertido p

-- | Converte uma lista de projéteis em uma lista com apenas o tipo dos projéteis, esta função ajuda na função validaProjeteisInimigosAux
projeteisInimigoConvertido :: [Projetil] -> [TipoProjetil]
projeteisInimigoConvertido [] = []
projeteisInimigoConvertido (Projetil{tipoProjetil = x} : xs) = x : projeteisInimigoConvertido xs

-- | Verifica que todas as torres estão colocadas sobre relva
validaTorreRelva :: [Torre] -> Mapa -> Bool
validaTorreRelva [] _ = True
validaTorreRelva (Torre{posicaoTorre = (x,y)} : xs) m | (mapa !! floor y !! floor x) == Terra = False
                                                      | otherwise = validaTorreRelva xs m

-- | Verifica que o alcance das torres é um valor positivo
validaAlcanceTorre :: [Torre] -> Bool
validaAlcanceTorre [] = True
validaAlcanceTorre (Torre{alcanceTorre = x} : xs) | x < 0 = False
                                                  | otherwise = validaAlcanceTorre xs                                                      

-- | Verifica que a rajada das torres é um valor positivo
validaRajadaTorre :: [Torre] -> Bool
validaRajadaTorre [] = True
validaRajadaTorre (Torre{rajadaTorre = x} : xs) | x < 0 = False
                                                | otherwise = validaRajadaTorre xs    

-- | Verifica que o ciclo das torres é um valor maior que 0
validaCicloTorre :: [Torre] -> Bool
validaCicloTorre [] = True
validaCicloTorre (Torre{cicloTorre = x} : xs) | x <= 0 = False
                                              | otherwise = validaCicloTorre xs
-- | Verifica que nenhuma torre se sobrepõe a outra
validaTorresSobrepostas :: [Torre] -> Bool
validaTorresSobrepostas l = validaTorresSobrepostasAc (validaTorresSobrepostasAux l) []
    where 
        validaTorresSobrepostasAc :: [Posicao] -> [Posicao] -> Bool
        validaTorresSobrepostasAc [] _ = True
        validaTorresSobrepostasAc (x:xs) l | elem x l = False
                                           | otherwise = validaTorresSobrepostasAc xs (x : l)

-- | Função auxiliar da função validaTorresSobrepostas que converte uma lista de torres numa lisa das suas respetivas posições
validaTorresSobrepostasAux :: [Torre] -> [Posicao]
validaTorresSobrepostasAux [] = []
validaTorresSobrepostasAux (Torre{posicaoTorre = p} : xs) = p : validaTorresSobrepostasAux xs

-- | Verifica que a base se encontra sobre terra
validaBaseTerra :: Posicao -> Mapa -> Bool
validaBaseTerra (x,y) m | (mapa !! floor y !! floor x) == Terra = True
                        | otherwise = False

-- | Verifica que os creditos da base são um valor positivo
validaCreditoBase :: Creditos -> Bool
validaCreditoBase x | x < 0 = False
                    | otherwise = True

mapa :: Mapa
mapa =
    [ [t, t, r, a, a, a],
      [r, t, r, a, r, r],
      [r, t, r, a, r, t],
      [r, t, r, a, r, t],
      [r, t, t, t, t, t],
      [a, a, a, a, r, r]
    ]
 where
    t = Terra
    r = Relva
    a = Agua

{-
jogo :: Jogo
jogo = Jogo {
    baseJogo = Base {
        vidaBase = 150,
        posicaoBase = (5.0, 2.0),
        creditosBase = 100
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.0, 0.0),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (4.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 80,
                            velocidadeInimigo = 1.2,
                            ataqueInimigo = 8,
                            butimInimigo = 15,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (5.0, 3.0),
                            direcaoInimigo = Sul,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1.5,
                            ataqueInimigo = 10,
                            butimInimigo = 20,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 3.0,
                    tempoOnda = 3.0,
                    entradaOnda = 10.0
                }
            ]
        }
    ],
    torresJogo = [
        Torre {
            posicaoTorre = (4.0, 3.0),
            danoTorre = 30,
            alcanceTorre = 5.0,
            rajadaTorre = 2,
            cicloTorre = 1.0,
            tempoTorre = 1.0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 4.0
            }
        },
        Torre {
            posicaoTorre = (2.0, 3.0),
            danoTorre = 60,
            alcanceTorre = 8.0,
            rajadaTorre = 3,
            cicloTorre = 1.2,
            tempoTorre = 0.5,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 6.0
            }
        }
    ],
    mapaJogo = mapa,
    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (1.0, 1.0),
            direcaoInimigo = Sul,
            vidaInimigo = 140,
            velocidadeInimigo = 2.0,
            ataqueInimigo = 20,
            butimInimigo = 25,
            projeteisInimigo = []
        },
        Inimigo {
            posicaoInimigo = (1.0, 2.0),
            direcaoInimigo = Este,
            vidaInimigo = 120,
            velocidadeInimigo = 1.8,
            ataqueInimigo = 18,
            butimInimigo = 22,
            projeteisInimigo = []
        }
    ],
    lojaJogo = [
        (80, Torre {
            posicaoTorre = (0.0, 0.0),
            danoTorre = 40,
            alcanceTorre = 4.0,
            rajadaTorre = 2,
            cicloTorre = 1.5,
            tempoTorre = 0.5,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 3.0
            }
        }),
        (120, Torre {
            posicaoTorre = (9.0, 9.0),
            danoTorre = 80,
            alcanceTorre = 10.0,
            rajadaTorre = 1,
            cicloTorre = 2.0,
            tempoTorre = 1.0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 4.0
            }
        })
    ]
}



portais1 = 
   [Portal {
            posicaoPortal = (0.0, 0.0),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (4.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 80,
                            velocidadeInimigo = 1.2,
                            ataqueInimigo = 8,
                            butimInimigo = 15,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (5.0, 3.0),
                            direcaoInimigo = Sul,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1.5,
                            ataqueInimigo = 10,
                            butimInimigo = 20,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 3.0,
                    tempoOnda = 3.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (4.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 80,
                            velocidadeInimigo = 1.2,
                            ataqueInimigo = 8,
                            butimInimigo = 15,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (5.0, 3.0),
                            direcaoInimigo = Sul,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1.5,
                            ataqueInimigo = 10,
                            butimInimigo = 20,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 3.0,
                    tempoOnda = 3.0,
                    entradaOnda = 0.0
                }
            ]
        }
   ]

torres1 = [
        Torre {
            posicaoTorre = (4.0, 3.0),
            danoTorre = 30,
            alcanceTorre = 5.0,
            rajadaTorre = 2,
            cicloTorre = 1.0,
            tempoTorre = 1.0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 4.0
            }
        },
        Torre {
            posicaoTorre = (2.0, 3.0),
            danoTorre = 60,
            alcanceTorre = 8.0,
            rajadaTorre = 3,
            cicloTorre = 1.2,
            tempoTorre = 0.5,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 6.0
            }
        }
    ]

inimigos1 = [
        Inimigo {
            posicaoInimigo = (1.0, 1.0),
            direcaoInimigo = Sul,
            vidaInimigo = 140,
            velocidadeInimigo = 2.0,
            ataqueInimigo = 20,
            butimInimigo = 25,
            projeteisInimigo = [
                Projetil {
	                tipoProjetil = Gelo,
	                duracaoProjetil = Infinita
                },
                Projetil {
	                tipoProjetil = Resina,
	                duracaoProjetil = Infinita
                }
            ]
        },
        Inimigo {
            posicaoInimigo = (2.0, 2.0),
            direcaoInimigo = Este,
            vidaInimigo = 120,
            velocidadeInimigo = 1.8,
            ataqueInimigo = 18,
            butimInimigo = 22,
            projeteisInimigo = []
        }
    ]

base1 = Base {
            vidaBase = 150,
            posicaoBase = (5.0, 2.0),
            creditosBase = 100
    }

portal1 =
    Portal {
                posicaoPortal = (0.0, 0.0),
                ondasPortal = [
                    Onda {
                        inimigosOnda = [
                            Inimigo {
                                posicaoInimigo = (4.0, 3.0),
                                direcaoInimigo = Norte,
                                vidaInimigo = 80,
                                velocidadeInimigo = 1.2,
                                ataqueInimigo = 8,
                                butimInimigo = 15,
                                projeteisInimigo = []
                            }
                        ]
                    }
                ]
    }
                            

torre1 = 
    Torre {
            posicaoTorre = (4.0, 3.0),
            danoTorre = 30,
            alcanceTorre = 5.0,
            rajadaTorre = 1,
            cicloTorre = 1.0,
            tempoTorre = 1.0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 4.0
            }
    }
    -}