module Main where

import LI12425
import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo


janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)
  

fundo :: Color
fundo = white

fr :: Int
fr = 60

main :: IO ()
main = do
  fundojogoplay <- loadBMP "imagens/jogo/fundodojogoplayact1920.bmp"
  fundojogar <- loadBMP "imagens/jogo/fundojogatina.bmp"
  

  let it = ImmutableTowers {jogo = jogo1, menuAtual = TelaInicial, imagens = [fundojogoplay,fundojogar], posicaoTorreSelecionar = (0,0), torreAtual = None}

  play janela fundo fr it desenha reageEventos reageTempo

-- teste de jogo


jogo1 :: Jogo
jogo1 = Jogo {
    baseJogo = Base {
        vidaBase = 150,
        posicaoBase = (7.0, 5.0),
        creditosBase = 100
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.0, 3.0),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1.4,
                            ataqueInimigo = 15,
                            butimInimigo = 20,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1.4,
                            ataqueInimigo = 15,
                            butimInimigo = 20,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1.4,
                            ataqueInimigo = 15,
                            butimInimigo = 20,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 3.0,
                    tempoOnda = 3.0,
                    entradaOnda = 5.0
                },
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 15,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 15,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 15,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 15,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 15,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 2.5,
                    tempoOnda = 2.5,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 400,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
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
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.0, 3.0),
                            direcaoInimigo = Norte,
                            vidaInimigo = 850,
                            velocidadeInimigo = 1.7,
                            ataqueInimigo = 20,
                            butimInimigo = 30,
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
        
    ],
    mapaJogo = mapa,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0.0, 0.0),
            danoTorre = 30,
            alcanceTorre = 2.0,
            rajadaTorre = 2,
            cicloTorre = 1.5,
            tempoTorre = 0.5,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 3.0
            }
        }),
        (30, Torre {
            posicaoTorre = (0.0, 0.0),
            danoTorre = 30,
            alcanceTorre = 2.0,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 0.5,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 3.0
            }
        }),
        (60, Torre {
            posicaoTorre = (0.0, 0.0),
            danoTorre = 20,
            alcanceTorre = 2.0,
            rajadaTorre = 2,
            cicloTorre = 1.7,
            tempoTorre = 0.5,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 4.0
            }
        })
    ]
}

