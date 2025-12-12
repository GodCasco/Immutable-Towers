{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Vasco Marques Machado <a109949@alunos.uminho.pt>
              Duarte Emanuel Soares Arruda <a109840@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa1
import Tarefa2

-- | Aplica todas as outras funções responsaveis pelo desenvolvimento do jogo
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t jogo@(Jogo{torresJogo = torres, inimigosJogo = inimigos, mapaJogo = mapa, portaisJogo = portais, baseJogo = base}) = 
        jogo{torresJogo = torresFinais, inimigosJogo = inimigosFinais, portaisJogo = portaisFinais, baseJogo = baseFinal}
                where (torresFinais, inimigosAtualizados1) = atualizaTorres t torres (aplicaFogoInimigos inimigos)
                      (inimigosAtualizados2, baseFinal) = atualizaBase inimigosAtualizados1 base
                      (inimigosAtualizados3) = atualizaInimigos t inimigosAtualizados2 mapa
                      (portaisFinais, inimigosFinais) = atualizaPortais t portais inimigosAtualizados3

---------------------------------------------------------------------------------------------------------------------------------------------------
-- | Responsavel por atualizar o tempo de espera da torre e aplicar o dano nos inimigos
atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaTorres _ [] i = ([],i)
atualizaTorres t (tr : ts) i = 
        let (torreAtualizada, inimigosAtualizados) = atualizaTorre t tr i 
            (torresRestantes, inimigosFinais) = atualizaTorres t ts inimigosAtualizados
        in (torreAtualizada : torresRestantes, inimigosFinais)

-- | Função auxiliar da atualiza torres que funciona para cada torre individualmente
atualizaTorre :: Tempo -> Torre -> [Inimigo] -> (Torre, [Inimigo])
atualizaTorre t torre@(Torre{cicloTorre = ct, tempoTorre = tt, rajadaTorre = rt, projetilTorre = pt}) inimigos
        | tt > 0 = (torre{tempoTorre = tt - t}, inimigos)
        | length (inimigosNoAlcance torre inimigos) == 0 = (torre, inimigos)
        | otherwise = (torre{tempoTorre = ct}, ia ++ ii ++ ifa)
                where (ia, ii) = rajadaInimigos torre (inimigosNoAlcance torre inimigos)
                      ifa = inimigosForaDoAlcance torre inimigos

-- | Determina quantos inimigos é que a torre pode atacar de acordo com a rajada da torre
rajadaInimigos :: Torre -> [Inimigo] -> ([Inimigo], [Inimigo])
rajadaInimigos t i = rajadaInimigosAc t i [] 1
        where 
          rajadaInimigosAc :: Torre -> [Inimigo] -> [Inimigo] -> Int -> ([Inimigo], [Inimigo])
          rajadaInimigosAc _ [] l _ = (l, [])
          rajadaInimigosAc torre@(Torre{danoTorre = dt, rajadaTorre = rt, projetilTorre = pt}) (inimigo : ic) ia ac 
                | ac == rt = ((ia ++ [atingeInimigo torre inimigo]), ic)
                | ac < rt = rajadaInimigosAc torre ic (ia ++ [atingeInimigo torre inimigo]) (ac+1)

inimigosForaDoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosForaDoAlcance _ [] = []
inimigosForaDoAlcance t (x:xs) = if distancia (posicaoTorre t) (posicaoInimigo x) > (alcanceTorre t) then x:inimigosForaDoAlcance t xs
                            else inimigosForaDoAlcance t xs
---------------------------------------------------------------------------------------------------------------------------------------------------
-- | Responsavel pela movimentação dos inimigos, pela aplicação dos efeitos dos projeteis neles contidos e atualiza o tempo desses mesmos projeteis
atualizaInimigos :: Tempo -> [Inimigo] -> Mapa -> [Inimigo]
atualizaInimigos t inimigos mapa = removeInimigos(atualizaTempoEfeito t (moveInimigos t inimigos mapa))
        where 
-- | Deteta quando a vida do inimigo acaba e remove o inimigo do jogo
removeInimigos :: [Inimigo] -> [Inimigo]
removeInimigos [] = []
removeInimigos (inimigo@(Inimigo{vidaInimigo = vi}) : is) | vi <= 0 = removeInimigos is
                                                          | otherwise = inimigo : removeInimigos is

-- | Atualiza o tempo restante do efeito dos projeteis nos inimigos
atualizaTempoEfeito :: Tempo -> [Inimigo] -> [Inimigo]
atualizaTempoEfeito _ [] = []
atualizaTempoEfeito t (i:is) = atualizaTempoEfeitoAux t i : atualizaTempoEfeito t is

-- | Função auxiliar da atualizaTempoEfeito que funciona para cada inimigo individualmente
atualizaTempoEfeitoAux :: Tempo -> Inimigo -> Inimigo
atualizaTempoEfeitoAux t inimigo@(Inimigo{projeteisInimigo = projeteis}) = 
        let listaDeProjeteisAtualizada = removeProjeteis(map (\p -> p {duracaoProjetil = case  duracaoProjetil p of
                                                                                               Finita tp -> Finita (max 0 (tp - t))
                                                                                               Infinita -> Infinita}) projeteis)
        in (inimigo{projeteisInimigo = listaDeProjeteisAtualizada})

-- | Verifica se o projetil ainda está ativo
naoExpirou :: Projetil -> Bool
naoExpirou projetil = case duracaoProjetil projetil of
                         Infinita -> True
                         Finita t -> if t <= 0 then False else True

-- | Remove os projeteis que já não estão atutivos
removeProjeteis :: [Projetil] -> [Projetil]
removeProjeteis projeteis = filter (\p -> naoExpirou p) projeteis

-- | Aplica o efeito dos projeteis de fogo aos inimigos
aplicaFogoInimigos :: [Inimigo] -> [Inimigo]
aplicaFogoInimigos inimigos = map aplicaFogoInimigo inimigos

-- | Função auxiliar da aplicaFogoInimigos que funciona para cada inimigo individualmente
aplicaFogoInimigo  :: Inimigo -> Inimigo
aplicaFogoInimigo  inimigo = foldl aplicaFogoInimigoAux  inimigo listaEfeitos
        where listaEfeitos = projeteisInimigoConvertido (projeteisInimigo inimigo)

-- | Função auxiliar da aplicaFogoInimigo que deteta os projeteis de fogo provocam o dano 
aplicaFogoInimigoAux :: Inimigo -> TipoProjetil -> Inimigo
aplicaFogoInimigoAux (inimigo@(Inimigo{vidaInimigo = vi,velocidadeInimigo = vci})) efeito
        | efeito == Fogo = inimigo{vidaInimigo = (max 0 (vi - 0.05))} 
        | otherwise = inimigo

-- | Responsavel pela movimentação dos inimigos
moveInimigos :: Tempo -> [Inimigo] -> Mapa -> [Inimigo]
moveInimigos t inimigos mapa = map (\inimigo -> defineDirecao t inimigo mapa) inimigos

-- | Verifica se o inimigo tem projeteis de gelo
verificaInimigosGelo :: [Projetil] -> Bool
verificaInimigosGelo [] = False
verificaInimigosGelo (projetil : ps) | tipoProjetil projetil == Gelo = True
                                     | otherwise = verificaInimigosGelo ps
-- | Verifica se o inimigo tem projeteis de resina
verificaInimigosResina :: [Projetil] -> Bool
verificaInimigosResina [] = False
verificaInimigosResina (projetil : ps) | tipoProjetil projetil == Resina = True
                                       | otherwise = verificaInimigosResina ps

-- | Define a direção do inimigo em função do tempo e desloca-o
defineDirecao :: Tempo -> Inimigo -> Mapa -> Inimigo
defineDirecao t inimigo@(Inimigo{posicaoInimigo = (x,y), velocidadeInimigo = vi}) mapa = 
        if verificaInimigosGelo (projeteisInimigo inimigo) then inimigo 
        else case direcaoInimigo inimigo of
                             Norte -> if verificaDirecao inimigo mapa then inimigo{posicaoInimigo = (x, y + fr*vi*t)} 
                                      else if verificaDirecao inimigo {direcaoInimigo = Este} mapa then inimigo {direcaoInimigo = Este} else inimigo {direcaoInimigo = Oeste}
                             Sul -> if verificaDirecao inimigo mapa then inimigo{posicaoInimigo = (x, y - fr*vi*t)} 
                                    else if verificaDirecao inimigo {direcaoInimigo = Oeste} mapa then inimigo {direcaoInimigo = Oeste} else inimigo {direcaoInimigo = Este}
                             Este -> if verificaDirecao inimigo mapa then inimigo{posicaoInimigo = (x + fr*vi*t, y)} 
                                     else if verificaDirecao inimigo {direcaoInimigo = Sul} mapa then inimigo {direcaoInimigo = Sul} else inimigo {direcaoInimigo = Norte}
                             Oeste -> if verificaDirecao inimigo mapa then inimigo{posicaoInimigo = (x - fr*vi*t, y)} 
                                      else if verificaDirecao inimigo {direcaoInimigo = Norte} mapa then inimigo {direcaoInimigo = Norte} else inimigo {direcaoInimigo = Sul}
                         where fr = if verificaInimigosResina (projeteisInimigo inimigo) then 0.3 else 1
                                                  
-- | Deteta se o inimigo deve continuar a andar na mesma direção
verificaDirecao :: Inimigo -> Mapa -> Bool
verificaDirecao inimigo@(Inimigo{posicaoInimigo = (x,y)}) mapa
    | direcaoInimigo inimigo == Norte && validaIndice mapa (floor x) (floor (y + 0.55)) = (mapa !! (floor (y + 0.55)) !! floor x) == Terra
    | direcaoInimigo inimigo == Sul && validaIndice mapa (floor x) (floor (y - 0.55)) = (mapa !! (floor (y - 0.55)) !! floor x) == Terra
    | direcaoInimigo inimigo == Este && validaIndice mapa (floor (x + 0.55)) (floor y) = (mapa !! floor y !! (floor (x + 0.55))) == Terra
    | direcaoInimigo inimigo == Oeste && validaIndice mapa (floor (x - 0.55)) (floor y) = (mapa !! floor y !! (floor (x - 0.55))) == Terra
    | otherwise = False


validaIndice :: Mapa -> Int -> Int -> Bool
validaIndice mapa x y = y >= 0 && y < length mapa && x >= 0 && x < length (head mapa)



---------------------------------------------------------------------------------------------------------------------------------------------------
-- | Coloca os inimigos prontos a ser lançados na lista de inimigos do jogo e retira-os do portal
atualizaPortais :: Tempo -> [Portal] -> [Inimigo] -> ([Portal], [Inimigo])
atualizaPortais t portais inimigos = 
        foldl atualizaPortal ([], inimigos) portais
  where
    atualizaPortal :: ([Portal], [Inimigo]) -> Portal -> ([Portal], [Inimigo])
    atualizaPortal (portaisAtualizados, inimigos) portal =
        let (portalAtualizado, novosInimigos) = atualizaOndas t portal inimigos
        in (portaisAtualizados ++ [portalAtualizado], novosInimigos)

-- | Função auxiliar atualizaPortais que funciona para cada portal individualmente
atualizaOndas :: Tempo -> Portal -> [Inimigo] -> (Portal, [Inimigo])
atualizaOndas t portal inimigos =
    let ondasAtualizadas = atualizaOndasCondicionais t (ondasPortal portal)
        portalAtualizado = portal {ondasPortal = ondasAtualizadas}
        (portalFinal, inimigosNovos) = ativaInimigo portalAtualizado inimigos
    in (portalFinal, inimigosNovos)

atualizaOndasCondicionais :: Tempo -> [Onda] -> [Onda]
atualizaOndasCondicionais _ [] = []
atualizaOndasCondicionais t (onda : restantes)
    | not (null (inimigosOnda onda)) = atualizaEntradaOnda t onda : restantes
    | otherwise = onda : atualizaOndasCondicionais t restantes
  where
    atualizaEntradaOnda :: Tempo -> Onda -> Onda
    atualizaEntradaOnda t o
        | entradaOnda o == 0 = o {entradaOnda = cicloOnda o}
        | otherwise = o {entradaOnda = max 0 (entradaOnda o - t)}



---------------------------------------------------------------------------------------------------------------------------------------------------
-- | Deteta quando um inimigo atinge a base e aplica o dano esperado
atualizaBase :: [Inimigo] -> Base -> ([Inimigo], Base)
atualizaBase inimigos base =
    foldl atualiza ([], adicionaButim inimigos base) inimigos
  where
    atualiza :: ([Inimigo], Base) -> Inimigo -> ([Inimigo], Base)
    atualiza (inimigosAtualizados, baseAtual) inimigo =
        let (inimigoAtualizado, baseAtualizada) = inimigoBase inimigo baseAtual
        in (inimigosAtualizados ++ [inimigoAtualizado], baseAtualizada)

-- | Função auxiliar da atualizaBase que funciona para cada inimigo individualmente
inimigoBase :: Inimigo -> Base -> (Inimigo, Base)
inimigoBase inimigo@(Inimigo{ataqueInimigo = ai}) base@(Base{vidaBase = vb}) 
        | floor xi == floor xb && floor yi == floor yb = (inimigo{vidaInimigo = 0}, base{vidaBase = (vb - ai)})
        | otherwise = (inimigo, base)
        where (xi, yi) = posicaoInimigo inimigo
              (xb, yb) = posicaoBase base

adicionaButim :: [Inimigo] -> Base -> Base
adicionaButim [] base = base 
adicionaButim (inimigo : is) base@(Base{creditosBase = cb}) | vidaInimigo inimigo <= 0 = base{creditosBase = cb + (butimInimigo inimigo)}
                                                            | otherwise = adicionaButim is base