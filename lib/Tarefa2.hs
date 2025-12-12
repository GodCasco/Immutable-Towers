{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Vasco Marques Machado <a109949@alunos.uminho.pt>
              Duarte Emanuel Soares Arruda <a109840@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where
import LI12425

-- | A funcao vai calcular a distancia de varios inimigos que estao no alcance de uma torre e vai entregar uma lista com todos os inimigos dentro do alcance
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance t (x:xs) = if distancia (posicaoTorre t) (posicaoInimigo x) <= (alcanceTorre t) then x:inimigosNoAlcance t xs
                            else inimigosNoAlcance t xs
    
-- | calcula a distancia entre duas posicoes                          
distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


-- | A função atinge um inimigo com um projétil de uma torre e atualizando sua vida e a sua lista de projéteis ativos
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo t i
                 | distancia (posicaoTorre t) (posicaoInimigo i) > alcanceTorre t = i  -- Inimigo fora do alcance
                 | otherwise = i {vidaInimigo = novaVida, projeteisInimigo = novosProjeteis}
            where
                novaVida = max 0 (vidaInimigo i - danoTorre t)  --  atualiza a vida do inimigo
                novosProjeteis = atualizaProjeteis (projetilTorre t) (projeteisInimigo i)  -- atualiza os projeteis

-- | Atualiza a lista de projéteis presentes no inimigo com base nas sinergias
atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis p [] = [p]  -- vai adicionar o projétil a lista  se não existir projeteis ativos
atualizaProjeteis p projeteis
                           | temFogo && temGelo = retiraProjetil Gelo (retiraProjetil Fogo projeteis)  -- Gelo e Fogo vao se anular
                           | temGelo && temFogo = retiraProjetil Fogo (retiraProjetil Gelo projeteis)  -- Fogo e Gelo vao se anular
                           | temFogo && temResina = duplicaDuracao Fogo (retiraProjetil Resina projeteis)  -- Fogo e Resina vai duplicar a duração do Fogo
                           | temResina && temFogo = duplicaDuracao Fogo (retiraProjetil Resina projeteis)  -- Fogo e Resina vai duplicar a duração do Fogo
                           | iguais = somaDuracao p projeteis  -- se forem projéteis iguais, vai somar  as suas durações 
                           | otherwise = p:projeteis -- outros casos como por exemplo : gelo e resina
                         where
                           iguais = elem p projeteis
                           temFogo = elem Fogo (map tipoProjetil projeteis)
                           temGelo = elem Gelo (map tipoProjetil projeteis)
                           temResina = elem Resina (map tipoProjetil projeteis)

-- | retira um certo projétil da lista
retiraProjetil :: TipoProjetil -> [Projetil] -> [Projetil]
retiraProjetil tipo = filter (diferente tipo)
                where
                   diferente t p = tipoProjetil p /= t

-- | duplica a duração de um certo projétil na lista
duplicaDuracao :: TipoProjetil -> [Projetil] -> [Projetil]
duplicaDuracao tipo projeteis = map atualizaDuracao projeteis
                      where
                         atualizaDuracao :: Projetil -> Projetil
                         atualizaDuracao p
                                      | tipoProjetil p == tipo = p {duracaoProjetil = duplica (duracaoProjetil p)}
                                      | otherwise = p

                         duplica :: Duracao -> Duracao
                         duplica (Finita x) = Finita (x * 2)
                         duplica Infinita   = Infinita

-- | Soma a duração de projéteis iguais
somaDuracao :: Projetil -> [Projetil] -> [Projetil]
somaDuracao projetil projeteis
                            | null iguais = projetil : outros
                            | otherwise   = projetil {duracaoProjetil = duracaoTotal} : outros
                      where
                         (iguais, outros) = separaProjeteis projetil projeteis
                         duracaoTotal = somaDuracoes (map duracaoProjetil iguais)

-- | Soma as durações de uma lista de duração
somaDuracoes :: [Duracao] -> Duracao
somaDuracoes = foldr somaDuracao (Finita 0)
        where
          somaDuracao :: Duracao -> Duracao -> Duracao
          somaDuracao (Finita x) (Finita y) = Finita (x + y)
          somaDuracao Infinita _ = Infinita
          somaDuracao _ Infinita = Infinita


-- | Separa os projéteis iguais e diferentes
separaProjeteis :: Projetil -> [Projetil] -> ([Projetil], [Projetil])
separaProjeteis p [] = ([], [])
separaProjeteis p (x:xs)
                     | tipoProjetil x == tipoProjetil p = (x : iguais, outros)
                     | otherwise = (iguais, x : outros)
                where
                  (iguais, outros) = separaProjeteis p xs


-- | A função conta quantos inimigos estão associados a cada portal e retorna o portal e a lista de inimigos associados 
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos
    | null ondasAtivas = (portal, inimigosAtivos) 
    | null inimigosOndaAtual = (portal {ondasPortal = ondasRestantes}, inimigosAtivos)
    | otherwise = 
        ( portal {ondasPortal = novaOnda : ondasRestantes}
        , novoInimigo : inimigosAtivos )
  where
    ondasAtivas = filter ondaPronta (ondasPortal portal)
    ondasRestantes = filter (not . ondaPronta) (ondasPortal portal)

    ondaPronta onda = entradaOnda onda <= 0

    ondaAtual = head ondasAtivas
    inimigosOndaAtual = inimigosOnda ondaAtual

    novoInimigo = head inimigosOndaAtual
    novaOnda = ondaAtual {inimigosOnda = tail inimigosOndaAtual} 


                              
  
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo  || perdeuJogo jogo

-- o jogador ganha se nao tiver mais inimigos
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo =  (null (inimigosJogo jogo)) && portaisVazios(portaisJogo jogo)

portaisVazios :: [Portal] -> Bool
portaisVazios [] = True
portaisVazios (portal : ps) | ondasVazias (ondasPortal portal) = portaisVazios ps
                            | otherwise = False

ondasVazias :: [Onda] -> Bool
ondasVazias [] = True
ondasVazias ondas = False

-- perde se a vida da base for menor ou igual a zero
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0 