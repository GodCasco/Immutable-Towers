module Tempo where

import ImmutableTowers
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it@(ImmutableTowers jogo TelaInicial _ _ _) = it
reageTempo t it@(ImmutableTowers{jogo = j})
    | terminouJogo j = if (ganhouJogo j) then it{menuAtual = GanhouJogo} else it{menuAtual = PerdeuJogo}
    | otherwise = it{jogo = atualizaJogo t j}
reageTempo _ it = it
