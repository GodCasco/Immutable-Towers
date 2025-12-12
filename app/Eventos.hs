module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import System.IO.Unsafe (unsafePerformIO)
import Desenhar
import LI12425

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (ImmutableTowers jogo TelaInicial [fundojogoplay,fundojogar] (x,y) _) = ImmutableTowers jogo TelaJogo [fundojogoplay,fundojogar] (x,y) None
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) it@(ImmutableTowers {posicaoTorreSelecionar = (x, y)}) =  if y < 7 then it {posicaoTorreSelecionar = (x, y + 1)} else it {posicaoTorreSelecionar = (x, y)}
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) it@(ImmutableTowers {posicaoTorreSelecionar = (x, y)}) = if y > 0 then it {posicaoTorreSelecionar = (x, y - 1)} else it {posicaoTorreSelecionar = (x, y)}
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) it@(ImmutableTowers {posicaoTorreSelecionar = (x, y)}) = if x < 7 then it {posicaoTorreSelecionar = (x + 1,y)} else it {posicaoTorreSelecionar = (x, y)}
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) it@(ImmutableTowers {posicaoTorreSelecionar = (x, y)}) = if x > 0 then it {posicaoTorreSelecionar = (x - 1, y)} else it {posicaoTorreSelecionar = (x, y)}

reageEventos (EventKey (Char '1') Down _ _) (ImmutableTowers jogo TelaJogo img (x,y) TorreResina) = 
    ImmutableTowers jogo TelaJogo img (x,y) None
reageEventos (EventKey (Char '1') Down _ _) (ImmutableTowers jogo TelaJogo img (x,y) _) =
    ImmutableTowers jogo TelaJogo (img) (x,y) TorreResina
reageEventos (EventKey (Char '2') Down _ _) (ImmutableTowers jogo TelaJogo img (x,y) TorreFogo) = 
    ImmutableTowers jogo TelaJogo img (x,y) None
reageEventos (EventKey (Char '2') Down _ _) (ImmutableTowers jogo TelaJogo img (x,y) _) =
    ImmutableTowers jogo TelaJogo img (x,y) TorreFogo
reageEventos (EventKey (Char '3') Down _ _) (ImmutableTowers jogo TelaJogo img (x,y) TorreGelo) = 
    ImmutableTowers jogo TelaJogo img (x,y) None
reageEventos (EventKey (Char '3') Down _ _) (ImmutableTowers jogo TelaJogo img (x,y) _) =
    ImmutableTowers jogo TelaJogo img (x,y) TorreGelo

reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it@(ImmutableTowers{torreAtual = None}) = it
reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it@(ImmutableTowers jogo@(Jogo{torresJogo = tj, lojaJogo = [(creditos1,torre1),(creditos2,torre2),(creditos3,torre3)], baseJogo = base@(Base{creditosBase = cb})}) TelaJogo img (x,y) ta)
    | cb < creditos1 && ta == TorreFogo = it
    | cb < creditos2 && ta == TorreGelo = it
    | cb < creditos3 && ta == TorreResina = it
    | (mapaJogo jogo) !! floor y !! floor x /= Relva = it 
    | verificaPosicaoTorres x y tj = it 
    | ta == TorreFogo = ImmutableTowers jogo{torresJogo = (tj ++ [torre1{posicaoTorre = (x,y)}]), baseJogo = base {creditosBase = (cb - creditos1)}} TelaJogo img (x, y) None
    | ta == TorreResina = ImmutableTowers jogo{torresJogo = (tj ++ [torre3{posicaoTorre = (x,y)}]), baseJogo = base {creditosBase = (cb - creditos3)}} TelaJogo img (x, y) None
    | ta == TorreGelo = ImmutableTowers jogo{torresJogo = (tj ++ [torre2{posicaoTorre = (x,y)}]), baseJogo = base {creditosBase = (cb - creditos2)}} TelaJogo img (x, y) None
        where 
            verificaPosicaoTorres :: Float -> Float -> [Torre] -> Bool
            verificaPosicaoTorres _ _ [] = False
            verificaPosicaoTorres x y (torre:ts) | posicaoTorre torre == (x,y) = True
                                                 | otherwise = verificaPosicaoTorres x y ts
reageEventos _ it = it

