module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Gloss (Picture)
import GHC.Base (Float)
import Data.Int (Int)
import LI12425 (Inimigo)
import Tarefa1 (eTerra)
import Tarefa3
import GHC.IO (unsafePerformIO)

desenha :: ImmutableTowers -> Picture
desenha (ImmutableTowers jogo TelaInicial [fundojogoplay, _ ] _ _) = fundojogoplay
desenha (ImmutableTowers jogo GanhouJogo _ _ _) = ganhouJogoFundo
desenha (ImmutableTowers jogo PerdeuJogo _ _ _) = perdeuJogoFundo
desenha (ImmutableTowers jogo TelaJogo img@[ _ ,fundojogar] (x,y) tipoDeTorre) = if tipoDeTorre /= None then pictures (img ++ desenhaMapas mapa ++ [desenhaTorreSelecionada (floor(x)) (floor(y)) mapa (torresJogo jogo) (baseJogo jogo) tipoDeTorre, desenhaVidaBase (baseJogo jogo), desenhaCreditosBase (baseJogo jogo), desenhaInimigosAtivos (todosOsInimigos (portaisJogo jogo) (inimigosJogo jogo))] ++ desenhaTorres (torresJogo jogo) ++ desenhaInimigos (inimigosJogo jogo) ++ desenhaPortais (portaisJogo jogo) ++ [desenhaBase (baseJogo jogo)] ++ [desenhaInfoSelecionado tipoDeTorre])
                                                                                 else pictures ([fundojogar, desenhaVidaBase (baseJogo jogo), desenhaCreditosBase (baseJogo jogo), desenhaInimigosAtivos (todosOsInimigos (portaisJogo jogo) (inimigosJogo jogo))] ++ desenhaMapas mapa ++ desenhaTorres (torresJogo jogo) ++ desenhaInimigos (inimigosJogo jogo) ++ desenhaPortais (portaisJogo jogo) ++ [desenhaBase (baseJogo jogo)])



-- da scale no x
sW :: Float --0.64 --5.12 --3.40
sW = 6.3
--da scale no y
sY :: Float --0.64  --5.12 --3.40
sY = 6.3
--da translate no x
--rT :: Int --36 --33
--rT = 33
-- da translate no y
--rY :: Int --36 --33
--rY = 33

ganhouJogoFundo :: Picture
ganhouJogoFundo = unsafePerformIO $ loadBMP "imagens/jogo/fundodojogo1920vitoria.bmp"

perdeuJogoFundo :: Picture
perdeuJogoFundo = unsafePerformIO $ loadBMP "imagens/jogo/fundodojogo1920Derrota.bmp"

-------------
torreResina :: Picture
torreResina = scale 0.33333333333333 0.33333333333333 $ unsafePerformIO $ loadBMP "imagens/jogo/torreresina.bmp"

torreFogo :: Picture
torreFogo = scale 0.33333333333333 0.33333333333333 $ unsafePerformIO $ loadBMP "imagens/jogo/torrefogo.bmp"

torreGelo :: Picture
torreGelo = scale 0.66666666666667 0.66666666666667 $ unsafePerformIO $ loadBMP "imagens/jogo/torredegelo.bmp"

-------------

torreResinaRed :: Picture
torreResinaRed = scale 0.33333333333333 0.33333333333333 $ unsafePerformIO $ loadBMP "imagens/jogo/redtorreresina.bmp"

torreFogoRed :: Picture
torreFogoRed = scale 0.33333333333333 0.33333333333333 $ unsafePerformIO $ loadBMP "imagens/jogo/redtorrefogo.bmp"

torreGeloRed :: Picture
torreGeloRed = scale 0.66666666666667 0.66666666666667 $ unsafePerformIO $ loadBMP "imagens/jogo/redtorregelo.bmp"

-------------

torreResinaVerde :: Picture
torreResinaVerde = scale 0.33333333333333 0.33333333333333 $ unsafePerformIO $ loadBMP "imagens/jogo/verdetorreresina.bmp"

torreFogoVerde :: Picture
torreFogoVerde = scale 0.33333333333333 0.33333333333333 $ unsafePerformIO $ loadBMP "imagens/jogo/verdetorrefogo.bmp"

torreGeloVerde :: Picture
torreGeloVerde = scale 0.66666666666667 0.66666666666667 $ unsafePerformIO $ loadBMP "imagens/jogo/verdetorregelo.bmp"
-------------

torreResinaInfo :: Picture
torreResinaInfo =  unsafePerformIO $ loadBMP "imagens/jogo/fundoresinainfo.bmp"

torreFogoInfo :: Picture
torreFogoInfo = unsafePerformIO $ loadBMP "imagens/jogo/fundofogoinfo.bmp"

torreGeloInfo :: Picture
torreGeloInfo = unsafePerformIO $ loadBMP "imagens/jogo/fundogeloinfo.bmp"

-------------

inimigoDesenhoN :: Picture
inimigoDesenhoN = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigocostas.bmp"

inimigoDesenhoS :: Picture
inimigoDesenhoS = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigofrente.bmp"

inimigoDesenhoE :: Picture
inimigoDesenhoE = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigoE.bmp"

inimigoDesenhoO :: Picture
inimigoDesenhoO = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigoO.bmp"

-------------

inimigoDesenhoGeloN :: Picture
inimigoDesenhoGeloN = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigogelocostas.bmp"

inimigoDesenhoGeloS :: Picture
inimigoDesenhoGeloS = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigogelofrente.bmp"

inimigoDesenhoGeloE :: Picture
inimigoDesenhoGeloE = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigogeloE.bmp"

inimigoDesenhoGeloO :: Picture
inimigoDesenhoGeloO = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigogeloO.bmp"

-------------

inimigoDesenhoResinaN :: Picture
inimigoDesenhoResinaN = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigoresinacostas.bmp"

inimigoDesenhoResinaS :: Picture
inimigoDesenhoResinaS = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigoresinafrente.bmp"

inimigoDesenhoResinaE :: Picture
inimigoDesenhoResinaE = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigoresinaE.bmp"

inimigoDesenhoResinaO :: Picture
inimigoDesenhoResinaO = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigoresinaO.bmp"

-------------

inimigoDesenhoFogoN :: Picture
inimigoDesenhoFogoN = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigofogocostas.bmp"

inimigoDesenhoFogoS :: Picture
inimigoDesenhoFogoS = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigofogofrente.bmp"

inimigoDesenhoFogoE :: Picture
inimigoDesenhoFogoE = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigofogoE.bmp"

inimigoDesenhoFogoO :: Picture
inimigoDesenhoFogoO = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/inimigo/inimigofogoO.bmp"

-------------

portalDesenho :: Picture
portalDesenho = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/jogo/portalinimigos.bmp"

baseDesenho :: Picture
baseDesenho = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/jogo/basefull.bmp"

baseDanoDesenho :: Picture
baseDanoDesenho = scale 3 3 $ unsafePerformIO $ loadBMP "imagens/jogo/basedano.bmp"

-------------

mapa :: [[Terreno]]
mapa =
    [
      [r,r,r,r,r,r,r,r],
      [r,t,t,t,t,r,r,r],
      [r,t,r,r,t,r,r,r],
      [t,t,r,r,t,r,r,r],
      [r,r,a,a,t,t,r,r],
      [r,a,a,a,a,t,t,t],
      [r,a,a,a,r,r,r,r],
      [r,r,r,r,r,r,r,r]
    ]
   where
      t=Terra
      r=Relva
      a=Agua

-- o loadBMP da IO e a forma mais rapida que vi para trocar para picture foi o unsafePerformIO que da uma Picture 



      
desenhaMapas :: [[Terreno]] -> [Picture]
desenhaMapas mapa = desenhaMapasAc mapa 0
      where desenhaMapasAc :: [[Terreno]] -> Int -> [Picture]
            desenhaMapasAc [] _ = []
            desenhaMapasAc (linha : ls) ac = (desenhaLinhaMapa linha 0 ac) ++ (desenhaMapasAc ls (ac + 1))


desenhaLinhaMapa :: [Terreno] -> Int -> Int -> [Picture]
desenhaLinhaMapa [] _ _ = []
desenhaLinhaMapa (terreno:ts) x y = (scale 8.8 8.8 $ (desenhaTerreno terreno x y)) : desenhaLinhaMapa ts (x + 1) y

desenhaTerreno :: Terreno -> Int -> Int -> Picture
desenhaTerreno terreno x y  =
         case terreno of
            Terra -> translate (14 * (fromIntegral x) + 13) (14 * (fromIntegral y) - 43) $ unsafePerformIO $ loadBMP "imagens/mapa/soterra.bmp"
            Relva -> translate (14 * (fromIntegral x) + 13) (14 * (fromIntegral y) - 43) $ unsafePerformIO $ loadBMP "imagens/mapa/soerva.bmp"
            Agua -> translate (14 * (fromIntegral x) + 13) (14 * (fromIntegral y) - 43) $ unsafePerformIO $ loadBMP "imagens/mapa/soagua.bmp"



desenhaTorres :: [Torre] -> [Picture]
desenhaTorres [] = []
desenhaTorres (t@(Torre{posicaoTorre = (x,y)}):ts) | tipoProjetil (projetilTorre t) == Resina = ((scale 1.4 1.4 $ translate ((fromIntegral (floor (x) * 88)) + 25.17) ((fromIntegral (floor (y) * 88)) - 325) $ torreResina) : desenhaTorres ts)
                                                   | tipoProjetil (projetilTorre t) == Gelo = ((scale 1.4 1.4 $ translate ((fromIntegral (floor (x) * 88)) + 25.17) ((fromIntegral (floor (y) * 88)) - 325) $ torreGelo) : desenhaTorres ts)
                                                   | tipoProjetil (projetilTorre t) == Fogo = ((scale 1.4 1.4  $ translate ((fromIntegral (floor (x) * 88)) + 25.17) ((fromIntegral (floor (y) * 88)) - 325) $ torreFogo) : desenhaTorres ts)




desenhaTorreSelecionada :: Int -> Int -> Mapa -> [Torre] -> Base -> TorreAtual -> Picture
desenhaTorreSelecionada x y mapa torres base ta | ta == None = blank
                               | ta == TorreResina = if mapa !! y !! x /= Relva || elem (fromIntegral(x),fromIntegral(y)) (coordenadasTorresJogo torres) || (creditosBase base) < 60 then scale 1.4 1.4 $ translate ((fromIntegral (x * 88)) + 25.17) ((fromIntegral (y * 88)) - 325) $ torreResinaRed 
                                                     else scale 1.4 1.4 $ translate ((fromIntegral (x * 88)) + 25.17) ((fromIntegral (y * 88)) - 325) $ torreResinaVerde
                               | ta == TorreGelo = if mapa !! y !! x /= Relva || elem (fromIntegral(x),fromIntegral(y)) (coordenadasTorresJogo torres) || (creditosBase base) < 30 then scale 1.4 1.4 $ translate ((fromIntegral (x * 88)) + 25.17) ((fromIntegral (y * 88)) - 325) $ torreGeloRed 
                                                     else scale 1.4 1.4 $ translate ((fromIntegral (x * 88)) + 25.17) ((fromIntegral (y * 88)) - 325) $ torreGeloVerde
                               | ta == TorreFogo = if mapa !! y !! x /= Relva || elem (fromIntegral(x),fromIntegral(y)) (coordenadasTorresJogo torres) || (creditosBase base) < 50 then scale 1.4 1.4 $ translate ((fromIntegral (x * 88)) + 25.17) ((fromIntegral (y * 88)) - 325) $ torreFogoRed 
                                                     else scale 1.4 1.4 $ translate ((fromIntegral (x * 88)) + 25.17) ((fromIntegral (y * 88)) - 325) $ torreFogoVerde

desenhaInfoSelecionado :: TorreAtual -> Picture
desenhaInfoSelecionado ta | ta == None = blank
                          | ta == TorreResina = torreResinaInfo
                          | ta == TorreFogo = torreFogoInfo
                          | ta == TorreGelo = torreGeloInfo
                          

coordenadasTorresJogo :: [Torre] -> [Posicao]
coordenadasTorresJogo [] = []
coordenadasTorresJogo (torre:ts) = posicaoTorre torre : coordenadasTorresJogo ts
---------------------------------------------------------------------------------------------------------------------------------

desenhaInimigos :: [Inimigo] -> [Picture]
desenhaInimigos [] = []
desenhaInimigos (inimigo:is) | verificaInimigosGelo (projeteisInimigo inimigo) = desenhaInimigosGelo inimigo : desenhaInimigos is
                             | verificaInimigosResina (projeteisInimigo inimigo) = desenhaInimigosResina inimigo : desenhaInimigos is
                             | verificaInimigosFogo (projeteisInimigo inimigo) = desenhaInimigosFogo inimigo : desenhaInimigos is
                             | otherwise = desenhaInimigosNormal inimigo : desenhaInimigos is

desenhaInimigosNormal :: Inimigo -> Picture
desenhaInimigosNormal Inimigo{posicaoInimigo = (x,y), direcaoInimigo = dir}
  | dir == Norte = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoN)
  | dir == Sul = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoS)
  | dir == Este = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoE)
  | dir == Oeste = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoO)        

desenhaInimigosGelo :: Inimigo -> Picture
desenhaInimigosGelo Inimigo{posicaoInimigo = (x,y), direcaoInimigo = dir}
  | dir == Norte = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoGeloN)
  | dir == Sul = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoGeloS)
  | dir == Este = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoGeloE)
  | dir == Oeste = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoGeloO)                                                                          

desenhaInimigosResina :: Inimigo -> Picture
desenhaInimigosResina Inimigo{posicaoInimigo = (x,y), direcaoInimigo = dir}
  | dir == Norte = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoResinaN)
  | dir == Sul = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoResinaS)
  | dir == Este = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoResinaE)
  | dir == Oeste = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoResinaO) 

desenhaInimigosFogo :: Inimigo -> Picture
desenhaInimigosFogo Inimigo{posicaoInimigo = (x,y), direcaoInimigo = dir}
  | dir == Norte = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoFogoN)
  | dir == Sul = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoFogoS)
  | dir == Este = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoFogoE)
  | dir == Oeste = (translate (125*x - 25) (125*y - 500) $ inimigoDesenhoFogoO)  

verificaInimigosFogo :: [Projetil] -> Bool
verificaInimigosFogo [] = False
verificaInimigosFogo (projetil : ps) | tipoProjetil projetil == Fogo = True
                                     | otherwise = verificaInimigosFogo ps    

---------------------------------------------------------------------------------------------------------------------------------
desenhaVidaBase :: Base -> Picture
desenhaVidaBase base = translate (-730) 395 $ scale 0.4 0.4 $ color red $ Text (show (round(vidaBase base)))

desenhaCreditosBase :: Base -> Picture
desenhaCreditosBase base = translate (-470) 395 $ scale 0.4 0.4 $ color black $ Text (show (creditosBase base))

desenhaInimigosAtivosAux :: [Inimigo] -> Int
desenhaInimigosAtivosAux [] = 0
desenhaInimigosAtivosAux (inimigo : is) = 1 + desenhaInimigosAtivosAux is

desenhaInimigosAtivos :: [Inimigo] -> Picture
desenhaInimigosAtivos inimigos = translate (-260) (-415) $ scale 0.4 0.4 $ color black $ Text (show (desenhaInimigosAtivosAux inimigos))

todosOsInimigos :: [Portal] -> [Inimigo] -> [Inimigo]
todosOsInimigos [] inimigos = inimigos
todosOsInimigos (portal:ps) inimigos = todosOsInimigosAux (ondasPortal portal) ++ todosOsInimigos ps inimigos

todosOsInimigosAux :: [Onda] -> [Inimigo]
todosOsInimigosAux [] = []
todosOsInimigosAux (onda:os) = (inimigosOnda onda) ++ (todosOsInimigosAux os)

desenhaPortais :: [Portal] -> [Picture]
desenhaPortais [] = []
desenhaPortais (Portal{posicaoPortal = (x,y)}:ps) = (scale 0.5 0.5 $ (translate (125*x + 20) (125*y - 540) $ portalDesenho)) : desenhaPortais ps

desenhaBase :: Base -> Picture
desenhaBase (Base{posicaoBase = (x,y), vidaBase = v}) | v > 75 = (scale 1 1 $ (translate (125*x) (125*y - 450) $ baseDesenho))
                                                      | v <= 75 = (scale 1 1 $ (translate (125*x) (125*y - 450) $ baseDanoDesenho))