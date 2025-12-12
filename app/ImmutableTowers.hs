module ImmutableTowers where
import LI12425
import Graphics.Gloss
data ImmutableTowers = ImmutableTowers {
  jogo :: Jogo,
  menuAtual :: Menu,
  imagens :: [Picture],
  posicaoTorreSelecionar :: Posicao,
  torreAtual :: TorreAtual}

data Menu = TelaInicial | TelaJogo | PerdeuJogo | GanhouJogo  deriving (Show, Eq)
data TorreAtual = TorreResina | TorreFogo | TorreGelo | None   deriving (Show, Eq)