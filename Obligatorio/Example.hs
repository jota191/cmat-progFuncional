
--modulo para de interfaz grafica

module Main where

import Graphics.UI.WX


data Grid = Grid { hGrid :: [Point],
                   vGrid :: [Point] }


-- main = ?

--La funcion main debe llamar a setup, y luego llamar a start gameFrame




main2 = start $ gameFrame

-- gameframe puede recibir otros parametros si lo consideran adecuado...





gameFrame =
  do
    turn   <- varCreate player1
    -- crea una variable mutable, que representara de quien es el turno,
    -- que en el comienzo vale player1
    
      -- varCreate :: a -> IO (Var a)
      -- crea variables mutables
      -- varGet :: Var a -> IO a
      -- permite acceder al valor en cualquier momento
      -- varUpdate :: Var a -> (a -> a) -> IO a
      -- actualiza la variable pasada en el primer parametro
      -- evaluandola en la funcion pasada en el segundo

      -- Pueden pensar en las variables mutables como una monada State
      -- Es exactamente eso, pero usando la monada y en lugar de get y put
      -- nos proveen varGet y varUpdate

    screen <- varCreate (initGrid dim dim)
    
    f      <- frameFixed [text := "Player 1"]
      -- frame (ventana) para el jugador 1
    p1     <- panel f   [on paint := paintScreen screen player1]
      -- panel (en donde se dibuja) para el jugador 1
      -- 
    set p1 [on click := (\clickcoords ->
                           changeState p1{-pasar mas parametros-})]
      -- en el evento onclick, se llama a una funcion (changeState) pasandole
      -- como parametro el punto dond ese hace click
    set f [layout := column 1 [minsize (sz maxX maxY) (widget p1)]]
    g      <- frameFixed [text := "Player 2"]
    p2     <- panel g    [on paint := paintScreen screen player2]
    set p2 [on click := (\clickcoords ->
                           changeState p2)]
    set g [layout := column 1 [minsize (sz maxX maxY) (widget p2)]]
    return ()
      where
        maxX, maxY :: Int
        maxY   = (*squareSize) dim --((*squareSize) . snd . dimBd) mockupState
        maxX   = (*squareSize) dim --((*squareSize) . fst . dimBd) mockupState


-- | Constantes hardcodeadas

squareSize :: Int
squareSize = 30

-- esta constante no tienen por que modificarla si no quieren,
-- representa el tamanio (en pixeles) del lado de una casilla


dim :: Int
dim = 4
--  esta constante se esta usando en la llamada de initGrid,
--  pero en su lugar se deben utilizar dimensiones de State,
--  por tanto este valor deberia de desaparecer en la version
--  final, esta para que funcione el ejemplo -}
                 
-- | Cosas que ya tienen definidas en otros modulos

type Player = Bool

player1 :: Player
player2 :: Player

player1 = True
player2 = False


-----------------


-- | Otras definiciones del modulo

changeState p
  = do --cosas
       repaint p -- luego de cambiar el estrado hay que repintar! 
       return ()


initGrid :: Int -> Int -> Grid
initGrid  m n=  Grid [Point 0 (squareSize*y) | y <- [0..m]]
                     [Point (squareSize*x) 0 | x <- [0..n]]

         
paintScreen :: Var Grid -> Player -> DC a -> t -> IO ()
paintScreen screen player dc view -- pueden pasar mas parametros (state?)!
  = do screen <- varGet screen
       -- obtengo la variable screen
       set dc [brushColor := red, brushKind := BrushSolid]
       -- antes de dibujar, debe setearse el device context
       -- cuando dibujen otra cosa recuerden hacer esto antes, segun como
       -- quieran que se dibuje.
       -- notese que toda esta linea tiene tipo IO(), y tiene un efecto, que es
       -- cambiar los parametros para dibujar.
       -- Si bien dc no es mutable, se usa como "inidice" de una variable de
       -- estado 
       mapM_ (drawHLine dim dc) (hGrid (screen))
       mapM_ (drawVLine dim dc) (vGrid (screen))
       -- drawBoard (...) 
       -- aca habria que dibujar el contenido del tablero...
       
drawHLine :: Int -> DC a -> Point2 Int -> IO ()
drawHLine dim dc (Point x y)
  = line dc (Point x y) (Point (x+squareSize*dim) y) []

drawVLine :: Int -> DC a -> Point2 Int -> IO ()
drawVLine dim dc (Point x y)
  = line dc (Point x y) (Point x (y+squareSize*dim)) []


-- drawBoard :: DC a -> Board -> IO ()
