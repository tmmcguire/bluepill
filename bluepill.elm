import Mouse
import Window
import Random
import Text

-- CONFIG
speed         = 500
spawnInterval = 57 / speed
sizePill      = 15
sizePlayer    = sizePill

(width, height) = (400, 400)
(hWidth, hHeight) = (width / 2, height / 2)

-- HELPER FUNCTIONS
relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x * x + y * y)

vecMulS : Vec -> Time -> Vec
vecMulS (x, y) t = (x * t, y * t)

strToForm : Float -> Float -> String -> Form
strToForm y scl str = toText str |> Text.color gray |> centered |> toForm |> scale scl |> move (0, y)

-- INPUT
data Event = Tick (Time, (Int, Int)) {-- Frame in game play --}
           | Add (Float, Color)      {-- Introduce new pill --}
           | Click                   {-- User mouse click --}

eventStream : Signal Event
eventStream = let delta : Signal Float
                  delta = fps 30

                  input : Signal (Float, (Int,Int))
                  input = (,) <~ lift inSeconds delta
                               ~ sampleOn delta (relativeMouse <~ lift center Window.dimensions ~ Mouse.position)
                  
                  interval : Signal Float
                  interval = (every (second * spawnInterval))
                  
                  rand : (Float -> res) -> Signal a -> Signal res
                  rand fn sig = lift fn (Random.float sig)
                  
                  randX : Signal a -> Signal Float
                  randX = rand (\r -> (width * r) - hWidth)
                  
                  randCol : Signal a -> Signal Color
                  randCol = rand (\r -> if r < 0.1 then lightBlue else defaultPill.col)
        in
          merges [ Tick                     <~ input
                 , (\x col -> Add (x, col)) <~ (randX interval) ~ (randCol interval) 
                 , (\_ -> Click)            <~ Mouse.clicks
                 ]

-- MODEL
type Pill = {pos:Vec, vel:Vec, rad:Float, col:Color}

defaultPill : Pill
defaultPill = { pos = (0, hHeight)
              , vel = (0, -speed)
              , rad = sizePill
              , col = lightRed
              }

newPill : (Float, Color) -> Pill
newPill (x, col) = { defaultPill | pos <- (x, hHeight)
                                 , col <- col
                   }

defaultPlayer : Pill
defaultPlayer = { defaultPill | pos <- (0, -hHeight - sizePlayer)
                              , rad <- sizePlayer
                              , col <- black
                }

data State = Start | Play | Over
type Game = {player:Pill, pills:[Pill], score:Int, state:State}

defaultGame : Game
defaultGame = { player = defaultPlayer
              , pills = [] 
              , score = 0 
              , state = Start
              }

-- UPDATE
stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecMulS p.vel t |> vecAdd p.pos }

stepPlay : Event -> Game -> Game
stepPlay event g = 
    let hit : Pill -> Bool
        hit pill = (vecSub g.player.pos pill.pos |> vecLen) < g.player.rad + pill.rad

        hitColor : Color -> [Pill] -> Bool
        hitColor c pills = filter (\{col} -> col == c) pills |> (not << isEmpty)
    in
      case event of
        Tick (t, mp) -> let (touched, untouched) = filter (\{pos} -> snd pos > -hHeight) g.pills |> partition hit
                            hitBlue              = hitColor lightBlue touched
                            hitRed               = hitColor lightRed touched

                            out                  = let (x, y) = mp in abs (toFloat x) > hWidth || abs (toFloat y) > hHeight

                            g'                   = { g | player <- stepPlayer mp g.player
                                                       , pills  <- map (stepPill t) untouched 
                                                       , score  <- if hitBlue then g.score + 1 else g.score
                                                   }
                        in
                          if hitRed || out
                          then { defaultGame | score <-  g'.score
                                             , state <- Over
                               }
                          else g'
        Add p        -> { g | pills <- newPill p :: g.pills }
        Click        -> g
    
stepGame : Event -> Game -> Game
stepGame event ({state} as g) = case state of
                                  Play  -> stepPlay event g
                                  _     -> case event of Click -> { defaultGame | state <- Play }
                                                         _     -> g


-- DISPLAY
type Formable a = { a | pos:Vec, rad:Float, col:Color}

render : (Int, Int) -> Game -> Element
render (w, h) g = 
    let formPill : Formable a -> Form
        formPill {rad, col, pos} = circle rad |> filled col |> move pos

        txts : [Form]
        txts = case g.state of
            Start -> [ strToForm  70 4 "BluePiLL"
                     , strToForm   0 2 "Click to Start"
                     ]
            Play  -> [ strToForm   0 4 (show g.score) ]
            Over  -> [ strToForm  70 4 "Game Over"
                     , strToForm   0 4 (show g.score) 
                     , strToForm -50 2 "Click to Restart"
                     ]

        forms : [Form]
        forms = txts ++ (g.player :: g.pills |> map formPill)
    in
      collage width height forms
          |> color white
          |> container w h middle
          |> color lightGray

main = render <~ Window.dimensions ~ foldp stepGame defaultGame eventStream
