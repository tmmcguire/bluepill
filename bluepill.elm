import Mouse
import Window
import Random

-- CONFIG
speed = 500
spawnInterval = 57 / speed
sizePill = 15
sizePlayer = sizePill

(width, height) = (400, 400)
(hWidth, hHeight) = (width / 2, height / 2)

-- HELPER FUNCTIONS
relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2, div h 2)

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x * x + y * y)

vecMulS : Vec -> Time -> Vec
vecMulS (x, y) t = (x * t, y * t)

tf : Float -> Float -> String -> Form
tf y scl str = toText str |> Text.color gray
                          |> centered
                          |> toForm
                          |> scale scl
                          |> move (0, y)

-- INPUT
delta = (fps 30)
input = (,) <~ lift inSeconds delta
             ~ sampleOn delta (lift2 relativeMouse (lift center Window.dimensions) Mouse.position)

rand fn sig = lift fn (Random.float sig)
randX = rand (\r -> (width * r) - hWidth)
randCol = rand (\r -> if r < 0.1 then lightBlue else defaultPill.col)

interval = (every (second * spawnInterval)) 
event = merges [ lift Tick input
                ,lift2 (\x col -> Add (newPill x col)) (randX interval) (randCol interval) 
                ,lift (\_ -> Click) Mouse.clicks ] -- Mouse.isClicked is deprecated

-- MODEL
type Pill = {pos:Vec, vel:Vec, rad:Float, col:Color}

defaultPill = { pos = (0, hHeight)
               ,vel = (0, -speed)
               ,rad = sizePill
               ,col = lightRed }

defaultPlayer = { defaultPill | pos <- (0, -hHeight - sizePlayer)
                              , rad <- sizePlayer
                              , col <- black }

data State = Start | Play | Over
type Game = {player:Pill, pills:[Pill], score:Int, state:State}

defaultGame = { player = defaultPlayer
               ,pills = [] 
               ,score = 0 
               ,state = Start }

newPill : Float -> Color -> Pill
newPill x col = { defaultPill | pos <- (x, hHeight)
                              , col <- col }

-- UPDATE
data Event = Tick (Time, (Int, Int)) | Add Pill | Click

stepPlay : Event -> Game -> Game
stepPlay event g = 
    case event of
        Tick (t, mp) -> let hit pill = (vecLen <| vecSub g.player.pos pill.pos) < g.player.rad + pill.rad
                            unculled = filter (\{pos} -> snd pos > -hHeight) g.pills
                            untouched = filter (not . hit) unculled
                            touched = filter hit unculled
                            hitColor c = not <| isEmpty <| filter (\{col} -> col == c) touched
                            hitBlue = hitColor lightBlue
                            hitRed = hitColor lightRed
                            out = let (x, y) = mp in abs (toFloat x) > hWidth || abs (toFloat y) > hHeight
                            g' = { g | player <- stepPlayer mp g.player
                                     , pills  <- map (stepPill t) untouched 
                                     , score  <- if hitBlue then g.score + 1 else g.score }
                        in  if hitRed || out then { defaultGame | score <-  g'.score
                                                                , state <- Over } else g'
        Add p        -> { g | pills <- p :: g.pills }
        Click        -> g
    

click : Event -> Bool
click event = 
    case event of
        Click -> True
        _     -> False

stepGame : Event -> Game -> Game
stepGame event ({state} as g) = 
    let playGame = { defaultGame | state <- Play }
        toPlay = if click event then playGame else g
    in case state of
        Play  -> stepPlay event g
        _     -> toPlay

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos <| vecMulS p.vel t }


-- DISPLAY
render : (Int, Int) -> Game -> Element
render (w, h) g = 
    let formPill {rad, col, pos} = circle rad |> filled col
                                              |> move pos
        txts = case g.state of
            Start -> [ tf  70 4 "BluePiLL"
                      ,tf   0 2 "Click to Start" ]
            Play  -> [ tf   0 4 (show g.score) ]
            Over  -> [ tf  70 4 "Game Over"
                      ,tf   0 4 (show g.score) 
                      ,tf -50 2 "Click to Restart" ]
        forms = txts ++ (map formPill <| g.player :: g.pills)
    in  color lightGray <| container w h middle
                        <| color white
                        <| collage width height forms


main = render <~ Window.dimensions ~ foldp stepGame defaultGame event
