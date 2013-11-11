import Mouse
import Window
import Random

-- HELPER FUNCTIONS

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

vecMulS : Vec -> Float -> Vec
vecMulS (x, y) s = (x * s, y * s)

vecLen : Vec -> Float
vecLen (x, y) = sqrt <| x * x + y * y

toFloatVec : (Int, Int) -> Vec
toFloatVec (x, y) = (toFloat x, toFloat y)

rndRange : Float -> Float -> Float -> Float
rndRange rnd start end = start + (end - start) * rnd

relativeCenter : (Int, Int) -> (Int, Int) -> Vec
relativeCenter (w, h) (x, y) = ( toFloat x - (toFloat w / 2),
                               -(toFloat y - (toFloat h / 2)))

-- INPUT

type Input = {time:Float, rnd:(Float, Float), lclick:Bool, mpos:Vec}

input : Signal Input
input = let delta = fps 60 in 
         Input <~ lift inSeconds delta
                ~ lift2 (,) (Random.float delta) (Random.float delta)
                ~ Mouse.isClicked 
                ~ sampleOn delta (lift2 relativeCenter Window.dimensions Mouse.position)

-- MODEL

(width, height) = (450, 450)
(hWidth, hHeight) = (225, 225)

type Pill = {pos:Vec, vel:Vec, rad:Float, col:Color}

defaultPill : Pill
defaultPill = { pos = (0, hHeight)
               ,vel = (0, -500)
               ,rad = 15
               ,col = lightRed }
               
player : Pill
player = { defaultPill | pos <- (0, -50)
                       , rad <- 15
                       , col <- black }
                       
data State = Start | Play | Over | Clear

type Game = {accm:Float, plyr:Pill, objs:[Pill], state:State, score:Int}

gameDefault : Game
gameDefault = {accm=0, plyr=player, objs=[], state=Start, score=0}


-- UPDATE

stepPill : Float -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos <| vecMulS p.vel t }

aboveGround : Pill -> Bool
aboveGround {pos} = let (_, y) = pos in y > -hHeight

newPill : (Float, Float) -> Pill
newPill (rp, rc) = { defaultPill | pos <- (rndRange rp -hWidth hWidth, snd defaultPill.pos)
                                 , col <- if rc < 0.1 then lightBlue else defaultPill.col }
               
stepPlayer : Pill -> Vec -> Pill
stepPlayer p mp = { p | pos <- mp }

playerOut : Pill -> Bool
playerOut {pos} = let (x, y) = pos in abs x > hWidth || abs y > hHeight

stepGame : Input -> Game -> Game
stepGame {time, rnd, lclick, mpos} ({accm, plyr, objs, state, score} as game) =
  let moved = map (stepPill time) objs
      culled = filter aboveGround moved
      hit pill = (vecLen <| vecSub plyr.pos pill.pos) < plyr.rad + pill.rad
      touched = filter hit culled
      hitColor c = not <| isEmpty <| filter (\{col} -> col == c) touched
      hitBlue = hitColor lightBlue
      hitRed = hitColor lightRed
      untouched = filter (not . hit) culled
      clickToPlay = if lclick then Play else state
      d = 0.1 in 
        case state of
          Play  -> { game | accm <- if accm > d then 0 else accm + time
                          , plyr <- stepPlayer plyr mpos
                          , objs <- if | accm > d -> newPill rnd :: untouched
                                       | otherwise -> untouched
                          , state <- if hitRed || playerOut plyr then Over else state 
                          , score <- if hitBlue then score + 1 else score }
          Start -> { gameDefault | state <- if lclick then Play else state }
          Clear -> { gameDefault | state <- Play }
          Over  -> { gameDefault | state <- if lclick then Clear else state
                                 , score <- score }


-- DISPLAY

tf : Float -> Float -> String -> Form
tf y scl str = move (0, y) <| scale scl <| toForm <| text <| Text.color gray
                                                          <| toText str

render : (Int, Int) -> Game -> Element
render (w, h) {plyr, objs, state, score} = 
  let formPill {rad, col, pos} = circle rad |> filled col |> move pos
      message = case state of 
        Start -> [ tf 40 3 "BluepiLL"
                  ,tf  0 1 "Click to Start" ]
        Over  -> [ tf 70 3 "Game Over"
                  ,tf 30 2 <| "Score: " ++ show score
                  ,tf  0 1 "Click to Restart" ]
        _     -> [ tf  0 4 (show score) ]
      forms = message ++ (map formPill (plyr :: objs)) in
        color lightGray <| container w h middle
                        <| color white
                        <| collage width height forms


main = lift2 render Window.dimensions <| foldp stepGame gameDefault input
