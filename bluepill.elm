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

rndRange : Float -> Float -> Float -> Float
rndRange rnd start end = start + (end - start) * rnd

relativeCenter : (Int, Int) -> (Int, Int) -> Vec
relativeCenter (w, h) (x, y) = ( toFloat x - (toFloat w / 2),
                               -(toFloat y - (toFloat h / 2)))

-- INPUT

data Event = Tick Float | Add Pill | Click | Move Vec

sigRands : Int -> Float -> Signal [Float]
sigRands n sec = 
  let tick = lift (\_ -> n) (every (second * sec))
  in  Random.floatList tick

delta = 60

event : Signal Event
event = merges [ lift2 (\d p -> Move (relativeCenter d p)) Window.dimensions (sampleOn (fps (delta + 1)) Mouse.position)
                ,lift (\_ -> Click) Mouse.isClicked 
                ,lift (\rs -> case rs of p::c::_ -> Add (newPill p c)) (sigRands 2 0.12)
                ,lift (Tick . inSeconds) (fps delta) ]

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

type Game = {plyr:Pill, objs:[Pill], state:State, score:Int}

gameDefault : Game
gameDefault = {plyr=player, objs=[], state=Start, score=0}


-- UPDATE

stepPill : Float -> Pill -> Pill
stepPill dt p = { p | pos <- vecAdd p.pos <| vecMulS p.vel dt }

aboveGround : Pill -> Bool
aboveGround {pos} = let (_, y) = pos in y > -hHeight

newPill : Float -> Float -> Pill
newPill rp rc = { defaultPill | pos <- (rndRange rp -hWidth hWidth, snd defaultPill.pos)
                              , col <- if rc < 0.1 then lightBlue else defaultPill.col }
               
stepPlayer : Pill -> Vec -> Pill
stepPlayer p mp = { p | pos <- mp }

playerOut : Pill -> Bool
playerOut {pos} = let (x, y) = pos in abs x > hWidth || abs y > hHeight

stepPlay : Event -> Game -> Game
stepPlay event ({plyr, objs, state, score} as game) = 
  case event of
    Tick dt  -> let culled = filter aboveGround objs
                    hit pill = (vecLen <| vecSub plyr.pos pill.pos) < plyr.rad + pill.rad
                    touched = filter hit culled
                    hitColor c = not <| isEmpty <| filter (\{col} -> col == c) touched
                    hitBlue = hitColor lightBlue
                    hitRed = hitColor lightRed
                    untouched = filter (not . hit) culled
                in
                    { game | objs <- map (stepPill dt) untouched                       
                           , state <- if hitRed || playerOut plyr then Over else state
                           , score <- if hitBlue then score + 1 else score }
    Move mp  -> { game | plyr <- stepPlayer plyr mp }
    Add pill -> { game | objs <- pill :: objs }
    _        -> game

click : Event -> Bool
click event = 
  case event of
    Click -> True
    _     -> False

stepGame : Event -> Game -> Game
stepGame event ({state, score} as game) =
  case state of
    Play  -> stepPlay event game
    Start -> { gameDefault | state <- if click event then Play else state }
    Clear -> { gameDefault | state <- Play }
    Over  -> { gameDefault | state <- if click event then Clear else state
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
      forms = message ++ (map formPill (plyr :: objs))
  in
      color lightGray <| container w h middle
                      <| color white
                      <| collage width height forms


main = lift2 render Window.dimensions <| foldp stepGame gameDefault event
