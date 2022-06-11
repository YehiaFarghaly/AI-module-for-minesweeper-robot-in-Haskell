type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState  deriving (Show,Eq)

up:: MyState -> MyState

up Null = Null
up (S (x,y) t str la) =  if x-1 <0 then Null
                         else (S (x-1,y) t "up"  (S (x,y) t  str la))
down Null = Null
down (S (x,y) t str la) = if x+1>3 then Null
                          else (S (x+1,y) t "down"  (S (x,y) t  str la))
right Null = Null
right (S (x,y) t str la) = if y+1>3 then Null
                           else (S (x,y+1) t "right"  (S (x,y) t  str la))
left Null = Null
left (S (x,y) t str la) = if y-1<0 then Null 
                          else (S (x,y-1) t "left"  (S (x,y) t  str la))

collect:: MyState -> MyState
collect Null = Null
collect (S p t str la) = if elem p t then (S p (delete p t) "collect" la)
                         else Null

nextMyStates::MyState->[MyState]
helperUp:: MyState -> MyState
nextMyStates Null = []

nextMyStates st = clearNull (helperUp st:helperDown st:helperRight st:helperLeft st:[])

isGoal(S p t str la) = if (length t)==0 then True
                       else False

search::[MyState]->MyState
search (h:t) = if(isGoal h) then h
               else search t

helperUp st = up st
helperDown st = down st
helperLeft st = left st
helperRight st = right st

clearNull::[MyState]->[MyState]

clearNull[] = []
clearNull (h:t) = if h==Null then clearNull t
                     else h:clearNull t
                       

delete x [] = []
delete x (h:t) = if x==h then delete x t
                 else h:delete x t
