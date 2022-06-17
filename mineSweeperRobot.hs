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
collect (S p t str la) = if elem p t then (S p (delete p t) "collect" (S p t str la))
                         else Null

nextMyStates::MyState->[MyState]
helperUp:: MyState -> MyState
nextMyStates Null = []

nextMyStates st = clearNull (helperUp st:helperDown st:helperRight st:helperLeft st: helperCollect st:[])

isGoal(S p t str la) = if (length t)==0 then True
                       else False

helperUp st = up st
helperDown st = down st
helperLeft st = left st
helperRight st = right st
helperCollect st= collect st

clearNull::[MyState]->[MyState]

clearNull[] = []
clearNull (h:t) = if h==Null then clearNull t
                     else h:clearNull t
     
delete x [] = []
delete x (h:t) = if x==h then delete x t
                 else h:delete x t

search (x:xs) = if (isGoal x) then x
		else search(xs ++ nextMyStates x)

constructSolutionH Null = []
constructSolutionH (S (x,y) mines lastmove parent) = lastmove:constructSolutionH parent
constructSolution cell = reverse (init(constructSolutionH cell))
solve cell list =  constructSolution (search [(S cell list "" Null)])


-- Or to work on any grid size :
manhattan (x1,y1) (x2,y2) = abs(x1 - x2) + abs(y1 - y2)
searchForNearest h [] curdis curitem = curitem
searchForNearest h (x:xs) curdis curitem = if otherdis < curdis then searchForNearest h xs otherdis x else searchForNearest h xs curdis curitem  where otherdis = manhattan h x
goto (x,y) (x1,y1) = if (x==x1 && y==y1) then ["collect"]
			else if (x/=x1) then if x < x1 then "down":goto ((x+1),y) (x1,y1)
					     else "up":goto ((x-1),y) (x1,y1)
			else if (y < y1) then "right":goto (x,(y+1)) (x1,y1)
			else "left" : goto (x,(y-1)) (x1,y1)
solve2 (x,y) [] = []
solve2 startpos (x:xs) = (goto startpos target) ++ (solve2 target (delete target (x:xs))) where target = searchForNearest startpos xs
