{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}


instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    lins :: Int,
    colums :: Int,
    hunter :: Position,
    target :: [Target],
    obstacles :: [Position],
    gateway :: [[Position]]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

getElemString :: Game -> Position -> String
getElemString (Game line_count cols_count old_hunt old_targs old_obst old_gates) pos@(x,y)
    | pos == old_hunt = "!"
    | (elementOf pos old_obst) == True && ( (y == 0) || (y == cols_count - 1 && x == line_count - 1) || (y /= cols_count - 1)) = "@" 
    | (elementOf pos old_obst) == True && (y == cols_count - 1) = "@\n"
    | (elementOf pos (getTargetPosList old_targs) ) == True = "*"
    | (elementOf pos (concat old_gates)) == True = "#"
    | otherwise = " " 

getAllPos :: Game -> [Position]
getAllPos (Game line_count cols_count old_hunt old_targs old_obst old_gates) = concat (transpose ((take (line_count * cols_count) [ [ (i,j) | i <- [0,1..line_count-1] ] | j <- [0,1..cols_count-1] ])))

gameAsString :: Game -> String
gameAsString  game = concatMap (getElemString game) (getAllPos game)
-- gameAsString (Game line_count cols_count hunt targs obst gates) = my_string where
--     my_string = map ( getElemString line_count cols  )

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
generateObstacle :: Int -> Int -> [Position]
generateObstacle n m = removeDuplicates (list1 ++ list2 ++ list3 ++ list4) where
    list1 = zip (replicate m 0) (take m [0,1..])
    list2 = zip (take n [0,1..]) (replicate n 0) 
    list3 = zip (replicate m (n-1)) (take m [0,1..])
    list4 = zip (take n [0,1..]) (replicate n (m-1))


emptyGame :: Int -> Int -> Game
emptyGame n m = Game l c hunt targ obstac gate where
    l = n
    c = m
    hunt = (1,1)
    targ = []
    gate = []
    obstac = generateObstacle n m
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
elementIndex :: Int -> Position -> [Position] -> Int
elementIndex _ _  [] = -1
elementIndex size el (x : xs) = if el == x then size - length xs - 1 else elementIndex size el xs

elementOf :: Position -> [Position] -> Bool
elementOf _  [] = False
elementOf (a,b) (x : xs) = if (fst x) == a && (snd x) == b then True else elementOf (a,b) xs

getTargetPosition :: Target -> Position
getTargetPosition (Target pos beh) = pos

getTargetPosList :: [Target] -> [Position]
getTargetPosList target_list = map getTargetPosition target_list

replaceHunter :: Int -> Int -> Position -> Position -> [Position] -> Position
replaceHunter lin col (oldx, oldy) (newx, newy) list = 
    if ((newx >= 0) && (newx <= col ) && (newy >= 0) && (newy <= lin )) then
        if elementOf (newx, newy) list == False then (newx, newy)
        else (oldx, oldy)
        else (oldx, oldy)

addHunter :: Position -> Game -> Game
addHunter (x,y) (Game line_count cols_count old_hunt old_targs old_obst old_gates) = Game lins cols new_hunt new_targ new_obst new_gates where
    lins = line_count
    cols = cols_count
    new_hunt = replaceHunter line_count cols_count old_hunt (x,y) old_obst
    new_targ = old_targs  
    new_obst = old_obst 
    new_gates = old_gates

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget new_behavior (x,y) (Game line_count cols_count old_hunt old_targs old_obst old_gates) = Game lins cols new_hunt new_targ new_obst new_gates where
    lins = line_count
    cols = cols_count
    new_hunt = old_hunt
    new_targ = old_targs ++ [(Target (x,y) new_behavior)] 
    new_gates = old_gates
    new_obst = old_obst

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((a,b), (c,d)) (Game line_count cols_count old_hunt old_targs old_obst old_gates) = Game lins cols new_hunt new_targ new_obst new_gates where
    lins = line_count
    cols = cols_count
    new_hunt = old_hunt
    new_targ = old_targs
    new_obst = old_obst
    new_gates = old_gates ++ [poslist] where
        poslist = [(a,b)] ++ [(c,d)]
    

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle  (x,y) (Game line_count cols_count old_hunt old_targs old_obst old_gates) = Game lins cols new_hunt new_targ new_obst new_gates where
    lins = line_count
    cols = cols_count
    new_hunt = old_hunt
    new_targ = old_targs
    new_gates = old_gates
    new_obst = old_obst ++ [(x,y)]

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
findPair :: Position -> [[Position]] -> Position
findPair _ [] = (-1,-1)
findPair pos (x:xs) = if pos == (head x) then (last x) else if pos == (last x) then (head x) else findPair pos xs


getAllGamePos :: Game -> [Position]
getAllGamePos (Game line_count cols_count old_hunt old_targs old_obst old_gates) = pos_list where
    pos_list = (getTargetPosList old_targs) ++ old_obst ++ (concat old_gates) ++ [old_hunt] 

attemptMove :: Position -> Game -> Maybe Position
attemptMove (x,y) game@(Game line_count cols_count old_hunt old_targs old_obst old_gates) 
    | (elementOf (x,y) (concat old_gates)) == True = Just (findPair (x,y) old_gates)
    | (elementOf (x,y)  (getAllGamePos game)) == False && ((x > 0 && x < line_count ) && ( y > 0 && y < cols_count)) == True = Just (x,y)
    | (elementOf (x,y)  old_obst) == True = Nothing
    | otherwise = Nothing


findTarget :: Position -> [Target] -> Target
findTarget pos (x:xs) = if pos == (position x) then x else findTarget pos xs
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

modifyPos :: Int -> Int -> Position -> Position
modifyPos x y (a,b) = (a + x, b + y) 

goEast :: Behavior

goEast pos game = 
    if (attemptMove (modifyPos 0 1 pos) game ) /= Nothing then (Target (fromJust (attemptMove (modifyPos 0 1 pos) game))  (behavior (findTarget pos (target game))))
    else if ( findPair pos (gateway game) ) /= (-1,-1) then (Target (findPair pos (gateway game))  (behavior (findTarget pos (target game)))) else 
    (findTarget pos (target game))

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos game =
    if (attemptMove (modifyPos 0 (-1) pos) game ) /= Nothing then (Target (fromJust (attemptMove (modifyPos 0 (-1) pos) game))  (behavior (findTarget pos (target game))))
    else if ( findPair pos (gateway game) ) /= (-1,-1) then (Target (findPair pos (gateway game))  (behavior (findTarget pos (target game)))) else 
    (findTarget pos (target game))


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos game = 
        if (attemptMove (modifyPos (-1) 0 pos) game ) /= Nothing then (Target (fromJust (attemptMove (modifyPos (-1) 0 pos) game))  (behavior (findTarget pos (target game))))
    else if ( findPair pos (gateway game) ) /= (-1,-1) then (Target (findPair pos (gateway game))  (behavior (findTarget pos (target game)))) else 
    (findTarget pos (target game))

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos game =
    if (attemptMove (modifyPos 1 0 pos) game ) /= Nothing then (Target (fromJust (attemptMove (modifyPos 1 0 pos) game))  (behavior (findTarget pos (target game))))
    else if ( findPair pos (gateway game) ) /= (-1,-1) then (Target (findPair pos (gateway game))  (behavior (findTarget pos (target game)))) else 
    (findTarget pos (target game))

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce dir 
    | dir == 1 = bounceSouth   
    | dir == (-1) = bounceNorth 

bounceSouth :: Behavior
bounceSouth pos game = if (attemptMove (modifyPos 1 0 pos) game) == Nothing then (Target (fromJust (attemptMove (modifyPos (-1) 0 pos) game )) bounceNorth)
else (Target (fromJust (attemptMove (modifyPos 1 0 pos) game ))  (behavior (findTarget pos (target game))))

bounceNorth :: Behavior
bounceNorth pos game = if (attemptMove (modifyPos (-1) 0 pos) game) == Nothing then (Target (fromJust (attemptMove (modifyPos 1 0 pos) game )) bounceSouth) 
else (Target (fromJust (attemptMove (modifyPos (-1) 0 pos) game ))  (behavior (findTarget pos (target game))))

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game@(Game line_count cols_count old_hunt old_targs old_obst old_gates) = (Game line_count cols_count old_hunt new_targs old_obst old_gates) where
    new_targs = map (\(Target pos b) -> b pos game) old_targs

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (huntx, hunty) (Target (x,y) behavior_target ) = 
    if (abs (huntx - x) == 1 && hunty == y) || ( abs(hunty - y) == 1 && huntx == x ) then True else False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
moveHunter :: Direction -> Position -> Game ->Position
moveHunter dir old_pos game
    | dir == North = if (attemptMove(modifyPos (-1) 0 old_pos) game) == Nothing then old_pos else (fromJust (attemptMove(modifyPos (-1) 0 old_pos) game) )  
    | dir == South = if (attemptMove(modifyPos 1 0 old_pos) game ) == Nothing then old_pos else (fromJust (attemptMove(modifyPos 1 0 old_pos) game ) )
    | dir == East = if (attemptMove(modifyPos 0 1 old_pos) game) == Nothing then old_pos else (fromJust (attemptMove(modifyPos 0 1 old_pos) game) )
    | dir == West = if (attemptMove (modifyPos 0 (-1) old_pos) game) == Nothing then old_pos else (fromJust (attemptMove (modifyPos 0 (-1) old_pos) game) )

getLeftTargets :: Position -> [Target] -> [Target]
getLeftTargets pos oldt = [x | x <- oldt, (isTargetKilled pos x ) == False]

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bool game@(Game line_count cols_count old_hunt old_targs old_obst old_gates) = (Game l c new_hunt new_targ new_obst new_gates) where
    l = line_count
    c = cols_count
    new_gates = old_gates
    new_obst = old_obst
    new_hunt = (moveHunter dir old_hunt game) 
    new_targ = if bool == False then  old_targs else
        (getLeftTargets new_hunt ( target (moveTargets (Game line_count cols_count new_hunt (getLeftTargets new_hunt old_targs) old_obst old_gates))))

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game line_count cols_count old_hunt old_targs old_obst old_gates) = if length old_targs > 0 then True else False

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


findKillTarget :: Position -> [Target] -> Position
findKillTarget _ [] = (1000, 1000)
findKillTarget pos (x:xs) = if ( isTargetKilled pos x ) == True then (position x) else findKillTarget pos xs

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors old_game = [new_game1, new_game2, new_game3, new_game4] where 
        new_game1 = (North, (advanceGameState North False old_game))
        new_game2 = (South, (advanceGameState South False old_game))
        new_game3 = (East, (advanceGameState East False old_game))
        new_game4 = (West, (advanceGameState West False old_game))

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal old_game = if length (getLeftTargets (hunter old_game) (target old_game)) == length (target old_game) then False else True

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h old_game =  hEuclidean (hunter old_game) (findKillTarget (hunter old_game) (target old_game)) 

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
hManhattan :: Position -> Position -> Float
hManhattan (x1, y1) (x2, y2) = fromIntegral $ ((abs(x1 - x2)) + (abs(y1 - y2))) ^ pow
  where
    pow = 2 :: Int
    
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame old_game) = [new_game1, new_game2, new_game3, new_game4] where 
        new_game1 = (North, (BonusGame (advanceGameState North False old_game)))
        new_game2 = (South, (BonusGame (advanceGameState South False old_game)))
        new_game3 = (East, (BonusGame (advanceGameState East False old_game)))
        new_game4 = (West, (BonusGame (advanceGameState West False old_game)))


    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame old_game) = if length (getLeftTargets (hunter old_game) (target old_game)) == length (target old_game) then False else True

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame old_game) = hManhattan (hunter old_game) (findKillTarget (hunter old_game) (target old_game)) 
