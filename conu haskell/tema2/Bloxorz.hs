{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs, StandaloneDeriving #-}


module Bloxorz where

import ProblemState

import Data.Matrix
--import qualified Data.Matrix --as Matrix
--deriving instance Ord (Matrix Cell)

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = 'H' --'▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East -- | Null
    deriving (Show, Eq, Ord)

{-
    *** DONE ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Tile Char | Switch [Position] deriving Ord

instance Eq Cell where
    Tile x == Tile y = x == y
    Switch _ == Tile _ = False
    Tile _ == Switch _ = False
    Switch _ == Switch _ = False

toCell :: Char -> Cell
toCell tile
    | tile == 'H' = Tile hardTile
    | tile == 'S' = Tile softTile
    | tile == 'W' = Tile winningTile
    | otherwise = Tile tile

instance Show Cell where
    show (Tile tile) = [tile]
    show (Switch _) = [switch]

{-
    *** DONE ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = NewLevel Position (Matrix Cell) deriving (Eq)--, Ord)

{-
    *** Opțional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiati explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

-- instance Eq Level where
--     (==) = undefined

instance Ord Level where
    compare (NewLevel p1 _) (NewLevel p2 _) = compare p1 p2

{-
    *** TODO ***

    Instantiati Level pe Show.

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou.
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n".
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n".
-}

instance Show Level where
    show (NewLevel _ cells) = "\n-------" ++ (prettyMatrix cells) ++ "\n===="

{-
    *** DONE ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel (n, m) initPos
    = NewLevel (n, m) (setElem (Tile hardTile) initPos (matrix n m (const (Tile emptySpace))))

{-
    *** DONE ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat:
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile tile pos (NewLevel origin cells)
    = NewLevel origin (setElem (toCell tile) pos cells)

{-
    *** DONE ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos tiles (NewLevel origin cells)
    = NewLevel origin (setElem (Switch tiles) pos cells)

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică.
    În funcție de mecanica activată, vor avea loc modificări pe hartă.
-}

changeTile :: Position -> (Matrix Cell) -> (Matrix Cell)
changeTile (i, j) cells = (let tile = getElem i j cells in
    if tile == Tile emptySpace
        then setElem (Tile hardTile) (i, j) cells
        else setElem (Tile emptySpace) (i, j) cells)

activateSwitch :: [Position] -> (Matrix Cell) -> (Matrix Cell)
activateSwitch tiles cells = foldr changeTile cells tiles

activate :: Cell -> Level -> Level
activate (Switch tiles) (NewLevel origin cells)
    = (NewLevel origin (activateSwitch tiles cells))
activate _ (NewLevel origin cells) = (NewLevel origin cells)

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

moveNSEW :: Position -> Directions -> Position
moveNSEW (i, j) dir
    | dir == North = (i - 1, j)
    | dir == South = (i + 1, j)
    | dir == East = (i, j + 1)
    | dir == West = (i, j - 1)
    | otherwise = undefined

move :: Directions -> Level -> Level
move dir (NewLevel origin cells) = let (i, j) = moveNSEW origin dir
    in activate (getElem i j cells) (NewLevel (i, j) cells)

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (NewLevel (i, j) cells) = let cell = getElem i j cells
    in (not (cell == Tile winningTile || cell == Tile emptySpace))

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.

    Hint: Un level câștigat nu are succesori!
    De asemenea, puteți ignora succesorii care
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors level = [(North, move North level),
                        (South, move South level),
                        (East, move East level),
                        (West, move West level)]

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined
