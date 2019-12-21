

{-# LANGUAGE RecordWildCards #-}

module Day2 where

import           Control.Applicative
import qualified Data.Map.Strict               as M

type Opcode = Int
type Ops = [Opcode]
type Addr = Int

data Op =
    Add {arg1:: Addr, arg2:: Addr, res:: Addr} |
    Mul {arg1:: Addr, arg2:: Addr, res:: Addr} |
    Exit
    deriving (Show, Eq)

newtype Parser a = P { parse:: Ops -> [(a, Ops, Ops)] }

unit :: a -> Parser a
unit a = P (\xs -> [(a, [], xs)])

instance Functor Parser where
    fmap f p = P (\xs -> [ (f a, c, b) | (a, c, b) <- parse p xs ])

instance Applicative Parser where
    pure = unit
    liftA2 f pa pb = P
        (\xs ->
            [ (f a b, c' ++ c'', xs'')
            | (a, c' , xs' ) <- parse pa xs
            , (b, c'', xs'') <- parse pb xs'
            ]
        )

instance Alternative Parser where
    empty = P (const [])
    (<|>) pa pb = P (\xs -> parse pa xs <|> parse pb xs)

addr :: Parser Opcode
addr = P p  where
    p (x : xs) = [(x, [x], xs)]
    p _        = []

opcode x = P p  where
    p (x' : xs) | x' == x = [(x', [x'], xs)]
    p _                   = []

addOp = Add <$ opcode 1 <*> addr <*> addr <*> addr
mulOp = Mul <$ opcode 2 <*> addr <*> addr <*> addr
exitOp = Exit <$ opcode 99

--mainParser = many (addOp <|> mulOp <|> exitOp)

{-
parseOpcodes :: Ops -> [Op]
parseOpcodes xs = case head $ parse mainParser xs of
    (_      , (err : opcodes)) -> [] --error "Unknown code" or insufficiend number of operands
    ([]     , []             ) -> []
    (ops@[_], _              ) -> ops

type Memory = M.Map Addr Int


eval :: [Op] -> Memory -> Memory
eval []          mem = mem
eval (Exit : xs) mem = mem
eval (Add {..} : xs) mem =
    eval xs $ M.insert (res) (mem M.! arg1 + mem M.! arg2) mem
eval (Mul {..} : xs) mem =
    eval xs $ M.insert (res) (mem M.! arg1 * mem M.! arg2) mem
-}

mainParser = addOp <|> mulOp <|> exitOp

data Zipper a = Z {
    left:: [a],
    right:: [a],
    pos:: Int
} deriving (Show, Eq)

zipper lst = Z [] lst 0
unzip Z {..} = left ++ right

getMem n Z {..} | n >= pos  = right !! (n - pos)
                | otherwise = left !! n

setMem n v Z {..}
    | n >= pos
    = let r = setInList (n - pos) v right
      in  Z { left = left, pos = pos, right = r }
    | otherwise
    = let l = setInList n v left in Z { left = l, pos = pos, right = right }

setInList n v xs = let (h, t) = splitAt n xs in h ++ v : tail t

eval Exit     z        = z
eval Add {..} z@Z {..} = setMem res (getMem arg1 z + getMem arg2 z) z
eval Mul {..} z@Z {..} = setMem res (getMem arg1 z * getMem arg2 z) z

run :: Zipper Opcode -> Zipper Opcode
run z@Z {..} = case parse mainParser right of
    []              -> z
    [(Exit, c, xs)] -> Z { left = left ++ c, right = xs, pos = pos + length c }
    [(op, c, xs)] ->
        run $ eval op Z { left = left ++ c, right = xs, pos = pos + length c }
