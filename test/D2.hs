module D2
    ( tests
    )
where

import           Day2
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "DAY2 Tests" [t1, t2, t3, t4, t5, r1, r2]


t1 = testCase "Parser Opcode" $ do
    -- test data  :: [(input, parser, result)]
    let td =
            [ ([9]  , opcode 9, [(9, [9], [])])
            , ([9]  , opcode 1, [])
            , ([100], addr    , [(100, [100], [])])
            ]
    mapM_ (\(i, p, r) -> parse p i @?= r) td

t2 = testCase "Applicative" $ do
    let i = [1, 2]
        td =
            [ ((,) <$> opcode 1 <*> opcode 2, [((1, 2), [1, 2], [])])
            , ((,) <$> opcode 1 <*> opcode 3, [])
            ]
    mapM_ (\(p, r) -> parse p i @?= r) td

t3 = testCase "Set memory" $ do
    let z = Z { left = [1, 2, 3], right = [10, 20, 30], pos = 3 }
        td =
            [ (2, -1, \z -> [1, 2, -1] @=? left z)
            , (3, -1, \z -> [-1, 20, 30] @=? right z)
            , (5, -1, \z -> [10, 20, -1] @=? right z)
            ]
    mapM_ (\(n, v, r) -> r $ setMem n v z) td

t4 = testCase "Get memory" $ do
    let z  = Z { left = [1, 2, 3], right = [10, 20, 30], pos = 3 }
        td = [(2, 3), (3, 10), (5, 30)]
    mapM_ (\(n, r) -> getMem n z @?= r) td


t5 = testCase "Eval" $ do
    let td =
            [ -- addition
              ([1, 0, 0, 0, 99]             , [2, 0, 0, 0, 99])
            , -- multiplication
              ([2, 3, 0, 3, 99]             , [2, 3, 0, 6, 99])
            , -- early termination
              ([2, 4, 4, 0, 99, 2, 3, 3, 0] , [9801, 4, 4, 0, 99, 2, 3, 3, 0])
            , -- self rewrite 
              ([1, 1, 1, 4, 99, 5, 6, 0, 99], [30, 1, 1, 4, 2, 5, 6, 0, 99])
            , ( [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
              , [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
              )
            ]
    mapM_ (\(i, o) -> Day2.unzip (run $ zipper i) @?= o) td


prepInput noun verb z = setMem 2 verb $ setMem 1 noun z

r1 =
    testCase "result 1"
        $ let i = prepInput 12 2 (zipper input)
          in  head (Day2.unzip $ run i) @?= 2894520

r2 = testCase "result 2" $ do
    let
        x = do
            noun <- [0 .. 99]
            verb <- [0 .. 99]
            guard
                (  head (Day2.unzip $ run (prepInput noun verb (zipper input)))
                == 19690720
                )
            return $ 100 * noun + verb
    x @?= [9342]


input =
    [ 1 -- 2894520
    , 0
    , 0
    , 3 -- 3 -- 14 -- 15 -- 2
    , 1
    , 1
    , 2
    , 3
    , 1
    , 3
    , 4
    , 3
    , 1
    , 5
    , 0
    , 3
    , 2
    , 1
    , 9
    , 19 -- 36
    , 1
    , 13
    , 19
    , 23 -- 41
    , 2
    , 23
    , 9
    , 27 -- 123
    , 1
    , 6
    , 27
    , 31 -- 125
    , 2
    , 10
    , 31
    , 35 -- 500
    , 1
    , 6
    , 35
    , 39 -- 502
    , 2
    , 9
    , 39
    , 43 -- 1506
    , 1
    , 5
    , 43
    , 47 -- 1507
    , 2
    , 47
    , 13
    , 51 -- 7535
    , 2
    , 51
    , 10
    , 55 -- 30140
    , 1
    , 55
    , 5
    , 59 -- 30141
    , 1
    , 59
    , 9
    , 63 -- 30144
    , 1
    , 63
    , 9
    , 67 -- 30147
    , 2
    , 6
    , 67
    , 71 -- 60294
    , 1
    , 5
    , 71
    , 75 -- 60295
    , 1
    , 75
    , 6
    , 79 -- 60297
    , 1
    , 6
    , 79
    , 83 -- 60299
    , 1
    , 83
    , 9
    , 87 -- 60302
    , 2
    , 87
    , 10
    , 91 -- 241208
    , 2
    , 91
    , 10
    , 95 -- 964832
    , 1
    , 95
    , 5
    , 99 -- 964833
    , 1
    , 99
    , 13
    , 103 -- 964838
    , 2
    , 103
    , 9
    , 107 -- 2894514
    , 1
    , 6
    , 107
    , 111 -- 2894516
    , 1
    , 111
    , 5
    , 115 -- 2894517
    , 1
    , 115
    , 2
    , 119 -- 2894519
    , 1
    , 5
    , 119
    , 0 -- 
    , 99
    , 2
    , 0
    , 14
    , 0
    ]
