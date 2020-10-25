module Data.Lang where

import Relude as R
import Data.Vector as V
import Data.Vector.Mutable as M
import Control.Monad.Primitive as P
import Data.IntSet as IS
import Data.Sequence as S
import Control.Parallel.Strategies as Par

data Language = Language 
    { language   :: Text
    , nVertices  :: Integer
    , edges      :: [(Int, Int)]
    } 

type Edges = Vector [Int]

newtype Graph = Graph Edges

to :: Graph -> V.Vector [Int]
to = coerce

from :: V.Vector [Int] -> Graph 
from = coerce

nEdges :: Language -> Int
nEdges = R.length . edges

kCalc :: Language -> Double
kCalc l = fromIntegral (2*nEdges l) / fromIntegral (nVertices l)

delta :: Language -> Double
delta l = fromIntegral (2*nEdges l) / fromIntegral (nVertices l*nVertices l-1)

buildGraph :: Language -> Graph
buildGraph Language{..} = 
    from $ V.create $ do 
        v <- M.replicate (fromInteger nVertices) []
        R.forM_ edges (insertNewEdge v)
        return v

insertNewEdge :: P.PrimMonad m
              => M.MVector (P.PrimState m) [Int]
              -> (Int, Int)
              -> m ()
insertNewEdge v (xInt, yInt) = M.modify v ((:) yInt) xInt >> M.modify v ((:) xInt) yInt 

{-# INLINE bfs #-}
bfs :: V.Vector [Int] -> Int -> Float
bfs graph start = R.sum $ R.unfoldr (go graph) (IS.empty, S.singleton start, 0)

{-# INLINE go #-}
go :: V.Vector [Int] -> (IS.IntSet, S.Seq Int, Int) -> Maybe (Float, (IS.IntSet, S.Seq Int, Int))
go graph (seen, queue, distance) = 
  case S.viewl queue of
      S.EmptyL         -> Nothing
      vertex S.:< rest -> 
          let neighbors'  = R.filter (`IS.notMember` seen) . V.unsafeIndex graph $ vertex
              seen'       = IS.union (IS.insert vertex seen) ( IS.fromList neighbors')
              queue'     = rest S.>< S.fromList neighbors'
              newDist    = distance + 1
              summedDist = sumDist (R.length neighbors') newDist 
           in Just (summedDist, (seen', queue', newDist))


{-# INLINE sumDist #-}
sumDist :: Int -> Int -> Float           
sumDist r i | i == 0    = 0.0
            | otherwise = getSum . foldMap (Sum . (/) 1 . fromIntegral) $ R.replicate r i

{-# INLINE closeness #-}
closeness :: Graph -> Int -> Float
closeness (Graph v) idx = let vertices = V.length v
                              !sumDist'  = bfs v idx
                           in sumDist' * (1 / fromIntegral (vertices - 1)) 

closenessCentrality :: Graph -> Float
closenessCentrality g = let vertices = V.length $ to g
                            oneOverN = 1 / fromIntegral vertices
                         in  (*) oneOverN
                                . R.sum
                                . parMap rdeepseq (closeness g)
                                $ [0..vertices-1]
    