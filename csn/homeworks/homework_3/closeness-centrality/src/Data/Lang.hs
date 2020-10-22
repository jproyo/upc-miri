module Data.Lang where

import Relude as R
import Data.Vector as V
import Data.Vector.Mutable as M
import Control.Monad.Primitive as P
import Data.Vector.Algorithms.Merge as A
import Data.IntSet as IS
import Data.Sequence as S

data Language = Language 
    { language   :: Text
    , nVertices  :: Integer
    , edges      :: [(Int, Int)]
    } 

newtype Graph = Graph (V.Vector [Int])

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
        A.sortBy (\a b -> if R.length a > R.length b then LT else GT) v
        return v

insertNewEdge :: P.PrimMonad m
              => M.MVector (P.PrimState m) [Int]
              -> (Int, Int)
              -> m ()
insertNewEdge v (xInt, yInt) = M.modify v ((:) yInt) xInt >> M.modify v ((:) xInt) yInt 

bfs :: V.Vector [Int] -> Int -> [Int]
bfs graph start = go IS.empty graph (S.singleton start) 0

go :: IS.IntSet -> V.Vector [Int] -> S.Seq Int -> Int -> [Int]
go seen graph queue distance = 
  case S.viewl queue of
      S.EmptyL      -> []
      vertex S.:< rest -> 
          let neighbors = R.filter (not . flip IS.member seen) . (V.!) graph $ vertex
              seen'     = IS.insert vertex seen
              queue'    = rest S.>< S.fromList neighbors
              newDist   = distance + 1
           in R.replicate (R.length neighbors) newDist R.++ go seen' graph queue' newDist
            

closeness :: Graph -> Int -> Float
closeness (Graph v) idx = let vertices = V.length v
                              sumDist  = getSum . foldMap (Sum . (/) 1 . fromIntegral) $ bfs v idx  
                           in sumDist * (1 / fromIntegral (vertices - 1)) 


closenessCentrality :: Graph -> Float
closenessCentrality g@(Graph v) = let vertices = V.length v
                                   in getSum . foldMap (Sum . closeness g) $ [0..vertices-1]
    