{-# LANGUAGE TupleSections #-}

module Routing
  ( randomDiag
  , interestingPoints
  , connectAll
  ) where

import Control.Monad.Random
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import Data.List (find, nub)
import Data.Maybe
import qualified Data.MultiMap as M
import qualified Debug.Trace as D
import Diagrams.Backend.SVG.CmdLine
import Diagrams.BoundingBox
import Diagrams.Prelude

type Graph = Gr (P2 Double) Double

type Node = G.LNode (P2 Double)

type Edge = G.LEdge Double

type Obj = Subdiagram B V2 Double Any

connectAll :: Diagram B -> String -> [(P2 Double, P2 Double)] -> Diagram B
connectAll diagram name points =
  let mkGraph objs = visibilityGraph (interestingPoints objs points) objs
      f objs d = d <> mconcat (fmap (connectPoints (mkGraph objs)) points)
   in withNameAll name f diagram

connectPoints :: Graph -> (P2 Double, P2 Double) -> Diagram B
connectPoints g (p1, p2) =
  case shortestPath g (p1, p2) of
    Nothing -> error $ "could not connect points " ++ show (p1, p2)
    Just ps ->
      let shaft = trailFromVertices ps
          style = with & arrowShaft .~ shaft & arrowTail .~ lineTail
       in arrowBetween' style p1 p2

shortestPath :: Graph -> (P2 Double, P2 Double) -> Maybe [P2 Double]
shortestPath g (p1, p2) = do
  start <- findNode p1 g
  end <- findNode p2 g
  path <- sp start end g
  mapM (G.lab g) path

findNode :: P2 Double -> Graph -> Maybe G.Node
findNode label g = fst <$> find (\ln -> snd ln == label) (G.labNodes g)

interestingPoints :: [Obj] -> [(P2 Double, P2 Double)] -> [P2 Double]
interestingPoints objs ps = concatMap getExpandedCorners objs ++ concat [[x, y] | (x, y) <- ps]

getExpandedCorners :: Obj -> [P2 Double]
getExpandedCorners obj =
  let [a, b, c, d] = getAllCorners $ boundingBox obj
      margin = 0.3
      a' = a & _x -~ margin & _y -~ margin
      b' = b & _x -~ margin & _y +~ margin
      c' = c & _x +~ margin & _y -~ margin
      d' = d & _x +~ margin & _y +~ margin
   in [a', b', c', d']

showGraph :: Diagram B -> Graph -> Diagram B
showGraph d g = d <> showGraphNodes g <> showGraphEdges g

showPoints :: [P2 Double] -> Diagram B
showPoints ps = mconcat (flip moveTo (circle 0.2 # fc green) <$> ps)

showGraphNodes :: Graph -> Diagram B
showGraphNodes g = showPoints (snd <$> G.labNodes g)

showGraphEdges :: Graph -> Diagram B
showGraphEdges g = mconcat $ showGraphEdge g <$> G.edges g

showGraphEdge :: Graph -> G.Edge -> Diagram B
showGraphEdge g (n1, n2) =
  let toPoint n = fromJust (G.lab g n)
   in toPoint n1 ~~ toPoint n2 # lc green

visibilityGraph :: [P2 Double] -> [Obj] -> Gr (P2 Double) Double
visibilityGraph ps objs =
  let nodes = zip [1 ..] (nub [p2 (x, y) | x <- view _x <$> ps, y <- view _y <$> ps])
      xs = M.fromList $ zip (view _x <$> (snd <$> nodes)) nodes
      ys = M.fromList $ zip (view _y <$> (snd <$> nodes)) nodes
      edges = concatMap (visibleEdges objs xs ys) nodes
      graph = G.mkGraph nodes edges
   in G.nfilter (\n -> G.deg graph n > 0) graph

visibleEdges :: [Obj] -> M.MultiMap Double Node -> M.MultiMap Double Node -> Node -> [Edge]
visibleEdges objs xs ys node = (fst node, , 1.0) . fst <$> visibleNodes objs xs ys node

visibleNodes :: [Obj] -> M.MultiMap Double Node -> M.MultiMap Double Node -> Node -> [Node]
visibleNodes objs xs ys node =
  let point = snd node
      (x, y) = unp2 point
      f = isVisible objs point . snd
   in filter f $ M.lookup x xs ++ M.lookup y ys

isVisible :: [Obj] -> P2 Double -> P2 Double -> Bool
isVisible objs p1 p2 = all (outside (fromPoints [p1, p2]) . boundingBox) objs

randomDiag :: Rand StdGen (Diagram B)
randomDiag = mconcat <$> replicateM 10 randomRect

randomRect :: Rand StdGen (Diagram B)
randomRect = do
  x <- getRandomR (0.0, 40.0)
  y <- getRandomR (0.0, 40.0)
  w <- getRandomR (1.0, 10.0)
  h <- getRandomR (1.0, 10.0)
  return $ rect h w # translateX x # translateY y # named "obj"
