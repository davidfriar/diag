{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Random
import Data.Maybe (fromMaybe)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Routing

-- to do
-- labels for arrows
-- looping arrows
-- clean up styling
-- optional dashing for arrows
-- more sensible text size
-- fake text wrap
-- optional numbering for arrows
--
-- main = mainWith $ draw myDiag
main = do
  g <- getStdGen
  mainWith $
    connectAll
      (evalRand randomDiag g)
      "obj"
      [(p2 (1.0, 1.0), p2 (40.0, 40.0)), (p2 (0.0, 20.0), p2 (40.0, 20.0))]

box :: String -> Diagram B
box title =
  (text title & fc white & fontSize (local 0.2)) <> roundedRect 2 1 0.1 & fc blue & named title

data SeqDiag =
  SeqDiag [Obj] [Msg]

type Obj = String

data Msg =
  Msg
    { source :: Obj
    , dest :: Obj
    , label :: String
    }

myDiag :: SeqDiag
myDiag =
  SeqDiag
    ["Foo", "Bar", "Baz", "Quux"]
    [ "Foo" ---> "Bar" $ "initiate the flux capacitor"
    , "Baz" ---> "Quux" $ "ask politely for the big thing"
    , "Bar" ---> "Bar" $ "do it to me"
    , "Baz" <--- "Quux" $ "reply nonchalantly"
    , "Bar" ---> "Quux" $ "say something silly"
    ]

(--->) :: Obj -> Obj -> String -> Msg
(--->) = Msg

(<---) :: Obj -> Obj -> String -> Msg
(<---) = flip Msg

draw :: SeqDiag -> Diagram B
draw (SeqDiag objs msgs) =
  let boxes = hsep 0.5 $ map box objs
   in drawVerticalLines $
      boxes === strutY 0.5 === vsep 0.5 (map (drawMsg boxes) msgs) === strutY 0.5

drawMsg :: Diagram B -> Msg -> Diagram B
drawMsg diag msg = drawArrow (label msg) (getPoint $ source msg) (getPoint $ dest msg)
  where
    getPoint nm =
      case lookupName nm diag of
        Just d -> location d
        Nothing ->
          error $
          "Ooops. Looks like there's an unexpected name in a message. Maybe a typo? : " ++ nm

drawArrow :: String -> Point V2 Double -> Point V2 Double -> Diagram B
drawArrow label p q =
  let drawLabel halign hpos =
        alignedText halign 1.0 label # fontSize (local 0.1) # moveTo (p .+^ r2 (hpos, -0.1))
   in case compare p q of
        GT -> drawLabel 0.0 0.1 <> arrowBetween p q
        LT -> drawLabel 1.0 (-0.1) <> arrowBetween p q
        EQ ->
          let w = 0.2
              h = 1.0
              shaft = trailFromVertices (map p2 [(0, 0), (w, 0), (w, -h), (0, -h)])
              style = with & arrowShaft .~ shaft & arrowTail .~ lineTail
           in arrowBetween' style p (q .+^ r2 (0, -1)) ||| strutX (w + 0.15) ||| drawLabel 0.0 0.0

drawVerticalLines :: Diagram B -> Diagram B
drawVerticalLines diagram =
  let addLines boxes diag =
        let bottomOf obj = snd $ unp2 $ fromMaybe origin $fst <$> getCorners (boundingBox obj)
            lineFrom box =
              (location box & _y .~ bottomOf box) ~~ (location box & _y .~ bottomOf diag) #
              dashingN [0.01, 0.01] 0
         in foldMap lineFrom boxes <> diag
   in withNames (fst <$> names diagram) addLines diagram
