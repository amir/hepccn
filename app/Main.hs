{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Lib

import Brick.Types (Widget)
import Brick.Util (fg, on)
import Brick.Widgets.Core ((<+>), hLimit, str, vBox, vLimit, withAttr)
import Control.Monad (void)
import Data.List (filter, nub)
import Data.Maybe (catMaybes)

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
  where
    box =
      B.borderWithLabel (str "TCP Connections") $
      hLimit 60 $ vLimit 25 $ L.renderList listDrawElement True l
    ui = C.vCenter $ vBox [C.hCenter box, str " "]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

appEvent ::
     L.List () String
  -> T.BrickEvent () e
  -> T.EventM () (T.Next (L.List () String))
appEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    ev -> M.continue =<< L.handleListEvent ev l
appEvent l _ = M.continue l

initialState :: [String] -> L.List () String
initialState conns = L.list () (Vec.fromList (filter (/= "invalid2.invalid") conns)) 1

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
   in selStr (show a)

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue)
    , (L.listSelectedAttr, V.blue `on` V.white)
    , (customAttr, fg V.cyan)
    ]

theApp :: M.App (L.List () String) e ()
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  cs <- connections
  names <- mapM (getHostServiceName . remote) (filter isHttps cs)
  cns <- mapM getCommonName (nub names)
  void $ M.defaultMain theApp (initialState (catMaybes cns))
