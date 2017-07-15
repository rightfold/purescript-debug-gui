module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.State.Class as State
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug.GUI (class Debug, debug)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Component (Component, ParentDSL, ParentHTML, parentComponent)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Prelude

main :: ∀ eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui value
  where
  value :: Array (Maybe (Boolean -> Int))
  value =
    [ Just if _ then 0 else 1
    , Just if _ then 1 else 2
    , Just if _ then 2 else 3
    , Nothing
    ]

ui :: ∀ m a. Debug a => Component HTML (Tuple a) a Void m
ui = parentComponent {initialState, render, eval, receiver}
  where
  initialState :: a -> a
  initialState = id

  render :: a -> ParentHTML (Tuple a) (Const Void) String m
  render = HH.div [] <<< debug "root"

  eval :: Tuple a ~> ParentDSL a (Tuple a) (Const Void) String Void m
  eval (x /\ next) = next <$ State.put x

  receiver :: a -> Maybe (Tuple a Unit)
  receiver = Just <<< (_ /\ unit)
