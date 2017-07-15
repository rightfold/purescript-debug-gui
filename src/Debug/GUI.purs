module Debug.GUI
  ( class Debug, debug
  , class Codebug, codebug
  ) where

import Control.Monad.State.Class as State
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Halogen.Component (Component, ComponentSlot, ParentDSL, ParentHTML, mkComponentSlot, parentComponent, unComponentSlot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

-- | Type class for values that can be displayed in a debugging GUI.
class Debug a where
  debug
    :: ∀ f g m
     . String
    -> a
    -> Array (ParentHTML f g String m)

-- | Type class for values that can be input from a debugging GUI.
class Codebug a where
  codebug
    :: ∀ m
     . String
    -> Maybe a
    -> Array (ParentHTML (Tuple a) (Const Void) String m)

instance debugVoid :: Debug Void where
  debug _ = absurd

instance codebugVoid :: Codebug Void where
  codebug _ _ = [HH.input [HP.placeholder "void", HP.disabled true]]

instance debugUnit :: Debug Unit where
  debug _ x = [HH.code [] [HH.text (show x)]]

instance codebugUnit :: Codebug Unit where
  codebug _ _ = [HH.input [HP.value "unit", HP.readOnly true]]

instance debugBoolean :: Debug Boolean where
  debug _ x = [HH.code [] [HH.text (show x)]]

instance codebugBoolean :: Codebug Boolean where
  codebug _ x =
    [ HH.input [ HP.type_ HP.InputCheckbox
               , HP.checked (fromMaybe false x)
               , HE.onChecked (HE.input (/\))
               ]
    ]

instance debugChar :: Debug Char where
  debug _ x = [HH.code [] [HH.text (show x)]]

instance debugInt :: Debug Int where
  debug _ x = [HH.code [] [HH.text (show x)]]

instance debugNumber :: Debug Number where
  debug _ x = [HH.code [] [HH.text (show x)]]

instance debugString :: Debug String where
  debug _ x = [HH.code [] [HH.text (show x)]]

instance debugArray :: (Debug a) => Debug (Array a) where
  debug s xs =
    [ HH.table []
        [ HH.tbody []
            [ HH.tr [] $ Array.mapWithIndex cell xs
            ]
        ]
    ]
    where cell i x = HH.td [] (debug (s <> "." <> show i) x)

instance debugMaybe :: (Debug a) => Debug (Maybe a) where
  debug s Nothing =
    [HH.fieldset [] [HH.legend [] [HH.code [] [HH.text "Nothing"]]]]
  debug s (Just x) =
    [ HH.fieldset [] $ []
        <> [HH.legend [] [HH.code [] [HH.text "Just"]]]
        <> debug (s <> ".fromJust") x
    ]

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debug s (x /\ y) =
    [ HH.table []
        [ HH.tbody []
            [ HH.tr []
                [ HH.td [] $ debug (s <> ".fst") x
                , HH.td [] $ debug (s <> ".snd") y
                ]
            ]
        ]
    ]

instance debugFunction :: (Codebug a, Debug b) => Debug (a -> b) where
  debug s f = [lmap reject $ HH.slot s ui unit absurd]
    where
    reject :: ∀ h g g' m p q. ComponentSlot h g m p q -> ComponentSlot h g' m p q
    reject = unComponentSlot \p ctor input _ _ _ ->
      mkComponentSlot p ctor input (const Nothing) (const Nothing) (const Nothing)

    ui :: ∀ m. Component HTML (Tuple a) Unit Void m
    ui = parentComponent
      { initialState
      , render
      , eval
      , receiver: const Nothing
      }

    initialState :: Unit -> Maybe a
    initialState _ = Nothing

    render :: ∀ m. Maybe a -> ParentHTML (Tuple a) (Const Void) String m
    render x =
      HH.div [] $ []
        <> renderIn x
        <> [HH.hr []]
        <> renderOut x

    renderIn :: ∀ m. Maybe a -> Array (ParentHTML (Tuple a) (Const Void) String m)
    renderIn = codebug (s <> ".input")

    renderOut :: ∀ m. Maybe a -> Array (ParentHTML (Tuple a) (Const Void) String m)
    renderOut Nothing = []
    renderOut (Just x) = debug (s <> ".output") (f x)

    eval :: ∀ m. Tuple a ~> ParentDSL (Maybe a) (Tuple a) (Const Void) String Void m
    eval (x /\ next) = next <$ State.put (Just x)

instance debugList :: (Debug a) => Debug (List a) where
  debug s xs = debug s (Array.fromFoldable xs)
