module Lucid.View
  ( View(..)
  , runView
  , includeFragment
  , includeExtraFragment
  , viewValue
  , withValue
  , ViewOf(..)
  , runViewOf
  ) where

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Writer.Strict
import Lucid.Base
import GHC.Exts

import Lucid.Fragment

newtype View s v a =
  View (WriterT (Fragment s v ()) (Fragment s v) a)
  deriving (Functor, Applicative, Monad)

instance Semigroup r => Semigroup (View s v r) where
  View s <> View t = View ((<>) <$> s <*> t)

instance Monoid r => Monoid (View s v r) where
  mempty = View $ pure mempty
  mappend = (<>)

instance IsString (View s v ()) where
  fromString = View . lift . fromString

instance Term (View s v r) (View s v r) where
  term n (View act) = View $ hoist (term n) act

instance (f ~ View s v r) => Term [Attributes] (f -> View s v r) where
  term n attrs (View act) = View $ hoist (term n attrs) act

viewValue :: View s v v
viewValue = includeFragment askValue

withValue :: (v -> View s v r) -> View s v r
withValue act = viewValue >>= act

runView :: Monad m => s -> v -> View s v a -> HtmlT m a
runView s a (View act) = runFragment s a $ do
  (res, extra) <- runWriterT act
  extra
  pure res

includeFragment :: Fragment s v r -> View s v r
includeFragment act = View (lift act)

includeExtraFragment :: Fragment s v r -> View s v ()
includeExtraFragment act = View $ tell (void act)

data ViewOf v =
  forall s . ViewOf s v (View s v ())

runViewOf :: Monad m => ViewOf v -> HtmlT m ()
runViewOf (ViewOf s v view) = runView s v view

instance ToHtml (ViewOf v) where
  toHtmlRaw = toHtml
  toHtml = runViewOf
