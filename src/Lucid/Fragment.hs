module Lucid.Fragment where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Lucid.Base
import GHC.Exts

newtype Fragment s v r =
  Fragment (HtmlT (Reader (s, v)) r)
  deriving (Functor, Applicative, Monad)

instance Semigroup r => Semigroup (Fragment s v r) where
  Fragment v <> Fragment b = Fragment ((<>) <$> v <*> b)

instance Monoid r => Monoid (Fragment s v r) where
  mempty = pure mempty
  mappend = (<>)

instance IsString (Fragment s v ()) where
  fromString = Fragment . fromString

instance Term (Fragment s v r) (Fragment s v r) where
  term n (Fragment f) = Fragment $ term n f

instance (f ~ Fragment s v r) => Term [Attributes] (f -> Fragment s v r) where
  term t attrs (Fragment act) = Fragment $ term t attrs act

toFragment :: ToHtml x => x -> Fragment s v ()
toFragment = Fragment . toHtml

askState :: Fragment s v s
askState = Fragment $ lift $ asks fst

askValue :: Fragment s v v
askValue = Fragment $ lift $ asks snd

runFragment :: Monad m => s -> v -> Fragment s v r -> HtmlT m r
runFragment s a (Fragment act) =
  generalizeHtmlT $ runReader (commuteHtmlT act) (s, a)

data FragmentOf s v =
  FragmentOf s v (Fragment s v ())

instance ToHtml (FragmentOf s v) where
  toHtml (FragmentOf s v f) = runFragment s v f
  toHtmlRaw = toHtml
