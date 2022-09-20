module Lucid.ViewSpec where

import Lucid.FragmentSpec (assertEqualHtml)
import Lucid.Fragment
import Lucid.View

import Lucid
import Test.Hspec
import Data.Text.Lazy (Text)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "View" do

    describe "IsString" do

      it "constructs a View from a string" do
        renderToText (viewOf "example") `shouldBe`
          "example"

    it "renders a view" do
      let view :: View () () ()
          view = div_ "ok"

      renderToText (viewOf view) `shouldBe`
        "<div>ok</div>"

    it "renders a view made of several fragments" do
      let view :: View () () ()
          view = do
            includeFragment (div_ "ok" :: Fragment () () ())
            includeFragment (span_ "second" :: Fragment () () ())

      renderToText (viewOf view) `shouldBe`
        "<div>ok</div><span>second</span>"

    it "embeds a fragment" do
      let view = do
            div_ [class_ "outer"] do
              includeFragment fragment
          fragment :: Fragment () () ()
          fragment = span_ "fragment"
      assertEqualHtml (viewOf view) do
        div_ [class_ "outer"] do
          span_ "fragment"

    it "embeds extra content" do
      let view = do
            div_ [class_ "main"] do
              div_ [class_ "inner"] do
                includeExtraFragment do
                  div_ [class_ "beside"] do
                    pure ()

      assertEqualHtml (viewOf view) do
        div_ [class_ "main"] do
          div_ [class_ "inner"] mempty
        div_ [class_ "beside"] mempty

viewOf :: View () () () -> ViewOf ()
viewOf = ViewOf () ()

renderToText :: ToHtml a => a -> Text
renderToText = renderText . toHtml
