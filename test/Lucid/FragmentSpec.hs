module Lucid.FragmentSpec where

import Lucid.Fragment

import Lucid
import Test.Hspec
import Data.Text.Lazy (Text, isInfixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Fragment" do

    describe "IsString" do
      it "can be created from a string" do
        let fragment :: Fragment () () ()
            fragment = "example"
        renderToText (runFragment () () fragment) `shouldBe` "example"

    it "can contain terms" do
      let fragment :: Fragment () () ()
          fragment = do
            p_ "test"
      renderToText (runFragment () () fragment) `shouldBe`
        "<p>test</p>"

    it "can contain terms with attributes" do
      let fragment :: Fragment () () ()
          fragment = do
            p_ [class_ "frog"] "test"
      renderToText (runFragment () () fragment) `shouldBe`
        "<p class=\"frog\">test</p>"

  describe "FragmentOf" do

    it "renders an empty FragmentOf" do
      let fragment :: FragmentOf () ()
          fragment = FragmentOf () () $ pure ()
      fragment `shouldBeEqualHtml` mempty

    it "retrieves from state and renders the FragmentOf" do
      let fragment :: FragmentOf Text ()
          fragment = FragmentOf "some text?" () $ do
            st <- askState
            p_ $ toFragment st
      fragment `shouldBeEqualHtml` p_ "some text?"

    it "retrieves from value and renders the FragmentOf" do
      let fragment :: FragmentOf () Integer
          fragment = FragmentOf () 42 $ do
            val <- askValue
            span_ $ toFragment $ show val
      fragment `shouldBeEqualHtml` span_ "42"

renderToText :: ToHtml a => a -> Text
renderToText = renderText . toHtml

assertEqualHtml :: ToHtml a => a -> Html () -> Expectation
assertEqualHtml = shouldBeEqualHtml

shouldBeEqualHtml :: ToHtml a => a -> Html () -> Expectation
shouldBeEqualHtml expected actual = do
  let actualText = renderToText actual
      expectedText = renderToText expected
  if actualText == expectedText
    then
      pure ()
    else
      expectationFailure $ mconcat
        [ "Expected "
        , show expectedText
        , ", but got "
        , show actualText
        ]


shouldContainElement :: ToHtml a => a -> Html () -> Expectation
shouldContainElement expected actual = do
  let actualText = renderToText actual
      expectedText = renderToText expected
  if actualText `isInfixOf` expectedText
    then
      pure ()
    else
      expectationFailure $ mconcat
        [ "Expected "
        , show actualText
        , " to contain "
        , show expectedText
        ]
