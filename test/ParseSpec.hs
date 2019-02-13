{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import Data.Either
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import Test.Hspec
import Text.Megaparsec

import Data.Text.Interp.Parse
import Data.Text.Interp.Types


spec :: Spec
spec = do
  describe "parsing" $ do
    it "should parse a single key" $ do
      let (Right k) = parse key "" "key"
      k `shouldBe` (Key "key")

    it "should parse multiple keys separated by periods" $ do
      let (Right ks) = parse keys "" "foo.bar.baz"
      ks `shouldBe` [Key "foo", Key "bar", Key "baz"]

    it "should create a ToInterpolate without any binding" $ do
      let (Right ti) = parse keyP "" "foo.bar.baz"
      ti `shouldBe` ToInterpolate Nothing [Key "foo", Key "bar", Key "baz"]

    it "should create a ToInterpolate with a binding" $ do
      let (Right ti) = parse bindP "" "(b#foo.bar).baz.quux"
      ti `shouldBe` ToInterpolate (Just $ Binding (Key "b")
                                    (Key "foo" :| [Key "bar"]))
                                    [Key "baz", Key "quux"]

    it "should parse raw text" $ do
      let (Right txt) = parse rawTextP "" "raw text"
      txt `shouldBe` RawText "raw text"

    it "shouldn't permit an empty binding key" $ do
      let res = parse bindP "" "(#foo).bar.baz"
      res `shouldSatisfy` isLeft

    it "shouldn't permit parens without binding stuff" $ do
      let res = parse bindP "" "(foo).bar.baz"
      res `shouldSatisfy` isLeft

    it "should deal properly with errant periods" $ do
      let res = parse bindP "" ".foo.bar"
      res `shouldSatisfy` isLeft
      let res' = parse bindP "" "foo.bar."
      res' `shouldSatisfy` isLeft

    it "should parse a mixture properly" $ do
      let s = "there is some {{ (text#to.parse) }} {{ for.us }} here"
      let (Right res) = parse itP "" s
      let expected = (RawText "there is some ") :|
                      [
                        ToInterpolate (Just $ Binding (Key "text")
                                  (Key "to" :| [Key "parse"])) [],
                        RawText " ",
                        ToInterpolate Nothing [Key "for", Key "us"],
                        RawText " here" ]
      res `shouldBe` expected
