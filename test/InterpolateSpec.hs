{-# LANGUAGE OverloadedStrings #-}
module InterpolateSpec where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (append)
import Test.Hspec

import Data.Text.Interp.Interpolate
import Data.Text.Interp.Types

fcs = ["green", "red"]
bcs = ["white", "black"]


samFedoraColors :: Subst
samFedoraColors = SubstL $ SubstV <$> fcs

samBallcapColors :: Subst
samBallcapColors = SubstL $ SubstV <$> bcs

testSubst :: Subst
testSubst = substm [
  ("people", SubstL [
    substm [
      ("sam", substm [
        ("hats", SubstL [
          substm [
            ("type", SubstV "fedora"),
            ("color", samFedoraColors)
         ],
          substm [
            ("type", SubstV "ballcap"),
            ("color", samBallcapColors)
         ]
        ]),
        ("shoes", SubstL $ SubstV <$> ["sneakers", "boots"])
      ])
     ]
    ])
   ]

spec :: Spec
spec = do
  describe "interp" $ do
    it "should interpolate without binding" $ do
      let its = RawText "sam was wearing a " :|
                [ToInterpolate Nothing ["people", "sam", "hats", "type"]]
      (Right res) <- runExceptT $ interp testSubst its
      let expected = ["sam was wearing a fedora", "sam was wearing a ballcap"]
      res `shouldSatisfy` (`elem` expected)

    it "should interpolate with binding" $ do
      let its = RawText "sam was wearing a " :|
                [ToInterpolate (Just (Binding "h" $ "people" :|
                                              ["sam", "hats"])) ["color"],
                 RawText " ",
                 ToInterpolate Nothing ["h", "type"]]
      (Right res) <- runExceptT $ interp testSubst its
      let expected = ["sam was wearing a " `append` fc `append` " fedora" | fc <- fcs] ++
                     ["sam was wearing a " `append` bc `append` " ballcap" | bc <- bcs]
      res `shouldSatisfy` (`elem` expected)

    it "should throw an error with too few keys" $ do
      let its = RawText "sam was wearing a  " :|
                [ToInterpolate Nothing ["people", "sam"]]
      (Left e) <- runExceptT $ interp testSubst its
      e `shouldBe` "too few keys"
