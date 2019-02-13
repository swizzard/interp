{-# LANGUAGE OverloadedStrings #-}
module GetSpec where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Test.Hspec

import Data.Text.Interp.Get
import Data.Text.Interp.Types

samFedoraColors :: Subst
samFedoraColors = SubstL $ SubstV <$> ["green", "red", "blue"]

samBallcapColors :: Subst
samBallcapColors = SubstL $ SubstV <$> ["white", "black", "orange"]

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

expectedColors = let (SubstL fcs) = samFedoraColors
                     (SubstL bcs) = samBallcapColors
                  in fcs ++ bcs

spec :: Spec
spec = do
  describe "get" $ do
    it "should get a color" $ do
      (Right c) <- runExceptT $
        get testSubst ["people", "sam", "hats", "color"] M.empty
      c `shouldSatisfy` (`elem` expectedColors)

    it "should error on bad key" $ do
      (Left e) <- runExceptT $ get testSubst ["people", "sam", "shirts"] M.empty
      e `shouldBe` "bad key: shirts"

    it "should error on too many keys" $ do
      (Left e) <- runExceptT $
        get testSubst ["people", "sam", "hats", "color", "hue"] M.empty
      e `shouldBe` "too many keys"

    it "should re-use bound results" $ do
      let bm = M.singleton "hats"  $
                  substm [("type", SubstV "fedora"), ("color", samFedoraColors)]
      (Right c) <- runExceptT $ get testSubst ["people", "sam", "hats", "type"] bm
      c `shouldBe` SubstV "fedora"
