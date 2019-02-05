{-# LANGUAGE OverloadedStrings #-}
module GetSpec where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Get
import Types (Subst(..), substm)

samFedoraColors :: Subst Text
samFedoraColors = SubstL $ SubstV <$> ["green", "red", "blue"]

samBallcapColors :: Subst Text
samBallcapColors = SubstL $ SubstV <$> ["white", "black", "orange"]

testSubst :: Subst Text
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
      (Right (c, _)) <- runExceptT $
        get testSubst ["people", "sam", "hats", "color"] M.empty
      c `shouldSatisfy` (`elem` expectedColors)

    it "should error on bad key" $ do
      (Left e) <- runExceptT $ get testSubst ["people", "sam", "shirts"] M.empty
      e `shouldBe` "bad key: shirts"

    it "should error on too many keys" $ do
      (Left e) <- runExceptT $
        get testSubst ["people", "sam", "hats", "color", "hue"] M.empty
      e `shouldBe` "too many keys"

    it "should error on too few keys" $ do
      (Left e) <- runExceptT $ get testSubst ["people", "sam", "hats"] M.empty
      e `shouldBe` "too few keys"

    it "shouldn't bind anything without being asked to" $ do
      (Right (_, m)) <- runExceptT $
        get testSubst ["people", "sam", "hats", "color"] M.empty
      m `shouldBe` M.empty

    it "shouldn't re-bind anything" $ do
      let bm = M.singleton "hats" $ Bound $
                  substm [("type", SubstV "fedora"), ("color", samFedoraColors)]
      (Right (_, m)) <- runExceptT $ get testSubst ["people", "sam", "hats", "color"] bm
      m `shouldBe` bm

    it "should bind NotYetBound" $ do
      let bm = M.singleton "hats" (NotYetBound "h")
      let hats = [substm [("type", SubstV "fedora"), ("color", samFedoraColors)],
                  substm [("type", SubstV "ballcap"), ("color", samBallcapColors)]]
      (Right (_, m)) <- runExceptT $ get testSubst ["people", "sam", "hats", "color"] bm
      let (Just (Bound h)) = M.lookup "h" m
      h `shouldSatisfy` (`elem` hats)

    it "should re-use bound results" $ do
      let bm = M.singleton "hats" $ Bound $
                  substm [("type", SubstV "fedora"), ("color", samFedoraColors)]
      (Right (c, _)) <- runExceptT $ get testSubst ["people", "sam", "hats", "type"] bm
      c `shouldBe` SubstV "fedora"
