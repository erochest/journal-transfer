{-# LANGUAGE OverloadedStrings #-}


module ProcessSpec where


import           Data.Aeson
import           Data.Maybe
import           Text.Pandoc.Definition

import           Test.Hspec

import           Process
import           Types

-- import           Debug.Trace
-- import           Text.Groom


spec :: Spec
spec = do
  let h1 = decode "{\
            \\"t\": \"Header\",\
            \\"c\": [1,[\"date-tue-14-apr-2015-163452--0400\", [], []],\
                \[{\"t\": \"Str\", \"c\": \"Date:\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"Tue,\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"14\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"Apr\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"2015\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"16:34:52\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"-0400\"}]]}" :: Maybe Block
      p1 = decode "{\
            \\"t\": \"Para\",\
            \\"c\": [{\"t\": \"Str\", \"c\": \"Today\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"I\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"helped\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Scott\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"work\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"on\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"For\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Better\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"or\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"for\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Verse,\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"throwing\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"the\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"hackiest\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"of\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"hacks\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"right\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"and\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"left\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"at\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"this\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"problem.\"}]}" :: Maybe Block
      h1p1 = decode "[{\
            \\"t\": \"Header\",\
            \\"c\": [1, [\"date-mon-02-feb-2015-114332--0500\", [], []],\
                \[{\"t\": \"Str\", \"c\": \"Date:\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"Mon,\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"02\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"Feb\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"2015\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"11:43:32\"},\
                    \{\"t\": \"Space\", \"c\": []},\
                    \{\"t\": \"Str\", \"c\": \"-0500\"}]]},\
        \{\"t\": \"Para\",\
            \\"c\": [{\"t\": \"Str\", \"c\": \"Today\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"while\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Melina\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"went\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"to\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"the\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"bathroom,\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"she\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"was\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"looking\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"through\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"her\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Bible.\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Afterward\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"I\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"was\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"helping\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"her\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Emph\", \"c\": [{\"t\": \"Str\", \"c\": \"ahem\"}]},\
                \{\"t\": \"Str\", \"c\": \",\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"finish\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"up,\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"she\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"said\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"she\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"hadn’t\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"been\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"able\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"to\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"find\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Link\", \"c\": [[{\"t\": \"Str\", \"c\": \"the\"},\
                            \{\"t\": \"Space\", \"c\": []},\
                            \{\"t\": \"Str\", \"c\": \"crippled\"},\
                            \{\"t\": \"Space\", \"c\": []},\
                            \{\"t\": \"Str\", \"c\": \"lamb\"}],\
                        \[\"http://www.goodreads.com/book/show/987587.The_Crippled_Lamb\", \"\"]]},\
                \{\"t\": \"Str\", \"c\": \".\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"I\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"explained\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"it\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"wasn’t\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"in\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"the\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Bible:\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"it\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"was\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"a\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"fiction\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"story\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"someone\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"had\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"made\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"up\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"later.\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"She\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"said\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"she\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"had\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"found\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"the\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"little\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"lost\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"lamb,\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"and\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"I\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"explained\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"story\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"was\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"kind\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"of\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"fiction\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"too.\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"There\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"wasn’t\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"a\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"specific\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"lost\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"lamb\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Jesus\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"was\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"talking\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"about,\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"but\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"that\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"he’d\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"told\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"the\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"story\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"to\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"teach\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"people\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"things.\"}]},\
        \{\"t\": \"Para\",\
            \\"c\": [{\"t\": \"Str\", \"c\": \"Then\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"she’d\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"asked\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"if\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"Jesus\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"were\"},\
                \{\"t\": \"Space\", \"c\": []},\
                \{\"t\": \"Str\", \"c\": \"real.\"}]}]" :: Maybe [Block]

  describe "blocksToTree" $ do
         it "should create a HeaderGroup with no body." $
            let Just (HG _ bls) = blocksToTree $ maybeToList h1
            in  bls `shouldBe` []
         it "should create a HeaderGroup with a body." $
            let Just (HG _ bls) = blocksToTree $ catMaybes [h1, p1]
            in  length bls `shouldBe` 1
  describe "split'" $ do
         let hgs = split' $ catMaybes [h1, p1] ++ fromMaybe [] h1p1
         it "should split two headers into two HeaderGroups." $
            length hgs `shouldBe` 2
         it "should use only have level 1 headers." $
            [level | (Header level _ _ : _) <- hgs] `shouldSatisfy`
                      \inp -> not (null inp) && all (== 1) inp
         it "should not have headers in the bodies." $ do
            let bodies = concat [body | (Header {} : body) <- hgs]
            bodies `shouldNotBe` []
            [ h | h@(Header 1 _ _) <- bodies] `shouldBe` []
