module CategorerServerSpec
    ( spec
    )
where

import           Test.Hspec                     ( Spec
                                                , it
                                                , describe
                                                , shouldBe
                                                )
import CategorerServer (CategorerAPI)

spec :: Spec
spec = describe "CategorerServer" $ it "should do" $ 1 `shouldBe` 2
