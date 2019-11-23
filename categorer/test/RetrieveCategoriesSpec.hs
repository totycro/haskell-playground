{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module RetrieveCategoriesSpec
    ( spec
    )
where

import           Data.Aeson.QQ                  ( aesonQQ )
import           Test.Hspec                     ( Spec
                                                , shouldReturn
                                                , it
                                                , describe
                                                )

import           RetrieveCategories             ( retrieveCategories
                                                , Retrieve
                                                )
import           TestUtils                      ( mockResponse )


mockRetrieve :: Retrieve
mockRetrieve _ = return $ mockResponse [aesonQQ|
         {"parse": {"categories": [
              {"*": "foo"},
              {"*": "bar"}
          ]}}|]

spec :: Spec
spec = describe "retrieveCategories" $ do
    it "returns categories from response " $ do
        retrieveCategories mockRetrieve "foo" `shouldReturn` ["foo", "bar"]
