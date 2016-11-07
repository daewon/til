import Control.Arrow     ((&&&))
import Test.Hspec        (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import qualified Data.Vector as Vector (fromList)

import Matrix
  ( Matrix
  , cols
  , column
  , flatten
  , fromList
  , fromString
  , reshape
  , row
  , rows
  , shape
  , transpose
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "matrix" $ do

    -- As of 2016-08-08, there was no reference file
    -- for the test cases in `exercism/x-common`.

    let intMatrix = fromString :: String -> Matrix Int
    let vector = Vector.fromList

    it "extract first row" $ do
      row 0 (intMatrix "1 2\n10 20") `shouldBe` vector [1, 2]
      row 0 (intMatrix "9 7\n8 6"  ) `shouldBe` vector [9, 7]

    it "extract second row" $ do
      row 1 (intMatrix "9 8 7\n19 18 17") `shouldBe` vector [19, 18, 17]
      row 1 (intMatrix "1 4 9\n16 25 36") `shouldBe` vector [16, 25, 36]

    it "extract first column" $ do
      column 0 (intMatrix "1 2 3\n4 5 6\n7 8 9\n 8 7 6")
        `shouldBe` vector [1, 4, 7, 8]
      column 1 (intMatrix "89 1903 3\n18 3 1\n9 4 800")
        `shouldBe` vector [1903, 3, 4]

    it "shape" $ do
      shape (intMatrix ""        ) `shouldBe` (0, 0)
      shape (intMatrix "1"       ) `shouldBe` (1, 1)
      shape (intMatrix "1\n2"    ) `shouldBe` (2, 1)
      shape (intMatrix "1 2"     ) `shouldBe` (1, 2)
      shape (intMatrix "1 2\n3 4") `shouldBe` (2, 2)

    it "rows & cols" $
      (rows &&& cols) (intMatrix "1 2") `shouldBe` (1, 2)

    it "eq" $ do

      intMatrix "1 2" `shouldBe`    intMatrix "1 2"
      intMatrix "2 3" `shouldNotBe` intMatrix "1 2 3"

    it "fromList" $ do
      fromList [[1 ,  2]] `shouldBe` intMatrix "1 2"
      fromList [[1], [2]] `shouldBe` intMatrix "1\n2"

    it "transpose" $ do
      transpose (intMatrix "1\n2\n3"      ) `shouldBe` intMatrix "1 2 3"
      transpose (intMatrix "1 4\n2 5\n3 6") `shouldBe` intMatrix "1 2 3\n4 5 6"

    it "reshape" $
      reshape (2, 2) (intMatrix "1 2 3 4") `shouldBe` intMatrix "1 2\n3 4"

    it "flatten" $
      flatten (intMatrix "1 2\n3 4") `shouldBe` vector [1, 2, 3, 4]

    it "matrix of chars" $
      fromString "'f' 'o' 'o'\n'b' 'a' 'r'" `shouldBe` fromList ["foo", "bar"]

    it "matrix of strings" $ 
       fromString "\"this one\"\n\"may be tricky!\""
       `shouldBe` fromList [ ["this one"      ]
                           , ["may be tricky!"] ]

    it "matrix of strings 2" $ 
       fromString "\"this one\" \"one\" \n\"may be tricky!\" \"really tricky\""
       `shouldBe` fromList [ ["this one"      , "one"          ]
                           , ["may be tricky!", "really tricky"] ]
