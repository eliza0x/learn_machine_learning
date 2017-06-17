import Test.Hspec 
import Logic hiding (main)

main :: IO ()
main = hspec $ describe "Logic" $ do
    it "AND" $ do
      step (andUnit 1 1) `shouldBe` 1
      step (andUnit 1 0) `shouldBe` 0
      step (andUnit 0 1) `shouldBe` 0
      step (andUnit 0 0) `shouldBe` 0
    it "OR" $ do
      step (orUnit 1 1) `shouldBe` 1
      step (orUnit 1 0) `shouldBe` 1
      step (orUnit 0 1) `shouldBe` 1
      step (orUnit 0 0) `shouldBe` 0
    it "NAND" $ do
      step (nandUnit 1 1) `shouldBe` 0
      step (nandUnit 1 0) `shouldBe` 1
      step (nandUnit 0 1) `shouldBe` 1
      step (nandUnit 0 0) `shouldBe` 1
    it "NOR" $ do
      step (norUnit 1 1) `shouldBe` 0
      step (norUnit 1 0) `shouldBe` 0
      step (norUnit 0 1) `shouldBe` 0
      step (norUnit 0 0) `shouldBe` 1
    it "XOR" $ do
      step (xorUnit 1 1) `shouldBe` 0
      step (xorUnit 1 0) `shouldBe` 1
      step (xorUnit 0 1) `shouldBe` 1
      step (xorUnit 0 0) `shouldBe` 0

