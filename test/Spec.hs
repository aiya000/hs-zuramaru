import Test.Tasty (testGroup)
import Test.Tasty as Test (defaultMain)
import qualified Steps.Step1 as Step1


main :: IO ()
main = do
  Test.defaultMain $
    testGroup "zuramaru" $
      [ Step1.test
      ]
