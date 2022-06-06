import qualified Test.Extensible.StreamFmt.Schema.Internal.Extensible as Ext
import qualified Test.Extensible.StreamFmt.Schema.Internal.Primitives as Prims
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ Prims.specs
    , Ext.specs
    ]
  defaultMain (testGroup "All Tests" [
      testGroup "Specs" specs
    ])
