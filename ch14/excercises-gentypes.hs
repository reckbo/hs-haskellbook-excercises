import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)

data Fool = Fulse | Frue
          deriving (Eq, Show)

fulseGen :: Gen Fool
fulseGen = return Fulse

frueGen :: Gen Fool
frueGen = return Frue

instance Arbitrary (Fool) where
  -- arbitrary = elements [Fulse,Frue]
  arbitrary = frequency [(2,frueGen) , (1,fulseGen)]
