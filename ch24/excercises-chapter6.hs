import Data.Word
import Text.Trifecta
import Data.Ix
import Data.Bits
import Data.List

data IPAddress = IPAddress Word32
  deriving (Eq, Ord)

instance Show IPAddress where
  show ip = concat $ intersperse "." $ map show $ fromIP ip

parseIP :: Parser (Maybe IPAddress)
parseIP = do
  x1 <- integer
  char '.'
  x2 <- integer
  char '.'
  x3 <- integer
  char '.'
  x4 <- integer
  return $ makeIP $ map fromIntegral [x1,x2,x3,x4]

makeIP :: [Int] -> Maybe IPAddress
makeIP xs
  | length xs == 4 && all (inRange (0,255)) xs  = Just $ IPAddress $ toIP xs
  | otherwise = Nothing
  where toIP [x1,x2,x3,x4] = fromIntegral $ shift x1 24 + shift x2 16 + shift x3 8 + x4
fromIP :: IPAddress -> [Int]
fromIP (IPAddress w32) = map (\n -> fromEnum $ (w32 `shiftR` n) .&. 0xff) [24, 16, 8, 0]
