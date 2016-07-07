import           Text.Trifecta

myp = integer <* eof

main :: IO ()
main = do
  print $ parseString myp mempty "123"
  print $ parseString myp mempty "123abc"
