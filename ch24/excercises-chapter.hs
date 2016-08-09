import Text.Trifecta
import Control.Applicative

data NumberOrString =
         NOSS String
       | NOSI Integer
       deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving Show

parseNOS :: Parser NumberOrString
parseNOS = try (fmap NOSI integer) <|> try (fmap NOSS $ some letter)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  release <- option [] $ char '-' >> parseNOS `sepBy` (char '.')
  meta <- option [] $ char '+' >> parseNOS `sepBy` (char '.')
  return $ SemVer major minor patch release meta

main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92+buildinfo"
  print $ parseString parseSemVer mempty "1.0.0+buildinfo"
  -- print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
