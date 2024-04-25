import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import GHC.Utils.Misc (split)
import Data.List (sort)

--- This is the current implementation of the `1_Coffee_test.exe` test that is run in the first part.
--- If you would prefer to run this as a standard haskell file then everything should run correctly through main.

type ParsedType = [Pure]

data Pure = Enum String [String] | Class String [(String, MetaType)] | Association String [(String, MetaType)]
        deriving (Show, Ord)

type MetaType = (String, [Either Integer String])

instance Eq Pure where
        (==) (Enum a bs) (Enum x ys) = (sort bs) == (sort ys) && stringIgnoreFolder a x
        (==) (Class a ms) (Class b ns) = (sort ms) == (sort ns) && stringIgnoreFolder a b
        (==) (Association a ms) (Association b ns) = (sort ms) == (sort ns) && stringIgnoreFolder a b
        (==) _ _ = False

stringIgnoreFolder :: String -> String -> Bool
stringIgnoreFolder a b = last (split ':' a) == last (split ':' b)

importingValue :: [Pure]
importingValue = [Class "coffee::model::Bean" [("BagId", ("String", [Left 1])), ("Roast", ("String", [Left 1])), ("OriginLocation", ("String", [Left 1]))]]

classesValue :: [Pure]
classesValue = [Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("String",[Left 1])),("OriginLocation",("String",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))]]

enumsValue :: [Pure]
enumsValue = [Enum "coffee::model::RoastStrength" ["Light","Dark","Raw"],Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("coffee::model::RoastStrength",[Left 1])),("OriginLocation",("String",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))]]

associationValue :: [Pure]
associationValue = [Enum "coffee::model::RoastStrength" ["Light","Dark","Raw"],Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("coffee::model::RoastStrength",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))],Association "coffee::model::BeanLocation" [("Bean",("coffee::model::Bean",[Left 1,Right "*"])),("Location",("coffee::model::Location",[Left 1]))]]

companyValue :: [Pure]
companyValue = [Enum "coffee::model::RoastStrength" ["Light","Dark","Raw"],Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("coffee::model::RoastStrength",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))],Class "coffee::model::Company" [("CompanyId",("Integer",[Left 1])),("Roaster",("Boolean",[Left 1]))],Association "coffee::model::BeanLocation" [("Bean",("coffee::model::Bean",[Left 1,Right "*"])),("Location",("coffee::model::Location",[Left 1]))],Association "coffee::model::CompanyBeans" [("Company",("coffee::model::Company",[Left 1])),("Bean",("coffee::model::Bean",[Right "*"]))]]

fullTest :: [(String, [Pure])]
fullTest = reverse [("test testing", importingValue),("Classes Exercise", classesValue),("Enums Exercise", enumsValue),("Associations Exercise", associationValue),("Company Exercise", companyValue)]

testValues :: [Pure] -> [(String, [Pure])] -> [String]
testValues input ((sf,exf):(sp,exp):exs) | compareToTestValue input exp && (not (compareToTestValue input exf)) = ["Looks like you managed to pass the " ++ sp ++ " successfully!", sf ++ " looks like it still needs work"]
                                         | compareToTestValue input exf = ["WOO well dones looks like the 1-Coffee-classes have all been completed!!!"]
                                         | otherwise = testValues input ((sp,exp):exs)
testValues input _ = ["Oh that looks like none of the tests pass, probably due to some model changes"]

compareToTestValue :: [Pure] -> [Pure] -> Bool
compareToTestValue ns ms = (sort ns) == (sort ms)


testParse :: (Either ParseError [Pure]) -> [String]
testParse (Left p) = ["Sorry looks like there is a parse error:", show p]
testParse (Right ps) = testValues ps fullTest

writeTestResultsToFile :: [String] -> IO ()
writeTestResultsToFile s = writeFile "1_Coffee_test_result.txt" (unlines s)

main :: IO ()
main = do
        p <- parsedRun
        writeTestResultsToFile (testParse p)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedRun :: IO (Either ParseError ParsedType)
parsedRun = parseFromFile usedParser "1_Coffee_test.pure"

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"


usedParser :: Parser ParsedType
usedParser = some (parsePure <* spaces) <* eof

parsePure :: Parser Pure
parsePure = Enum <$ string "Enum" <* spaces <*> parseName <* spaces <* (char '{') <* spaces <*> some (parseName <* (many (char ',')) <* spaces) <* (char '}')
        <|> Class <$ string "Class" <* spaces  <*> parseName <* spaces <* (char '{') <* spaces <*> some (parseValue) <* char '}'
        <|> Association <$ string "Association" <* spaces <*> parseName <* spaces <* (char '{') <* spaces <*> some (parseValue) <* char '}'

parseValue :: Parser (String, MetaType)
parseValue = (,) <$> parseWord <* string ": " <*> parseMetaType <* char ';' <* spaces

parseMetaType :: Parser MetaType
parseMetaType = (,) <$> parseName <* char '[' <*> some (parseEitherIntOrStar <* many (char '.')) <* char ']'

parseEitherIntOrStar :: Parser (Either Integer String)
parseEitherIntOrStar = Left <$> parseNum <|> Right <$> string "*"

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))

parseName :: Parser String
parseName = some (oneOf (':':'_':(['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])))

parseWord :: Parser String
parseWord = some (oneOf (['A'..'Z'] ++ ['a'..'z']))