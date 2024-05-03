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
importingValue = sort [Class "coffee::model::Bean" [("BagId", ("String", [Left 1])), ("Roast", ("String", [Left 1])), ("OriginLocation", ("String", [Left 1]))]]

classesValue :: [Pure]
classesValue = sort [Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("String",[Left 1])),("OriginLocation",("String",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))]]

enumsValue :: [Pure]
enumsValue = sort [Enum "coffee::model::RoastStrength" ["Light","Dark","Raw"],Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("coffee::model::RoastStrength",[Left 1])),("OriginLocation",("String",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))]]

associationValue :: [Pure]
associationValue = sort [Enum "coffee::model::RoastStrength" ["Light","Dark","Raw"],Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("coffee::model::RoastStrength",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))],Association "coffee::model::BeanLocation" [("Bean",("coffee::model::Bean",[Left 1,Right "*"])),("Location",("coffee::model::Location",[Left 1]))]]

companyValue :: [Pure]
companyValue = sort [Enum "coffee::model::RoastStrength" ["Light","Dark","Raw"],Class "coffee::model::Bean" [("BagId",("String",[Left 1])),("Roast",("coffee::model::RoastStrength",[Left 1]))],Class "coffee::model::Location" [("Name",("String",[Left 1])),("Country",("String",[Left 1]))],Class "coffee::model::Company" [("CompanyId",("Integer",[Left 1])),("Roaster",("Boolean",[Left 1]))],Association "coffee::model::BeanLocation" [("Bean",("coffee::model::Bean",[Left 1,Right "*"])),("Location",("coffee::model::Location",[Left 1]))],Association "coffee::model::CompanyBeans" [("Company",("coffee::model::Company",[Left 1])),("Bean",("coffee::model::Bean",[Right "*"]))]]

fullTest :: Int -> (String, [Pure])
fullTest 1 = ("Test Testing:", importingValue)
fullTest 2 = ("Classes Exercise:", classesValue)
fullTest 3 = ("Enums Exercise:", enumsValue)
fullTest 4 = ("Associations Exercise:", associationValue)
fullTest 6 = ("Company Exercise:", companyValue)

testValues :: [Pure] -> (String, [Pure]) -> [String]
testValues input (s, testCase) = [s] ++ (onEmptyShowSuccess (concat (compareToTestValue input testCase)))

onEmptyShowSuccess :: [String] -> [String]
onEmptyShowSuccess s | concat s == [] = ["Successfully passed well done!!!"]
                     | otherwise = s

compareToTestValue :: [Pure] -> [Pure] -> [[String]]
compareToTestValue (i:is) (e:es) = comparePure i e : (compareToTestValue is es)
compareToTestValue _ _ = []

comparePure :: Pure -> Pure -> [String]
comparePure (Enum name as) (Enum expectedName bs) = "" : concat (compareNames "enum" name expectedName  : zipWith (compareNames "enum value") as bs)
comparePure (Class name as) (Class expectedName bs) = "" : concat (compareNames "class" name expectedName : comparePropLength expectedName (length as) (length bs) : zipWith (compareProperty expectedName) (sort as) (sort bs))
comparePure (Association name as) (Association expectedName bs) = "" : concat (compareNames "association" name expectedName : comparePropLength expectedName (length as) (length bs) : zipWith (compareProperty expectedName) (sort as) (sort bs))
comparePure _ (Enum name _) = "" : [("Expected an enum with name: " ++ name)]
comparePure _ (Class name _) = "" : [("Expected a class with name: " ++ name)]
comparePure _ (Association name _) = "" : [("Expected an association with name: " ++ name)]

comparePropLength :: String -> Int -> Int -> [String]
comparePropLength s n m | n == m = []
                        | otherwise = ["Class " ++ s ++ " has incorrect number of properties"]

compareNames :: String -> String -> String -> [String]
compareNames t s1 s2 | s1 == s2 = []
                     | otherwise = ["Was expecting " ++ t ++ " to be " ++ s2 ++ " instead found " ++ s1]

compareProperty :: String -> (String, MetaType) -> (String, MetaType) -> [String]
compareProperty tName (name, meta) (expectedName, expectedMeta) | name == expectedName = (compareMeta (expectedName ++ " property on " ++ tName) meta expectedMeta)
                                                                | otherwise = compareNames ("property on " ++ tName) name expectedName

compareMeta :: String -> MetaType -> MetaType -> [String]
compareMeta info (t, multiplicty) (et, expectedMultiplicity) | t == et = compareMultiplicty info multiplicty expectedMultiplicity
                                                             | otherwise = compareNames ("type of " ++ info) t et

compareMultiplicty :: String -> [Either Integer String] -> [Either Integer String] -> [String]
compareMultiplicty info as bs | as /= bs = ["Multiplicity of " ++ info ++ " was incorrect"]
                              | otherwise = []

testParse :: (Either ParseError [Pure]) -> [String]
testParse (Left p) = ["Sorry looks like there is a parse error:", show p]
testParse (Right ps) = testValues (sort ps) (fullTest (length ps))

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