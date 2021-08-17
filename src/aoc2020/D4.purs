module D4 where

import Prelude
import Data.Array (filter, length, (!!))
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Data.Validation.Semigroup (V, invalid, isValid)
import Effect (Effect)
import Parser (Parser(..), runParser)
import Passport (BirthYear(..), CountryId(..), ExpirationYear(..), EyeColor(..), HairColor(..), Height(..), IssueYear(..), Passport, PassportId(..), passport)
import ReadFile (readInput)
import Validation (Errors, matches, rangeIs)

birthYearRegex :: Regex
birthYearRegex = unsafeRegex "(byr):([0-9]+)" noFlags

issueYearRegex :: Regex
issueYearRegex = unsafeRegex "(iyr):([0-9]+)" noFlags

expirationYearRegex :: Regex
expirationYearRegex = unsafeRegex "(eyr):([0-9]+)" noFlags

heightRegex :: Regex
heightRegex = unsafeRegex "(hgt):([\\d]+)(cm|in)" noFlags

hairColorRegex :: Regex
hairColorRegex = unsafeRegex "(hcl):([0-9a-zA-Z#]+)" noFlags

hairColor2Regex :: Regex
hairColor2Regex = unsafeRegex "^#[0-9|a-f]{6}$" noFlags

eyeColorRegex :: Regex
eyeColorRegex = unsafeRegex "(ecl):([a-zA-Z]+)" noFlags

eyeColor2Regex :: Regex
eyeColor2Regex = unsafeRegex "^(amb|blu|brn|gry|grn|hzl|oth)$" noFlags

passportIdRegex :: Regex
passportIdRegex = unsafeRegex "(pid):([0-9]+)" noFlags

passportId2Regex :: Regex
passportId2Regex = unsafeRegex "^[0-9]{9}$" noFlags

countryIdRegex :: Regex
countryIdRegex = unsafeRegex "(cid):([0-9]+)" noFlags

anyField :: Regex -> Parser (Maybe (Array String))
anyField re =
  Parser
    ( \s -> case match re s of
        Just (NonEmptyArray kv) -> Just (sequence kv)
        Nothing -> Nothing
    )

extractValueAt :: Int -> Maybe (Array String) -> Maybe String
extractValueAt = (=<<) <<< flip (!!)

parseFieldValueAtByRe :: Int -> Regex -> String -> Maybe String
parseFieldValueAtByRe idx regex s = identity =<< runParser (extractValueAt idx <$> anyField regex) s

parseHeight :: String -> Maybe Height
parseHeight s = case parseFieldValueAtByRe 3 heightRegex s of
  Just "cm" -> Cm <$> (fromString =<< parseFieldValueAtByRe 2 heightRegex s)
  Just "in" -> In <$> (fromString =<< parseFieldValueAtByRe 2 heightRegex s)
  Just _ -> Nothing
  Nothing -> Nothing

parseEyeColor :: String -> Maybe EyeColor
parseEyeColor s = case parseFieldValueAtByRe 2 eyeColorRegex s of
  Just "amb" -> pure Amb
  Just "blu" -> pure Blu
  Just "brn" -> pure Brn
  Just "gry" -> pure Gry
  Just "grn" -> pure Grn
  Just "hzl" -> pure Hzl
  Just "oth" -> pure Oth
  Just _ -> Nothing
  Nothing -> Nothing

parsePassport :: String -> Maybe Passport
parsePassport input =
  passport <$> (BirthYear <$> (fromString =<< parseFieldValueAtByRe 2 birthYearRegex input))
    <*> (IssueYear <$> (fromString =<< parseFieldValueAtByRe 2 issueYearRegex input))
    <*> (ExpirationYear <$> (fromString =<< parseFieldValueAtByRe 2 expirationYearRegex input))
    <*> parseHeight input
    <*> (HairColor <$> parseFieldValueAtByRe 2 hairColorRegex input)
    <*> parseEyeColor input
    <*> (PassportId <$> parseFieldValueAtByRe 2 passportIdRegex input)
    <*> (Just $ CountryId <$> (fromString =<< parseFieldValueAtByRe 2 countryIdRegex input))

validateBirthYear :: BirthYear -> V Errors BirthYear
validateBirthYear (BirthYear byr) = BirthYear <$> rangeIs "Birth year" 1920 2002 byr

validateIssueYear :: IssueYear -> V Errors IssueYear
validateIssueYear (IssueYear iyr) = IssueYear <$> rangeIs "Issue year" 2010 2020 iyr

validateExpirationYear :: ExpirationYear -> V Errors ExpirationYear
validateExpirationYear (ExpirationYear eyr) = ExpirationYear <$> rangeIs "Expiration year" 2020 2030 eyr

validateHeight :: Height -> V Errors Height
validateHeight (Cm hgt)
  | 150 <= hgt && hgt <= 193 = pure $ Cm hgt

validateHeight (In hgt)
  | 59 <= hgt && hgt <= 76 = pure $ In hgt

validateHeight _ = invalid [ "Height is out of range." ]

validateHairColor :: HairColor -> V Errors HairColor
validateHairColor (HairColor hcl) = HairColor <$> matches "Hair color" hairColor2Regex hcl

validateEyeColor :: EyeColor -> V Errors EyeColor
validateEyeColor ecl = pure ecl

validatePassportId :: PassportId -> V Errors PassportId
validatePassportId (PassportId pid) = PassportId <$> matches "Passport id" passportId2Regex pid

noNeedValidate :: Maybe CountryId -> V Errors (Maybe CountryId)
noNeedValidate cid = pure cid

validatePassport :: Passport -> V Errors Passport
validatePassport p =
  passport <$> validateBirthYear p.byr
    <*> validateIssueYear p.iyr
    <*> validateExpirationYear p.eyr
    <*> validateHeight p.hgt
    <*> validateHairColor p.hcl
    <*> validateEyeColor p.ecl
    <*> validatePassportId p.pid
    <*> noNeedValidate p.cid

testInput :: String
testInput =
  """eyr:2029 ecl:blu byr:1989 cid:0
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"""

test2 :: String
test2 = show $ parsePassport testInput

part1 :: Effect String
part1 = do
  input <- readInput "./src/aoc2020/input/d4"
  pure $ show $ length $ filter isJust $ parsePassport <$> split (Pattern "\n\n") input

part2 :: Effect String
part2 = do
  input <- readInput "./src/aoc2020/input/d4"
  pure $ show $ length $ filter isValid $ validatePassport <$> (fromMaybe [] $ sequence $ filter isJust $ (parsePassport <$> split (Pattern "\n\n") input))
