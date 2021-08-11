module Passport where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

newtype BirthYear
  = BirthYear Int

derive newtype instance showBirthYear :: Show BirthYear

newtype IssueYear
  = IssueYear Int

derive newtype instance showIssueYear :: Show IssueYear

newtype ExpirationYear
  = ExpirationYear Int

derive newtype instance showExpirationYear :: Show ExpirationYear

data Height
  = Cm Int
  | In Int

instance showHeight :: Show Height where
  show (Cm h) = show h <> "cm"
  show (In h) = show h <> "in"

newtype HairColor
  = HairColor String

derive newtype instance showHairColor :: Show HairColor

data EyeColor
  = Amb
  | Blu
  | Brn
  | Gry
  | Grn
  | Hzl
  | Oth

derive instance genericEyeColor :: Generic EyeColor _

instance showEyeColor :: Show EyeColor where
  show = genericShow

newtype PassportId
  = PassportId String

derive newtype instance showPassportId :: Show PassportId

newtype CountryId
  = CountryId Int

derive newtype instance showCountryId :: Show CountryId

type Passport
  = { byr :: BirthYear
    , iyr :: IssueYear
    , eyr :: ExpirationYear
    , hgt :: Height
    , hcl :: HairColor
    , ecl :: EyeColor
    , pid :: PassportId
    , cid :: Maybe CountryId
    }

passport :: BirthYear -> IssueYear -> ExpirationYear -> Height -> HairColor -> EyeColor -> PassportId -> Maybe CountryId -> Passport
passport byr iyr eyr hgt hcl ecl pid cid = { byr, iyr, eyr, hgt, hcl, ecl, pid, cid }
