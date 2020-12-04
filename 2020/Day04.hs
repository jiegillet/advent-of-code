module Day04 where

import Data.Char (isDigit, isHexDigit)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.Parsec
  ( Parsec,
    endBy,
    many,
    newline,
    noneOf,
    parse,
    sepBy,
    space,
    string,
    try,
    (<|>),
  )

data Passport = Passport
  { birthYear :: String,
    issueYear :: String,
    expirationYear :: String,
    height :: String,
    hair :: String,
    eye :: String,
    passportId :: String,
    countryId :: Maybe String
  }
  deriving (Show)

data Field
  = BirthYear String
  | IssueYear String
  | ExpirationYear String
  | Height String
  | Hair String
  | Eye String
  | PassportId String
  | CountryId String
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  passports <- readFile "Day04.txt"
  let Right pass = parse passportsP "passwordP" passports
  print $ length pass
  print $ length $ filter valid pass

passportsP :: Parsec String () [Passport]
passportsP = catMaybes <$> sepBy passportP newline

passportP :: Parsec String () (Maybe Passport)
passportP = do
  fields <-
    endBy
      (birthYearP <|> issueYearP <|> passportIdP <|> countryIdP <|> (try hairP <|> heightP) <|> (try expirationYearP <|> eyeP))
      (space <|> newline)
  makePassport $ sort fields
  where
    makePassport [BirthYear byr, IssueYear iyr, ExpirationYear eyr, Height hgt, Hair hcl, Eye ecl, PassportId pid, CountryId cid] =
      return $ Just $ Passport byr iyr eyr hgt hcl ecl pid (Just cid)
    makePassport [BirthYear byr, IssueYear iyr, ExpirationYear eyr, Height hgt, Hair hcl, Eye ecl, PassportId pid] =
      return $ Just $ Passport byr iyr eyr hgt hcl ecl pid Nothing
    makePassport _ = return Nothing

birthYearP, issueYearP, expirationYearP, heightP, hairP, eyeP, passportIdP, countryIdP :: Parsec String () Field
birthYearP = BirthYear <$> (string "byr:" >> many (noneOf " \n"))
issueYearP = IssueYear <$> (string "iyr:" >> many (noneOf " \n"))
expirationYearP = ExpirationYear <$> (string "eyr:" >> many (noneOf " \n"))
heightP = Height <$> (string "hgt:" >> many (noneOf " \n"))
hairP = Hair <$> (string "hcl:" >> many (noneOf " \n"))
eyeP = Eye <$> (string "ecl:" >> many (noneOf " \n"))
passportIdP = PassportId <$> (string "pid:" >> many (noneOf " \n"))
countryIdP = CountryId <$> (string "cid:" >> many (noneOf " \n"))

valid :: Passport -> Bool
valid (Passport byr iyr eyr hgt hcl ecl pid _) =
  all isDigit byr && 1920 <= read byr && read byr <= 2002
    && all isDigit iyr
    && 2010 <= read iyr
    && read iyr <= 2020
    && all isDigit eyr
    && 2020 <= read eyr
    && read eyr <= 2030
    && validHeight
    && head hcl == '#'
    && length hcl == 7
    && all isHexDigit (tail hcl)
    && ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    && length pid == 9
    && all isDigit pid
  where
    validHeight
      | unit == "cm" = 150 <= read height && read height <= 193
      | unit == "in" = 59 <= read height && read height <= 76
      | otherwise = False
      where
        (height, unit) = span isDigit hgt
