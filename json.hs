-- We need these for Show JSON
import Data.List (foldl', intersperse)
-- Used when parsing unicode escape sequences
import Data.Char (chr, ord)
-- Used to represent JS Assoiciative arrays
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Char

-- This is our data structure to represent JSON
data JSON = JsonNum Double
          | JsonStr String
          | JsonArr [JSON] -- An array
          | JsonAA (Map.Map String JSON) -- An associative array
          | JsonBool Bool
          | JsonNull
    deriving (Eq, Ord)

-- Collapse a JSON document to a string
-- If this confuses you, just accept it as magic.
instance Show JSON where
    showsPrec _ = showsJSON
      where commaJoin = foldl' (.) id . intersperse (", "++)
            showsAssoc (field, val) = shows field . (": "++) . shows val
            showsJSON (JsonNum num)  = shows num
            showsJSON (JsonStr str)  = shows str
            showsJSON (JsonArr jarr) =
                ('[':) . commaJoin (map shows jarr) . (']':)
            showsJSON (JsonAA aa)    =
                ('{':) . commaJoin (map showsAssoc $ Map.toList aa) . ('}':)
            showsJSON (JsonBool tf)  =
                if tf then ("true"++) else ("false"++)
            showsJSON JsonNull       = ("null"++)

-- The base parser
jsonParser = do
    spaces -- handle whitespace
    -- Each of these parses the corresponding data type
    -- E.g. jsonPStr -> JsonStr
    json <- jsonPNum
        <|> jsonPStr
        <|> jsonPBool
        <|> jsonPNull
        <|> jsonPArr
        <|> jsonPAA
        <?> "value" -- Otherwise report we expected a value
    spaces
    -- return for monads means something very different than
    -- + procedural languages.
    -- Simply put, it wraps the value in the monad
    return json

-- These parsers try to collect a literal as a string.
-- Note that the try is not strictly necessary, but this is a good example
-- + of when one might use it (Consider if there was also a "none" value)
jsonPNull = try (string "null") >> return JsonNull

jsonPBool = fmap JsonBool
    $ (try (string "true") >> return True)
    <|> (try (string "false") >> return False)

-- Note how this parser splits the problem into parts
jsonPNum = do
    -- We return a function that might negate our result later
    doNeg <- (char '-' >> return negate) <|> return id
    -- Every number can 
    realPart <- realP
    fracPart <- fracP
    expPart  <- expP
    -- We put all the pieces together and wrap it into a JSON type
    return $ JsonNum $ doNeg $ (realPart + fracPart) * 10 ** expPart
        -- This is the real part.  It must exist.
  where realP = (char '0' >> return 0) <|> (do
                -- It's either just a 0, or begins with 1 - 9
                firstDigit <- satisfy (\ ch -> '1' <= ch && ch <= '9')
                -- after that it can be any digit
                restDigits <- many digit
                return $ read $ firstDigit : restDigits
            )
        fracP = (do
                char '.'
                digits <- many1 digit
                return $ read $ "0." ++ digits
            )
            -- This parser will fail if there's no dot
            -- That means there's no fractional part, so we default to 0
            <|> return 0
        expP = (do 
            oneOf "eE"
            -- This is similar to the other doNeg, but we have an 
            -- + extra option
            doNeg <- (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id
            digits <- many1 digit
            return $ doNeg $ read digits
            )
            -- Similarly, we default to 0 if there's no exponent
            <|> return 0
            
jsonStr = do 
    char '\"'
    strChar `manyTill` (char '\"')
        -- This is a list of substutions to make after a \
  where subMap = Map.fromList
            [ ('\"', '\"')
            , ('\\', '\\')
            , ('/', '/')
            , ('n', '\n')
            , ('r', '\r')
            , ('f', '\f')
            , ('t', '\t')
            , ('b', '\b')
            ]
        -- Parses a 
        unicode = do
            -- get 4 hexadecimal digits
            code <- count 4 hexDigit
            if null code -- The docs say this is possible
              then fail $ "expecting unicode"
              -- Otherwise, just read the values
              else return $ chr $ read $ "0x" ++ code
        -- This acts like aanyChar, but will try to escape first
        strChar = (do
            char '\\'
            ch <- anyChar
            -- search for the correct substitution
            case Map.lookup ch subMap of
                Just newCh -> return newCh
                Nothing -> do 
                    if ch == 'u' -- It might yet be unicode
                      then unicode
                      else fail "expecting escape sequence"
            )
            -- If we don't have an escape sequence, just return the char
            <|> anyChar

-- This just wraps the String result of jsonStr into a JSON
jsonPStr = fmap JsonStr jsonStr

-- Arrays are real simple at this point.
-- Just get a comma seperated list of JSON values between brackets
jsonPArr = do
    char '['
    jarr <- jsonParser `sepBy` (char ',')
    char ']'
    return $ JsonArr $ jarr

-- Associative arrays are almost as simple, except we need to teach it
-- + how to parse fields
jsonPAA = do
    char '{'
    jAA <- assocP `sepBy` char ',' -- returns a list of (key, value) tuples
    char '}'
    return $ JsonAA $ Map.fromList $ jAA
        -- This should seem really straight forward.
        -- We get the label followed by a colon and its corresponding
        -- + JSON value
  where assocP = do
            spaces
            label <- jsonStr
            spaces
            char ':'
            json <- jsonParser
            return (label, json)

-- A real simple interactive program to test
main = do
    line <- getLine
    case parse jsonParser "" line of
        Right json -> do 
            print json
            main
        Left err -> print err
