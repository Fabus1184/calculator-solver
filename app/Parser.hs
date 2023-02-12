module Parser (parseButtons) where

import Data.Functor (($>), (<&>))
import Text.ParserCombinators.ReadP (ReadP, char, choice, munch1, readP_to_S, sepBy1, skipSpaces, string)

parseButtons :: String -> Maybe [Button]
parseButtons s =
    case (readP_to_S $ char '[' *> sepBy1 (skipSpaces *> parseButton <* skipSpaces) (char ',') <* char ']') s of
        [(xs, "")] -> Just xs
        _ -> Nothing

parseButton :: ReadP Button
parseButton =
    choice
        [ parsePlus
        , parseMinus
        , parseMult
        , parseDiv
        , parseRev
        , parseShl
        , parseN
        , parseReplace
        , parseSum
        , parsePow
        , parseRotR
        , parseRotL
        , parseMirror
        , parseMetaPlus
        , parseStore
        , parseInv10
        ]

positiveDecimal :: ReadP Int
positiveDecimal = read <$> munch1 isDigit

parsePlus :: ReadP Button
parsePlus = (char '+' >> skipSpaces >> positiveDecimal) <&> Plus

parseMinus :: ReadP Button
parseMinus = (char '-' >> skipSpaces >> positiveDecimal) <&> Minus

parseMult :: ReadP Button
parseMult = (char '*' >> skipSpaces >> positiveDecimal) <&> Mult

parseDiv :: ReadP Button
parseDiv = (char '/' >> skipSpaces >> positiveDecimal) <&> Div

parseRev :: ReadP Button
parseRev = string "Rev" $> Rev

parseShl :: ReadP Button
parseShl = string "<<" $> Shl

parseN :: ReadP Button
parseN = munch1 isDigit <&> N . read

parseReplace :: ReadP Button
parseReplace = do
    a <- positiveDecimal
    skipSpaces
    string "=>"
    skipSpaces
    Replace (show a) . show <$> positiveDecimal

parseSum :: ReadP Button
parseSum = string "Sum" $> Sum

parsePow :: ReadP Button
parsePow = (char '^' >> skipSpaces >> munch1 isDigit) <&> Pow . read

parseRotR :: ReadP Button
parseRotR = string ">" $> RotR

parseRotL :: ReadP Button
parseRotL = string "<" $> RotL

parseMirror :: ReadP Button
parseMirror = string "Mirror" $> Mirror

parseMetaPlus :: ReadP Button
parseMetaPlus = string "[+]" >> skipSpaces >> munch1 isDigit <&> MetaPlus . read

parseStore :: ReadP Button
parseStore = string "Store" $> Store

parseInv10 :: ReadP Button
parseInv10 = string "Inv10" $> Inv10
