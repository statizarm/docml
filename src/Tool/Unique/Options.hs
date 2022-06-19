module Tool.Unique.Options where


import Options.Applicative hiding (option)
import Language.Token (identifier)

import Data.List.Split


data UniqueOptions = UniqueOptions {
    getIdentifierTags :: [String]
}
    deriving(Show, Eq, Ord)


identifierTagsParser :: Parser [String]
identifierTagsParser = 
    (
        concat <$>
            some (
                splitOn "," <$>
                strOption
                ( long "identifier-tag"
                <> short 'i'
                )
            )
        )
    <|>
    pure ["identifier", "header", "enum_value", "enum_name"]


uniqueOptionsParser :: Parser UniqueOptions
uniqueOptionsParser = UniqueOptions
    <$> identifierTagsParser
