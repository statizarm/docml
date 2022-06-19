module Tool.Link.Options where


import Data.List.Split

import Options.Applicative

import Tool.Unique.Options (identifierTagsParser)


data LinkOptions =
    LinkOptions { getIdentifierTags :: [String]
                , getLinkTags :: [String]
}
    deriving(Show, Eq, Ord)


linkTagsParser :: Parser [String]
linkTagsParser = 
    (
        concat <$>
            some (
                splitOn "," <$>
                strOption
                ( long "link-tag"
                <> short 'l'
                )
            )
        )
    <|>
    pure ["link"]



linkOptionsParser :: Parser LinkOptions
linkOptionsParser = LinkOptions
    <$> identifierTagsParser
    <*> linkTagsParser
