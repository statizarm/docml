module Tool.Unique.Options where


import Options.Applicative


data UniqueOptions = UniqueOptions
    deriving(Show, Eq, Ord)


uniqueOptionsParser :: Parser UniqueOptions
uniqueOptionsParser = pure UniqueOptions
