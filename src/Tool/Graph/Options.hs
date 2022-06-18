module Tool.Graph.Options where


import Options.Applicative


data GraphOptions = GraphOptions {
    getOutput :: String
} deriving(Show, Eq, Ord)


graphOptionsParser :: Parser GraphOptions
graphOptionsParser = GraphOptions
        <$> strOption
        ( long "output"
        <> short 'o'
        <> value "stdout"
        <> showDefault
        )
