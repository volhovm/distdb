import qualified ClientOptions as O

main :: IO ()
main = do
    o@O.ClientOptions{..} <- O.getClientOptions
    putStrLn $ "Called with options: " ++ show o
