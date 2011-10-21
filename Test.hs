import Hinatra

main :: IO ()
main = hinatra $ do
  get "/hi" $ do
    return "Hello, World!"
