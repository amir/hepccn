import Lib

main :: IO ()
main = putStrLn $ show $ regularParse connectionParser "F800A8C0:ABBE 85106597:01BB 01\n"
