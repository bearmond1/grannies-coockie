module Main (main) where



import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp (run)
import           API


connString :: ConnectionString
connString = "host=localhost port=5432 user=postgres dbname=postgres password=mysecretpassword"



main :: IO ()
main = print "Hello" >> ( run 3000 $ app connString)
