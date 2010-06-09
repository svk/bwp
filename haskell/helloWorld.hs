import IO

main = do
         hSetBuffering stdout NoBuffering            
         putStr   "Enter an integer: "        
         x1 <- readNum 
         putStr   "Enter another integer: "          
         x2 <- readNum                          
         putStr  ("Their sum is " ++ show (x1+x2) ++ "\n")
       where readNum :: IO Integer
-- Providing a type signature avoids reliance on
-- the defaulting rule to fix the type of x1,x2
             readNum = readLn
