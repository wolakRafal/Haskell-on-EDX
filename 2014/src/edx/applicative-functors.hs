{-main = do line <- getLine
			let line' = reverse line
			putStrLn $ "You said " ++ line' ++ " backwards!"
			putStrLn $ "Yes , You really said " ++ line' ++ " backwards!"
-}			
import Data.Char  
import Data.List  

--The user is prompted for a line and we give it back to the user, only reversed. Here's how to rewrite this by using fmap:
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!" 

main' = do
			line <- fmap (intersperse '-' . reverse . map toUpper) getLine
			putStrLn line
