-- January 30th, 2025

import Data.List  -- Import the Data.List module
import Data.Maybe -- Import the Data.Maybe module

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  


-- Cases 
-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result  
--                    ...  

-- Case Ex
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

grade :: Int -> String
grade score = case score of
    x | x >= 90 -> "A"
    x | x >= 80 -> "B"
    x | x >= 70 -> "C"
    _           -> "F"