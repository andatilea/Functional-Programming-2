-----------------------
-- Anda-Corina Tilea
-- 30.11.2020
-----------------------
import Data.List (intercalate, sortBy)

-- helper implemented for obtaining the length of a list of type a;
len :: [a] -> Int
len l =  case l of
                 -- if the list is empty -> 0 elements;
                 [] -> 0
                 -- otherwise, we add 1 for each existing element;
                 x:xs -> (+1) $ len xs


strWords :: String -> [String]
-- if the string is empty -> obtain an empty list;
strWords "" =  []
strWords str = let
                    -- if the first character of the string is a space we remove it;
                    del_space = dropWhile (\x -> x == ' ') str
                    -- then we take a word from the string;
                    word = takeWhile (\x -> x /= ' ') del_space
                    -- we declare the remaining string;
                    s = drop (len word) del_space
               in
                    -- recursively append each word to the list;
                    word : strWords (dropWhile (\x -> x == ' ') s)


-- helper implemented to check whether a character is a vowel or not;
isVowel :: Char -> Bool
isVowel c = if elem c ['a', 'e','i','o','u', 'A', 'E', 'I', 'O', 'U']
            -- if the char is an element from the vowels list then we return True;
            then True
            -- otherwise, we return False;
            else False

-- helper implemented to transform a given word into the pig latin format;
toPigLatin :: String -> String
toPigLatin str =
                 case str of
                 -- if the string is empty -> return an empty string;
                 "" -> ""
                 y:ys ->
                        -- if the first letter is a vowel -> we have "hay" added to the end;
                        if isVowel y then str ++ "-hay"
                        -- if the first letter is a consonant -> move it to the end and add "ay";
                        else ys ++ "-" ++ [y] ++ "ay"

piglatinize :: String -> String
piglatinize str = case str of
                  -- if the string is empty -> return an empty string;
                  ""  -> ""
                  otherwise -> -- apply the helper on the list of words (obtained with strWords);
                               helper (strWords str)
                               where
                                   helper :: [String] -> String
                                   -- if the list is empty -> return an empty string;
                                   helper [] = ""
                                   helper (x:xs) =  -- if the remaining list is not empty
                                                    if (xs /= [])
                                                    -- we can intercalate the elements;
                                                    then intercalate " " [toPigLatin x, helper xs]
                                                    -- if xs is empty ->
                                                    -- it means only one word left so we just apply toPigLatin on it;
                                                    else (toPigLatin x)


-- declare the iteration function;
iter :: (a -> a) -> a -> [a]
iter f x = x : (map f (iter f x))

-- declare the stopping criteria;
stop_condition :: [Double] -> Double
-- we will stop when the difference (in absolute value) between 2 successive terms equals 0;
stop_condition (x:y:xs) =
                          -- if the difference is 0, it means the elements are equal;
                          if (abs (x-y) == 0) then y
                          -- otherwise, we keep going;
                          else stop_condition (y:xs)

apprPi :: Double
apprPi =
  let
    nextPi :: Double -> Double
    -- declare the formula used for approximation;
    nextPi x = x + ((2 * cos ( x / 2)) / ((2 * sin (x / 2)) - 1))
  in
    -- use the functions to approximate minus pi, starting from the value 0;
    stop_condition (iter (nextPi) 0)


-- Implement:

-- the update function for option 1

--update :: (Eq k) => (v -> v) -> v -> k -> [(k, v)] -> [(k, v)]
--update _ _ _ _ = error "Implement this function"

-- OR

-- the uniques, countOccurrences and countWords functions for option 2

-- function which helps us obtain the unique elements from a list;
uniques :: (Eq a) => [a] -> [a]
uniques l = case l of
            -- if the list is empty -> return an empty list;
            [] -> []
            (x:xs) ->   -- recursively append the elements to the filtered list;
                        x : uniques (filter (\m -> m /= x) xs)


-- function which counts how many times an element occurs in a list;
countOccurrences :: (Eq a) => a -> [a] -> Int
-- we use the "len" function declared at the beginning;
countOccurrences x l = len $ filter (\m -> m == x) l

-- obtain the list of tuples;
countWords :: String -> [(String, Int)]
countWords str =    let
                        -- obtain the list of unique words using strWords function;
                        lst =  uniques $ strWords str
                    in
                        -- create the tuples;
                        map (\x -> (x, countOccurrences x $ strWords str)) lst


-- helper implemented for ordering by word-count and then lexicographically;
helper :: (String, Int) -> (String, Int) -> Ordering
-- w  = words, c = counts;
helper (w1, c1) (w2, c2) = if c1 < c2 then GT           -- w1 in front of w2;
                           else if c1 > c2 then LT      -- w1 after w2;
                           else compare w1 w2           -- if w1 == w2 then we compare the words;


-- Implement topWords using the functions implemented above.

topWords :: Int -> String -> [(String, Int)]
topWords i str = take i $ sortBy helper $ countWords str