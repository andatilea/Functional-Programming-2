module Main where
-- Data.Char will be used for the validation of the password in 5.2.2 exercise.
import Data.Char

--5.2.1

-- create a single list with all the digits, lowercase and uppercase letters.
c_list = concat [['0'..'9'], ['a'..'z'], ['A'..'Z']]

passwords :: [String]
-- we will replicate the c_list obtaining a new list of length 8 with the same element containing all characters.
-- using sequenceA we will generate all the possible character passwords.
passwords = sequenceA $ replicate 8 $ c_list

data User = User {email :: Email, password :: Password} deriving (Show, Eq)

data Password = Password String deriving (Show, Eq)

data Email = Email {username :: String, domain:: String} deriving (Show, Eq)

--5.2.2

-- helper for finding if a given string will contain at least one lowercase character.
check_lower :: String -> Bool
check_lower "" = False
check_lower s = any isLower s

-- helper for finding if a given string will contain at least one uppercase character.
check_upper :: String -> Bool
check_upper "" = False
check_upper s = any isUpper s

-- helper for finding if a given string will contain at least one digit.
check_digit :: String -> Bool
check_digit "" = False
check_digit s = any isDigit s

-- function for validating the password.
validatePassword :: String -> Maybe Password
validatePassword "" = Nothing
-- check for the given string
validatePassword s =  if check_lower s && check_upper s && check_digit s && (length s >= 8)
                      -- if all conditions are satisfied then return the valid password.
                      then Just $ Password s
                      -- if the string isn't a valid password -> return Nothing.
                      else Nothing

--5.2.3

-- helper for extracting the username and also checking if its length is at least 3 characters long.
check_username :: String -> Bool
check_username str = if length (takeWhile (\x -> x /= '@') str) >= 3 then True
                     else False

-- helper for checking if the input string contains exactly one '@' character.
check_char :: String -> Bool
check_char str = if length (filter (\x -> x == '@') str) == 1 then True
                 else False

-- helper for extracting the domain.
get_domain :: String -> String
get_domain str = (dropWhile (\x -> x == '@') $ dropWhile (\x -> x /= '@') str)

-- helper for making sure the domain is valid (valid hostname, the '.' character and the top level domain (com)).
check_domain :: String -> Bool
check_domain str =    -- firstly, we extract the hostname from the domain and check it not to be empty (len > 0).
                      -- we know that the last 4 characters of the domain must always be ".com".
                      if length (take (length (get_domain str) - 4) $ get_domain str) > 0 then
                       -- if the last 4 characters from the domain are ".com" then it is valid
                        if (drop (length (get_domain str) - 4) $ get_domain str) == ".com"
                        then True
                        -- else it is not a valid domain.
                        else False
                      else False

validateEmail :: String -> Maybe Email
validateEmail "" = Nothing
validateEmail s = if check_username s && check_char s && check_domain s
                  -- if all conditions are satisfied then the email address is valid.
                  then Just Email {username = (takeWhile(\x -> x/= '@') s), domain = get_domain s}
                  -- if the string isn't a valid email -> return Nothing.
                  else Nothing

--5.2.4
validateUser :: String -> String -> Maybe User
-- if both validators return a valid Email and a valid Password then the User is a valid one.
validateUser email password = if validateEmail email /= Nothing && validatePassword password /= Nothing
                              -- if both conditions are satisfied then the user is valid
                              then Just User {email = Email {username = (takeWhile(\x -> x/= '@') email),
                                                  domain = get_domain email},
                                              password = Password password }
                              -- if the two strings are not a valid email and a valid password -> return Nothing.
                              else Nothing

main :: IO ()
main = do
           putStrLn "Please enter the Email:"
           email <- getLine
           putStrLn "Please enter the password:"
           password <- getLine
           -- check if the current user is a valid one.
           if validateUser email password /= Nothing
           -- if the user is valid -> then the email and the password are both valid.
           then putStrLn "VALID Input"
           -- if the user is not valid -> the email and/or the password are not valid.
           else putStrLn "The Input is NOT VALID"