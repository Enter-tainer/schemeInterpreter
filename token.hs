module Token where
import Data.Char
data LToken = NumLit Int | VarLit String | Equal | Plus | Minus | Multi | Divide | Define | If | Cond | Lambda | Else | Let | And | Or | Not | Par [LToken] deriving (Eq, Show)

_splitP :: String -> String -> Int -> (String, String)
_splitP ('(' : xs) str cnt = _splitP xs (str ++ ['(']) $ cnt + 1
_splitP (')' : xs) str 1   = (str ++ [')'], xs)
_splitP (')' : xs) str cnt = _splitP xs (str ++ [')']) $ cnt - 1
_splitP (x   : xs) str cnt = _splitP xs (str ++ [x]) cnt
_splitP []         str _   = (str, [])

splitP :: String -> Either String (String, String)
splitP s@('(' : _) = return $ _splitP s "" 0
splitP _           = Left "First parameter should start with ("

readToken :: String -> (String, String)
readToken = span (\x -> not (isSpace x || isP x))
  where
    isP s = s == '(' || s == ')'

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

getToken :: String -> LToken
getToken s
  | ts == "+" = Plus
  | ts == "-" = Minus
  | ts == "*" = Multi
  | ts == "/" = Divide
  | ts == "=" = Equal
  | ts == "define" = Define
  | ts == "if" = If
  | ts == "cond" = Cond
  | ts == "lambda" = Lambda
  | ts== "else" = Else
  | ts == "let" = Let
  | ts == "and" = And
  | ts == "or" = Or
  | ts == "not" = Not
  where ts = trim s
getToken num
  | all isNumber num = NumLit $ read num
  | otherwise = VarLit num


tokenize :: String -> Either String [LToken]
tokenize []            = Right []
tokenize (    ' ' : s) = tokenize s
tokenize str@('(' : _) = do
  (h, t) <- splitP str
  hres   <- tokenize $ tail $ init $ h
  tres   <- tokenize t
  return $ (Par hres):tres
tokenize str = do
  let (h, t) = readToken str
  let s = getToken h
  remain <- tokenize t
  return $ s:remain
