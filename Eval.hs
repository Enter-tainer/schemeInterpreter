module Eval where
import           Token
import           Control.Monad

type Context = [(LToken, [LToken])] -- actually, VarLit and it's definition

getDef :: LToken -> Either String (LToken, [LToken])
getDef (Par [Define, (VarLit t), (Par x)]) = Right (VarLit t, x)
getDef (Par [Define, (VarLit t), x]) = Right (VarLit t, [x])
getDef _ = Left "Not a definition"

isDef :: LToken -> Bool
isDef (Par (Define : _)) = True
isDef (Par [x         ]) = isDef x
isDef _                  = False

eval :: String -> Either String Int
eval str = do
  tk  <- tokenize str
  ctx <- mapM getDef $ filter isDef tk
  res <- _evalE ctx $ head $ filter (not . isDef) tk
  let NumLit r = res
  return r


_evalE :: Context -> LToken -> Either String LToken
_evalE ctx (Par [Plus, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  return $ NumLit $ (xx + yy)
_evalE ctx (Par [Minus, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  return $ NumLit $ (xx - yy)
_evalE ctx (Par [Multi, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  return $ NumLit $ (xx * yy)
_evalE ctx (Par [Divide, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  return $ NumLit $ (xx `div` yy)

_evalE ctx (Par [Equal, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  if xv == yv then return $ NumLit 1 else return $ NumLit 0

_evalE ctx (Par [NotEq, x, y]) = _evalE ctx (Par [Not, Par [Equal, x, y]])

_evalE ctx (Par [Less , x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  if xx < yy then return $ NumLit 1 else return $ NumLit 0

_evalE ctx (Par [GreaterEq, x, y]) = _evalE ctx (Par [Not, Par [Less, x, y]])

_evalE ctx (Par [Greater, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  if xx > yy then return $ NumLit 1 else return $ NumLit 0

_evalE ctx (Par [LessEq, x, y]) = _evalE ctx (Par [Not, Par [Greater, x, y]])

_evalE ctx (Par [And, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  if xv /= NumLit 0 && yv /= NumLit 0
    then return $ NumLit 1
    else return $ NumLit 0

_evalE ctx (Par [Or, x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  if xv /= NumLit 0 || yv /= NumLit 0
    then return $ NumLit 1
    else return $ NumLit 0
_evalE ctx (Par [Not, x]) = do
  xv <- _evalE ctx x
  if xv == NumLit 0 then return $ NumLit 1 else return $ NumLit 0
_evalE ctx (Par [If, c, x, y]) = do
  cv <- _evalE ctx c
  if cv /= NumLit 0 then _evalE ctx x else _evalE ctx y
_evalE ctx (Par    [x]) = _evalE ctx x
_evalE _   (NumLit x  ) = return $ NumLit x
_evalE _   _            = undefined
