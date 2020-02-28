module Eval where
import Token
import Control.Monad

type Context = [(LToken, [LToken])] -- actually, VarLit and it's definition

getDef :: [LToken] -> (LToken, [LToken])
getDef [Define,(VarLit t),(Par x)] = (VarLit t, x)
getDef [Define,(VarLit t), x] = (VarLit t, [x])
getDef _ = undefined

eval :: String -> Either String Int
eval str = do
  tk <- tokenize str
  res <- _eval $ head tk
  let NumLit r = res
  return r

_eval :: LToken -> Either String LToken
_eval x = _evalE [] x
_evalE :: Context -> LToken -> Either String LToken
_evalC :: Context -> LToken -> Context

_evalC ctx (Par s@[Define, (VarLit _), _]) = getDef s : ctx
_evalC _ _ = undefined
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
_evalE ctx (Par [x]) = _evalE ctx x
_evalE ctx (NumLit x) = return $ NumLit x
_evalE _ _ = undefined
