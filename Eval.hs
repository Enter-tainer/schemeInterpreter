module Eval where
import           Token
import           Control.Monad()
import qualified Data.Map.Strict               as M

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither s Nothing  = Left s

type Context = M.Map LToken ([LToken], LToken) -- actually, VarLit and it's definition
-- Map name (paraList, Defs)

bindPara :: [LToken] -> [LToken] -> Context -> Either String Context
bindPara [] _ ctx = return ctx
bindPara _ [] ctx = return ctx
bindPara (x:xs) (y:ys) ctx = do
  case y of
    (VarLit _) -> do
      (para, def) <- maybeToEither "No such variable or function" $ M.lookup y ctx
      if para == [] then do
        yv <- _evalE ctx y
        bindPara xs ys $ bindv x yv ctx
      else
        bindPara xs ys $ bindf x para def ctx
    (Par [Lambda, (Par para), def]) -> do
      bindPara xs ys $ bindf x para def ctx
    _ -> do
      yv <- _evalE ctx y
      bindPara xs ys $ bindv x yv ctx
  where
    bindv tk1 s vctx = M.insert tk1 ([], s) vctx
    bindf tk1 param def vctx = M.insert tk1 (param, def) vctx

getDef :: LToken -> Either String (LToken, [LToken], LToken)
getDef (Par [Define, (Par (name : para)), (Par x)]) = Right (name, para, Par x)
getDef (Par [Define, (Par (name : para)), x]) = Right (name, para, x)
getDef (Par [Define, (VarLit t), (Par x)]) = Right (VarLit t, [], Par x)
getDef (Par [Define, (VarLit t), x]) = Right (VarLit t, [], x)

getDef _ = Left "Not a definition"

isDef :: LToken -> Bool
isDef (Par (Define : _)) = True
isDef (Par [x         ]) = isDef x
isDef _                  = False
eval :: String -> Either String Int
eval str = do
  tk  <- tokenize str
  ctx <- mapM getDef $ filter isDef tk
  res <- _evalE (M.fromList $ transT <$> ctx) $ head $ filter (not . isDef) tk
  let NumLit r = res
  return r
    where
      transT (a, b, c) = (a, (b, c))
_evalE :: Context -> LToken -> Either String LToken

_evalE ctx (Par [Lambda, (Par para), x]) = error "not implemented"

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

_evalE ctx (Par [Greater  , x, y]) = do
  xv <- _evalE ctx x
  yv <- _evalE ctx y
  let (NumLit xx) = xv
  let (NumLit yy) = yv
  if xx > yy then return $ NumLit 1 else return $ NumLit 0

_evalE ctx (Par [LessEq, x, y]) = _evalE ctx (Par [Not, Par [Greater, x, y]])

_evalE ctx (Par [And   , x, y]) = do
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

_evalE ctx x@(VarLit _) = do
  (_, def) <- maybeToEither "No such variable" $ M.lookup x ctx
  val  <- _evalE ctx def
  return val

_evalE ctx (Par (name@(VarLit _) : paraList)) = do
  (para, def) <- maybeToEither "No such function" $ M.lookup name ctx
  newCtx <- bindPara para paraList ctx
  _evalE newCtx def

_evalE ctx (Par    [x]) = _evalE ctx x
_evalE _   (NumLit x  ) = return $ NumLit x
_evalE _   _            = Left "runtime error"
