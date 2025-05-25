{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Resolver
    ( ResolvedLispVal(..)
    , ResolverError(..)
    , resolve
    , resolve'
    , removePos
    ) where

import Verset

import Lisp (Pos(..), noPos)
import Lisp qualified as L
import LispParser qualified as P



data ResolvedLispVal
  = RlAtom !Pos Text
  | RlBool !Pos Bool
  | RlDefine !Pos !Text !ResolvedLispVal
  | RlDo !Pos ![ResolvedLispVal]
  | RlFuncCall !Pos !ResolvedLispVal ![ResolvedLispVal]
  | RlIf !Pos !ResolvedLispVal !ResolvedLispVal !ResolvedLispVal
  | RlInt !Pos Int
  | RlLambda !Pos ![(Pos, Text)] ![ResolvedLispVal]
  | RlLet !Pos !L.LetStyle ![((Pos, Text), (Pos, ResolvedLispVal))] ![ResolvedLispVal]
  | RlList !Pos [ResolvedLispVal]
  | RlNil !Pos
  | RlString !Pos Text
  deriving (Show, Eq)



data ResolverError
  = ReReserved !(Maybe Pos) !Text
  | ReResolverError !(Maybe Pos) !Text
  | ReTypeDeclaration !(Maybe Pos) !Text
  | ReTypeName !(Maybe Pos) !Text
  | ReUnsupported !(Maybe Pos) !Text
  deriving (Show)


resolve :: [P.ParsedLispVal] -> Either ResolverError [ResolvedLispVal]
resolve = traverse resolve'


resolve' :: P.ParsedLispVal -> Either ResolverError ResolvedLispVal
resolve' = resolveImpl True


resolveImpl :: Bool -> P.ParsedLispVal -> Either ResolverError ResolvedLispVal
resolveImpl isTopLevel lv =
  case lv of
    -- No change
    P.PlNil p -> Right $ RlNil p
    P.PlInt p v -> Right $ RlInt p v
    P.PlString p v -> Right $ RlString p v
    P.PlBool p v -> Right $ RlBool p v
    P.PlAtom p v -> Right $ RlAtom p v

    P.PlList p allVs -> do
      case allVs of
        (h: vs) ->
          case h of
            P.PlAtom _ "let" -> resolveLet p L.LetParallel vs
            P.PlAtom _ "lambda" -> resolveLambda p vs
            P.PlAtom _ "λ" -> resolveLambda p vs
            P.PlAtom _ "if" -> resolveIf p vs

            P.PlAtom _ "define" ->
              if isTopLevel
                then resolveDefine p vs
                else Left . ReResolverError (Just p) $ "define can only be used at top level"

            -- (list 1 2 3) => (1 2 3)
            P.PlAtom _ "list" -> do
              vs' <- traverse (resolveImpl False) vs
              Right $ RlList p vs'

            P.PlAtom _ "do" -> do
              vs' <- traverse (resolveImpl False) vs
              Right $ RlDo p vs'

            -- Everything else is a function call
            _ -> do
              f <- resolveImpl False h
              args <- traverse (resolveImpl False) vs
              Right $ RlFuncCall p f args


        [] -> do
          -- Empty list is empty list not nil
          Right $ RlList p []


resolveDefine :: Pos -> [P.ParsedLispVal] -> Either ResolverError ResolvedLispVal
resolveDefine pos vs' = do
  let err = "expected define in form (define name expr)\n"

  case vs' of
     [name1, val1] -> do
        name2 <- as' asAtom "atom" name1 $ Just (err <> "expected atom for name of define")
        val2 <- resolveImpl False val1
        pure $ RlDefine pos name2 val2

     _ ->
       Left . ReResolverError (Just pos) $ err <> "Invalid number of arguments for define"



resolveIf :: Pos -> [P.ParsedLispVal] -> Either ResolverError ResolvedLispVal
resolveIf pos vs' = do
  let err = "expected if in form (if condition then else)\n"

  case vs' of
     [cond1, then1, else1] -> do
        cond2 <- resolveImpl False cond1
        then2 <- resolveImpl False then1
        else2 <- resolveImpl False else1
        pure $ RlIf pos cond2 then2 else2

     _ ->
       Left . ReResolverError (Just pos) $ err <> "Invalid number of arguments for if"



resolveLambda :: Pos -> [P.ParsedLispVal] -> Either ResolverError ResolvedLispVal
resolveLambda pos vs' = do
  let err = "expected lambda in form (λ (param1 param2 ...) body...)\n"

  case vs' of
     (binding1 : body1) -> do
        params1 <- as' asList "list" binding1 $ Just (err <> "Expecting list of lambda parameters")
        params2 <- for params1 $ \p1 -> do
          p2 <- as' asAtom "param" p1 $ Just (err <> "expected lambda parameter as an atom")
          pure (P.getPos p1, p2)

        body2 <- traverse (resolveImpl False) body1
        pure $ RlLambda pos params2 body2

     _ ->
        Left . ReResolverError (Just pos) $ err <> "Invalid number of arguments for lambda"




resolveLet :: Pos -> L.LetStyle -> [P.ParsedLispVal] -> Either ResolverError ResolvedLispVal
resolveLet pos lstyle vs' = do
  let err = "expected let in form (let ( (name1 val1) (name2 val2) ...) body...)\n"
  case vs' of
     (binding1 : body1) -> do
        bindingList1 <- as' asList "list" binding1 $ Just "Expecting list of let bindings"
        bindingList2 <- traverse asBinding bindingList1

        body2 <- traverse (resolveImpl False) body1

        pure $ RlLet pos lstyle bindingList2 body2

     _ ->
       Left . ReResolverError (Just pos) $ err <> "Invalid number of arguments for let"

  where
    asBinding :: P.ParsedLispVal -> Either ResolverError ((Pos, Text), (Pos, ResolvedLispVal))
    asBinding v = do
      vs <- as' asList "list" v $ Just "expected let binding in form (name val)"
      case vs of
        (name1 : val1 : []) -> do
          name2 <- as' asAtom "atom" name1 $ Just "expected atom for name of let binding"
          val2 <- resolveImpl False val1
          pure ((P.getPos name1, name2), (getPos val2, val2))

        _ ->
          Left $ ReResolverError (Just pos) $ "Invalid number of arguments for let binding"



asList :: P.ParsedLispVal -> Maybe [P.ParsedLispVal]
asList (P.PlList _ xs) = Just xs
asList _ = Nothing

asAtom :: P.ParsedLispVal -> Maybe Text
asAtom (P.PlAtom _ a) = Just a
asAtom _ = Nothing


as' :: (P.ParsedLispVal -> Maybe a) -> Text -> P.ParsedLispVal -> Maybe Text -> Either ResolverError a
as' f n v e' =
  case f v of
    Just v' -> Right v'
    Nothing ->
      let e = maybe "" ("\n" <>) e' in
      Left $ ReResolverError (Just $ P.getPos v) $ "Expected " <> n <> ", but got " <> P.nameOf v <> e



removePos :: ResolvedLispVal -> ResolvedLispVal
removePos (RlAtom _ a) = RlAtom noPos a
removePos (RlBool _ b) = RlBool noPos b
removePos (RlDefine _ name val) = RlDefine noPos name (removePos val)
removePos (RlDo _ xs) = RlDo noPos (removePos <$> xs)
removePos (RlFuncCall _ f xs) = RlFuncCall noPos (removePos f) (removePos <$> xs)
removePos (RlIf _ cond then' else') = RlIf noPos (removePos cond) (removePos then') (removePos else')
removePos (RlInt _ i) = RlInt noPos i
removePos (RlLambda _ xs vs) = RlLambda noPos (xs <&> (\(_, b) -> (noPos, b))) (removePos <$> vs)
removePos (RlLet _ s vs xs) = RlLet noPos s (vs <&> (\((_, b), (_, v)) -> ((noPos, b), (noPos, removePos v)))) (removePos <$> xs)
removePos (RlList _ xs) = RlList noPos (removePos <$> xs)
removePos (RlNil _) = RlNil noPos
removePos (RlString _ s) = RlString noPos s


getPos :: ResolvedLispVal -> Pos
getPos (RlAtom p _) = p
getPos (RlBool p _) = p
getPos (RlDefine p _ _) = p
getPos (RlDo p _) = p
getPos (RlFuncCall p _ _) = p
getPos (RlIf p _ _ _) = p
getPos (RlInt p _) = p
getPos (RlLambda p _ _) = p
getPos (RlLet p _ _ _) = p
getPos (RlList p _) = p
getPos (RlNil p) = p
getPos (RlString p _) = p



instance L.LispError ResolverError where
  showLispError :: ResolverError -> (Maybe Pos, Text, Text, Text)
  showLispError e =
    let errTyp = "Resolver Error" in
    case e of
      ReReserved pos msg -> (pos, errTyp, "Reserved", msg)
      ReResolverError pos msg -> (pos, errTyp, "Resolver", msg)
      ReTypeDeclaration pos msg -> (pos, errTyp, "Type Declaration", msg)
      ReTypeName pos msg -> (pos, errTyp, "Type Name", msg)
      ReUnsupported pos msg -> (pos, errTyp, "Unsupported", msg)
