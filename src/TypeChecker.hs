{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TypeChecker
    ( TypeEnv(..)
    , TypeError(..)
    , TypedLispVal(..)
    , emptyTcState
    , emptyTypeEnv
    , getValType
    , removePos
    , tlookup
    , typeCheck
    , typeCheckVal
    , typeEnvFromPrimFns
    , typeVarPrefix
    ) where

import Verset
import Control.Monad.Trans.Except (ExceptT, Except, runExcept, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put, modify')
import Data.Set qualified as Set
import Data.Text qualified as Txt
import Data.Map.Strict qualified as Map

import Lisp (Pos(..), noPos)
import Lisp qualified as L
import Resolver qualified as R
import Printer.PrintError qualified as Pr
import Printer.PrintType qualified as Pr


-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------------------------------------------------------------------------------
data TypeEnv = TypeEnv
  { teTypes :: !(Map.Map Text L.PolyType)
  , teParent :: !(Maybe TypeEnv)
  }


emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv
  { teTypes = mempty
  , teParent = Nothing
  }

typeEnvFromPrimFns :: L.PrimitiveFunctionTypes -> TypeEnv
typeEnvFromPrimFns (L.PrimitiveFunctionTypes primFns) =
  TypeEnv
    { teTypes = primFns
    , teParent = Nothing
    }
-------------------------------------------------------------------------------------------------------------------------------------------------------




-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Typed values
-------------------------------------------------------------------------------------------------------------------------------------------------------
data TypedLispVal
  = TvAtom !Pos !L.LispType !Text
  | TvBool !Pos !Bool
  | TvDefine !Pos !L.PolyType !Text !TypedLispVal
  | TvDo !Pos !L.LispType ![TypedLispVal]
  | TvFuncCall !Pos !L.LispType !TypedLispVal ![TypedLispVal]  -- ^ TvFuncCall pos returnType functionVal argVals
  | TvIf !Pos !L.LispType !TypedLispVal !TypedLispVal !TypedLispVal
  | TvInt !Pos !Int
  | TvLambda !Pos !L.LispType ![(Pos, Text)] ![TypedLispVal]
  | TvLet !Pos !L.LispType !L.LetStyle ![(Pos, Text, TypedLispVal)] ![TypedLispVal]
  | TvList !Pos !L.LispType ![TypedLispVal]
  | TvNil !Pos
  | TvString !Pos !Text
  deriving (Show, Eq)



data TcState = TcState
  { tsTypeVarCounter :: !Int
  , tsSubst :: !(Map Text L.LispType)
  }


emptyTcState :: TcState
emptyTcState =
  TcState
    { tsTypeVarCounter = 0
    , tsSubst = mempty
    }


removePos :: TypedLispVal -> TypedLispVal
removePos (TvAtom _ t a) = TvAtom noPos t a
removePos (TvBool _ b) = TvBool noPos b
removePos (TvDefine _ t n v) = TvDefine noPos t n (removePos v)
removePos (TvDo _ t xs) = TvDo noPos t (removePos <$> xs)
removePos (TvFuncCall _ ftyp fVal argVals) = TvFuncCall noPos ftyp (removePos fVal) (removePos <$> argVals)
removePos (TvIf _ typ cond then' else') = TvIf noPos typ (removePos cond) (removePos then') (removePos else')
removePos (TvInt _ i) = TvInt noPos i
removePos (TvLambda _ t bindings body) = TvLambda noPos t (bindings <&> \(_, n) -> (noPos, n)) (removePos <$> body)
removePos (TvLet _ t s xs ys) = TvLet noPos t s (xs <&> \(_, n, y) -> (noPos, n, removePos y)) (removePos <$> ys)
removePos (TvList _ t xs) = TvList noPos t (fmap removePos xs)
removePos (TvNil {}) = TvNil noPos
removePos (TvString _ s) = TvString noPos s


getValType :: TypedLispVal -> L.LispType
getValType (TvAtom _ t _) = t
getValType (TvBool {}) = L.TyBool
getValType (TvDefine {}) = L.TyNil -- define is not a value, so return nil. Do not return the type of the value
getValType (TvDo _ t _) = t
getValType (TvFuncCall _ t _ _) = t
getValType (TvIf _ t _ _ _) = t
getValType (TvInt {}) = L.TyInt
getValType (TvLambda _ t _ _) = t
getValType (TvLet _ t _ _ _) = t
getValType (TvList _ t _) = t
getValType (TvNil {}) = L.TyNil
getValType (TvString {}) = L.TyString


instance L.HasPos TypedLispVal where
  getPos (TvAtom p _ _) = p
  getPos (TvBool p _) = p
  getPos (TvDefine p _ _ _) = p
  getPos (TvDo p _ _) = p
  getPos (TvFuncCall p _ _ _) = p
  getPos (TvIf p _  _ _ _) = p
  getPos (TvInt p _) = p
  getPos (TvLambda p _ _ _) = p
  getPos (TvLet p _ _ _ _) = p
  getPos (TvList p _ _) = p
  getPos (TvNil p) = p
  getPos (TvString p _) = p
-------------------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Typed Check
-------------------------------------------------------------------------------------------------------------------------------------------------------
typeCheck ::  TypeEnv -> [R.ResolvedLispVal] -> Either TypeError (TypeEnv, [TypedLispVal])
typeCheck env0 vs = do
  runExcept $ flip evalStateT emptyTcState $ do
    foldTypeCheckVals env0 vs


typeCheckVal :: TypeEnv -> R.ResolvedLispVal -> Either TypeError (TypeEnv, TypedLispVal)
typeCheckVal env0 rv = do
  runExcept $ flip evalStateT emptyTcState $ do
    typeCheckVal' env0 rv


-- | Type check a list of values
-- Folds a TypeEnv though a list of ResolvedLispVal calling `typeCheckVal'` on each value.
-- See `typeCheckVal'` for the actual type checking logic.
foldTypeCheckVals :: TypeEnv -> [R.ResolvedLispVal] -> StateT TcState (Except TypeError) (TypeEnv, [TypedLispVal])
foldTypeCheckVals env1 vs = do
  -- Fold and collect results
  (env2, res2) <- L.foldM' (env1, []) vs $ \(env2, res2) tv2 -> do
    (env3, tv3) <- typeCheckVal' env2 tv2
    pure (env3, tv3 : res2)

  -- Reverse the results to maintain original order
  pure (env2, reverse res2)


-- | Type check a single value.
-- Uses a `StateT ... (Except...) ...` monad transformer stack
--  * StateT to keep and update the TcState (e.g. type variable counter)
--  * Except allows "throwing" errors (throwE/Left)
--
--  The order is `StateT (Except)` rather than `Except (StateT ...)`
--   because if a type error occurs, we want to stop processing and return the error immediately.
typeCheckVal'
  :: TypeEnv
  -> R.ResolvedLispVal
  -> StateT TcState (Except TypeError) (TypeEnv, TypedLispVal)
typeCheckVal' env1 rv = do
  -- Type check
  (envFinal, topT1) <- go

  -- Apply substitutions to the top level value.
  -- We need to do this here because there may be polymorphic types that need to be instantiated
  -- E.g. `(concat (list () () (list 1 2)))`  where concat is `[[a]] -> [a]`
  --    The type check may infer a result type of `[a]`
  --    but there is a concrete type of `[Int]` implied by the last param
  --    Calling `applySubstitutions` here ensures that we get the concrete type of `[Int]`
  topFinal <- applyValSubstitutions topT1
  pure (envFinal, topFinal)

  where
    go =
      case rv of
        -- Simple cases, no extra type checking needed
        R.RlNil p -> pure (env1, TvNil p)
        R.RlInt p v -> pure (env1, TvInt p v)
        R.RlString p v -> pure (env1, TvString p v)
        R.RlBool p v -> pure (env1, TvBool p v)

        -- Type check the more complex cases
        R.RlAtom p v -> typeCheckAtom p v
        R.RlDefine p name val -> typeCheckDefine p name val
        R.RlDo p vs -> typeCheckDo p vs
        R.RlFuncCall p f as -> typeCheckFuncCall p f as
        R.RlIf p cond then' else' -> typeCheckIf p cond then' else'
        R.RlLambda p bindings body -> typeCheckLambda p bindings body
        R.RlLet p style bindings body -> typeCheckLet p style bindings body
        R.RlList p vs -> typeCheckList p vs


    -- (define name val)
    typeCheckDefine pos name val1 = do
      -- Type check the value being defined.
      (_env2, val2) <- typeCheckVal' env1 val1

      -- Apply substitutions before generalising. This gets the final (possibly concrete) type of the value.
      -- This step resolves all type variables to their current bindings.
      tFinal <- applySubstitutions (getValType val2)

      -- Generalise the type of the value to create a polymorphic type if possible.
      -- See `generalise` for details.
      let pt = generalise env1 tFinal

      -- Update the *current* environment with the new binding.
      -- Note: this does not create a new env layer, it is an in-place update.
      let env3 = env1 { teTypes = Map.insert name pt env1.teTypes }

      pure (env3, TvDefine pos pt name val2)


    -- (if cond then else)
    typeCheckIf pos cond1 then1 else1 = do
      -- Type check the `condition`, `then`, and `else` expressions.
      (env2, cond2) <- typeCheckVal' env1 cond1
      (env3, then2) <- typeCheckVal' env2 then1
      (env4, else2) <- typeCheckVal' env3 else1

      -- Check that the condition is a boolean.
      unify (L.getPos cond2, getValType cond2) (pos, L.TyBool)

      -- Unify the types of the then and else branches.
      -- I.e. the then and else branches must have the same result type.
      unify (L.getPos then2, getValType then2) (L.getPos else2, getValType else2)

      -- Get the final type of the body of the if expression.
      let finalType = getValType then2

      pure (env4, TvIf pos finalType cond2 then2 else2)



    -- (λ (param1 param2 ...) body..)
    typeCheckLambda pos params1 body1 = do
      -- Create new (fresh) monomorphic type variables for each of the parameters.
      params2 <- for params1 $ \(p, name) -> do
        u <- nextTypeVar
        pure (p, name, L.PtMono $ L.TyVar u)

      -- Create a new environment layer for this function.
      -- containing the parameter bindings as local variables.
      let env2 = TypeEnv
            { teParent = Just env1
            , teTypes = Map.fromList $ params2 <&> \(_, name, pt) -> (name, pt)
            }

      -- Type check the lambda body in the new environment.
      (_tenv2, body2) <- foldTypeCheckVals env2 body1

      -- Apply substitutions to each parameter type to resolve any type variables.
      -- Use `getMonoType` to extract the monomorphic type from a PolyType.
      --  This is required here because we only ever bind parameters to monotypes,
      --  so it is safe to extract without ambiguity.
      params3 <- for params2 $ \(_p, _name, pt) ->
        applySubstitutions (getMonoType pt)

      -- Get the return type of the lambda by looking at the last expression in the body.
      -- Apply substitutions to get the fully resolved type.
      retType <- case reverse body2 of
        []   -> pure L.TyNil
        (l:_) -> applySubstitutions (getValType l)

      -- Construct the final function type for the lambda.
      let lambdaType = L.TyFunc params3 retType
      pure (env1, TvLambda pos lambdaType params1 body2)


    -- (do ...)
    typeCheckDo pos vs = do
      -- Type check all expressions in the do block.
      (env2, vs2) <- foldTypeCheckVals env1 vs
      -- The type of the block is the type of the last expression or nil if empty.
      let lastType = case reverse vs2 of
            [] -> L.TyNil
            (x:_) -> getValType x
      pure (env2, TvDo pos lastType vs2)


    -- (let ( (n1 v1) (n2 v2)...) body...)
    typeCheckLet pos style bindings1 body1 = do
      (bindings2', env2') <-
        case style of
          -- Parallel let bindings:
          --  * All bindings are evaluated in the same outer environment.
          --  * No binding can refer to any other binding in the same let.
          --  * Only the body sees all new bindings.
          --
          -- Sequential let bindings, where a binding may refer to a previous binding,
          -- are not supported, but should be easy to add.
          L.LetParallel -> do
            -- Type check each binding in the let form.
            ls <- traverse typeCheckLetBinding bindings1
            let bindings2 = ls <&> \(_, p, name, val) -> (p, name, val)
                -- Generalise the types of the bindings to allow polymorphism.
                bindings3 = bindings2 <&> \(_, name, val) -> (name, generalise env1 $ getValType val)
                -- Create a new environment layer for the let body, containing the new bindings as local variables.
                env2 = TypeEnv
                   { teParent = Just env1
                   , teTypes = Map.fromList bindings3
                   }

            pure (bindings2, env2)

      -- Type check the let body in the new environment to get its final type.
      (_, body2') <- foldTypeCheckVals env2' body1
      let lastType = case reverse body2' of
            [] -> L.TyNil
            (x:_) -> getValType x

      -- Return the original environment, because let bindings are not visible outside the let.
      pure (env1, TvLet pos lastType style bindings2' body2')


    -- (name value)
    -- Type check a single let binding.
    -- Returns the updated environment, position, name, and value.
    typeCheckLetBinding :: ((Pos, Text), (Pos, R.ResolvedLispVal)) -> StateT TcState (Except TypeError) (TypeEnv, Pos, Text, TypedLispVal)
    typeCheckLetBinding ((npos, name), (_vpos, val1)) = do
      (env2, val2) <- typeCheckVal' env1 val1
      pure (env2, npos, name, val2)



    -- (func args...)
    -- Type check a function call (application).
    -- Note that this is for when a function is called, not when it is defined.
    typeCheckFuncCall pos funcVal' args' = do
      -- Type check the function being called and all argument expressions.
      (_, funcVal) <- typeCheckVal' env1 funcVal'
      (_, argVals) <- foldTypeCheckVals env1 args'
      -- Collect the types of all the arguments.
      let argTypes = getValType <$> argVals

      -- Create a fresh type variable for the return type.
      retType <- L.TyVar <$> nextTypeVar

      -- Unify the type of the function value with a function type: (argTypes -> retType).
      -- This means the value being called must be a function taking the argument types and returning the return type.
      -- I.e. the thing we are calling is actually a function with the expected arguments and return type.
      unify (L.getPos funcVal, getValType funcVal) (pos, L.TyFunc argTypes retType)

      -- After unification, apply substitutions to get the final (possibly concrete) return type.
      retTypeSubst <- applySubstitutions retType

      pure (env1, TvFuncCall pos retTypeSubst funcVal argVals)



    -- atom
    typeCheckAtom pos v = do
      -- Try to look up the variable in the type environment.
      case tlookup v env1 of
        -- If found, instantiate the value (see `instantiate`).
        Just pt -> do
          lt <- instantiate pos pt
          pure (env1, TvAtom pos lt v)

        -- If not found, throw an unbound type variable error.
        Nothing -> lift . throwE $ TcUnboundVariable pos v


    -- (list v...)
    -- Type check a list of values.
    -- In this implementation, lists are always homogeneous (all elements must have the same type).
    typeCheckList pos vs =
      case vs of
        -- An empty list can have any element type. Create a fresh type variable for the element type.
        -- Note that nil and the empty list `()` are not synonymous in this system (the mostly are in lisps).
        [] -> do
          u <- nextTypeVar
          pure (env1, TvList pos (L.TyList $ L.TyVar u) [])

        -- Lists at this point are just vectors of values, never function calls.
        -- The only requirement is that all elements have the same type.
        (h:t) -> do
          -- Type check the head element to get its type.
          (env2, headVal) <- typeCheckVal' env1 h
          -- Type check the tail elements to get their types.
          (env3, tailVals) <- foldTypeCheckVals env2 t

          let
              -- Type and position of the first element.
              headType = (L.getPos headVal, getValType headVal)
              -- Type and position the tail elements.
              tailTypes = tailVals <&> \v -> (L.getPos v, getValType v)

          -- Unify the type of each tail element with the head element's type,
          -- to ensure all elements in the list have the same type.
          forM_ tailTypes $ \tt -> unify headType tt
          -- If no type mismatch is found, the list is homogeneous.
          pure (env3, TvList pos (L.TyList $ snd headType) (headVal : tailVals))



-- | Instantiate a polymorphic type
-- Replaces each quantified variable in a polymorphic type  (e.g., one with forall variables, `∀ a b. a -> b -> a`)
--  and replaces each quantified variable with a fresh type variable.
--
-- For monomorphic types (PtMono), it still recurses through the type to ensure uniform treatment and
-- allow substitution of nested types.
--
--
-- * Quantified variables: Are the variables that are bound by a forall in the type, in the example that is `a` and `b`.
-- * Fresh type variables: A new type variable that is guaranteed to be unique and not already in use (bound)
--
-- Example
--    `∀ a b. a -> b -> a` becomes `U0 -> U1 -> U0`
--
-- This ensures that each use of a polymorphic value remains independent in type inference
--  Just because the user used the same name for a type variable in different places, does NOT mean they represent the same type.
instantiate :: Pos -> L.PolyType -> StateT TcState (Except TypeError) L.LispType
instantiate _pos pt1 = do
  case pt1 of
    L.PtMono lt1 ->
      instantiate' Map.empty lt1

    L.PtForall vars1 lt1 -> do
      isubsts1 <- for vars1 $ \v -> do
        c <- nextTypeVar
        pure (v, c)

      let isubsts2 = Map.fromList isubsts1
      instantiate' isubsts2 lt1


  where
    instantiate' :: Map Text Text -> L.LispType -> StateT TcState (Except TypeError) L.LispType
    instantiate' isubst lt1 = do
      case lt1 of
        L.TyNil -> pure lt1
        L.TyInt -> pure lt1
        L.TyString -> pure lt1
        L.TyBool -> pure lt1

        L.TyList lt2 -> do
          lt3 <- instantiate' isubst lt2
          pure $ L.TyList lt3

        L.TyVar v -> do
          case Map.lookup v isubst of
            Just v2 -> pure $ L.TyVar v2
            Nothing ->
              -- No substitution found, so return the original type variable
              pure $ L.TyVar v

        L.TyFunc targs tret -> do
          targs2 <- traverse (instantiate' isubst) targs

          tret2 <- instantiate' isubst tret
          pure $ L.TyFunc targs2 tret2



-- | Attempt to unify two types
-- This is the core of the type inference algorithm.
--
-- This means making two types be equal by finding a common substitution
--  E.g. for `(concat () (list 1 2))`
--       The concat params are of type `[U1]` and `[Int]`
--       We can then unify U1 and [Int] to see if they can be made equal.
--
-- Main steps
--   1. Apply all substitutions to both types. (see `applySubstitutions`)
--   2. If they are equal, do nothing
--   3. If one is a type variable, bind it to the other type (unless this creates an infinite type)
--   4. If both are lists, unify their elements
--   5. If both are functions, unify their argument types and return types
--   6. Otherwise, throw a type mismatch error
--
unify
  :: (Pos, L.LispType)
  -> (Pos, L.LispType)
  -> StateT TcState (ExceptT TypeError Identity) ()
unify (lhsPos, lhs1) (rhsPos, rhs1) = do
  -- 1) Substitute
  lhs2 <- applySubstitutions lhs1
  rhs2 <- applySubstitutions rhs1
  unify' lhs2 rhs2

  where
    unify' :: L.LispType -> L.LispType -> StateT TcState (Except TypeError) ()

    -- 2) Equal?
    unify' l r | l == r = pure ()

    -- 3) Type variable?
    unify' (L.TyVar name) r = bindVar lhsPos name r
    unify' l (L.TyVar name) = bindVar rhsPos name l

    -- 4) Lists?
    unify' (L.TyList a1) (L.TyList b1) = unify' a1 b1

    -- 5) Functions?
    unify' (L.TyFunc fnArgsType1 fnRetType1) (L.TyFunc fnArgsType2 fnRetType2) = do
      unless (length fnArgsType1 == length fnArgsType2) $ do
        lift . throwE $ TcArityError (Just $ lhsPos) (length fnArgsType1) (length fnArgsType2)

      -- unify the argument types
      zipWithM_ (\a1 a2 -> unify (lhsPos, a1) (rhsPos, a2)) fnArgsType1 fnArgsType2
      -- unify the return types
      unify (lhsPos, fnRetType1) (rhsPos, fnRetType2)

    -- 6) Mismatch
    unify' l r = lift . throwE $ TcTypeMismatch "Unification mismatch" lhsPos (L.PtMono l) (Just rhsPos) (L.PtMono r)




-- | Bind a variable to a type
-- This is done during type unification as part of the type inference process.
bindVar :: Pos -> Text -> L.LispType -> StateT TcState (Except TypeError) ()
bindVar pos name lt
  -- If trying to bind a type variable to itself, do nothing.
  --  E.g. `U1 = U1`
  | getTVarName lt == Just name = pure ()
  -- Prevent infinite types. See `occurs`
  | occurs name lt = lift . throwE $ TcInfiniteType pos name lt
  -- Record the substitution
  -- i.e. record that `name` is now bound to `lt`
  | otherwise = modify' $ \st -> st { tsSubst = Map.insert name lt st.tsSubst }

  where
    getTVarName :: L.LispType -> Maybe Text
    getTVarName (L.TyVar v) = Just v
    getTVarName _ = Nothing



-- | Check if a type variable occurs in a type
-- This is used to prevent infinite types
-- For example, if you this haskell type is infinite: a = [a]
occurs :: Text -> L.LispType -> Bool
occurs name lt =
  case lt of
    L.TyNil -> False
    L.TyInt -> False
    L.TyString -> False
    L.TyBool -> False
    L.TyVar v -> v == name
    L.TyList vs -> occurs name vs
    L.TyFunc fnArgsType1 fnRetType1 ->
      any (occurs name) (fnArgsType1 <> [fnRetType1])


-- | Apply substitutions.
-- Looks up type variables in the substitution map and replaces them with their corresponding types.
--
-- E.g. in `(concat () () (list 1 2))`
--   You start with types something like:
--      `(concat [U1] [U2] [Int])`
--   and accumulate substitutions such as:
--     U2 -> U1, U1 -> [Int]
--   After recursively applying substitutions, you get:
--      `(concat [Int] [Int] [Int])`
--
-- I.e. Get the principal types for the expression, that is the most specific, fully resolved types after inference.
applySubstitutions :: L.LispType -> StateT TcState (Except TypeError) L.LispType
applySubstitutions lt = do
  case lt of
    L.TyNil -> pure lt
    L.TyInt -> pure lt
    L.TyString -> pure lt
    L.TyBool -> pure lt
    L.TyVar v -> do
      st <- get
      case Map.lookup v st.tsSubst of
        Just lt2 -> applySubstitutions lt2
        Nothing -> pure lt

    L.TyList lt1 -> do
      lt2 <- applySubstitutions lt1
      pure $ L.TyList lt2

    L.TyFunc targs tret -> do
      targs2 <- traverse applySubstitutions targs

      tret2 <- applySubstitutions tret
      pure $ L.TyFunc targs2 tret2


-- | Apply substitutions to a polytype
-- See 'applySubstitutions'
applyPolyTypeSubstitutions :: L.PolyType -> StateT TcState (Except TypeError) L.PolyType
applyPolyTypeSubstitutions pt =
  case pt of
    L.PtMono t -> do
      t' <- applySubstitutions t
      pure (L.PtMono t')
    L.PtForall vars t -> do
      t' <- applySubstitutions t
      pure (L.PtForall vars t')


-- | Apply substitutions to a typed value
-- See 'applySubstitutions'
applyValSubstitutions :: TypedLispVal -> StateT TcState (Except TypeError) TypedLispVal
applyValSubstitutions v = do
  case v of
    TvNil p -> do
      pure $ TvNil p

    TvInt p i -> do
      pure $ TvInt p i

    TvString p s -> do
      pure $ TvString p s

    TvBool p b -> do
      pure $ TvBool p b

    TvAtom p lt1 a -> do
      lt2 <- applySubstitutions lt1
      pure $ TvAtom p lt2 a

    TvList p lt1 xs -> do
      lt2 <- applySubstitutions lt1
      xs2 <- for xs applyValSubstitutions
      pure $ TvList p lt2 xs2

    TvFuncCall p ftyp1 fVal argVals -> do
      fVal2 <- applyValSubstitutions fVal
      argVals2 <- for argVals applyValSubstitutions
      ftyp2 <- applySubstitutions ftyp1
      pure $ TvFuncCall p ftyp2 fVal2 argVals2

    TvLet p lt s xs ys -> do
      lt2 <- applySubstitutions lt
      xs2 <- for xs $ \(p', n, v') -> do
        v2 <- applyValSubstitutions v'
        pure (p', n, v2)

      ys2 <- traverse applyValSubstitutions ys
      pure $ TvLet p lt2 s xs2 ys2

    TvDo p lt xs -> do
      lt2 <- applySubstitutions lt
      xs2 <- traverse applyValSubstitutions xs
      pure $ TvDo p lt2 xs2

    TvLambda p lt1 bindings1 body1 -> do
      lt2 <- applySubstitutions lt1
      body2 <- traverse applyValSubstitutions body1
      pure $ TvLambda p lt2 bindings1 body2

    TvIf p lt1 cond1 then1 else1 -> do
      lt2 <- applySubstitutions lt1
      cond2 <- applyValSubstitutions cond1
      then2 <- applyValSubstitutions then1
      else2 <- applyValSubstitutions else1
      pure $ TvIf p lt2 cond2 then2 else2

    TvDefine p lt1 n v1 -> do
      lt2 <- applyPolyTypeSubstitutions lt1
      v2 <- applyValSubstitutions v1
      pure $ TvDefine p lt2 n v2



-- | Generalise a type.
-- Used to convert a monomorphic type into a polymorphic one by quantifying over type variables
-- that are not fixed by the environment.
--
-- Example:
--      (let (id (λ (a) a))
--        (id 1)
--        (id "oops"))
--   * An identity function is defined
--   * If we unify with the first call `(id 1)` we get a type of `Int -> Int` for `id`
--   * Then `(id "oops")` would cause a type error because `id` woud not accept a `String`
--
--   Generalising ensures that types are polymorphic where possible.
--   E.g. the type of `id` is generalised to `∀ a. a -> a` rather than just `a -> a`
--
-- In Hindley-Milner type inference, generalisation is what allows let-bound values to be used for multiple types.
generalise :: TypeEnv -> L.LispType -> L.PolyType
generalise env t =
  let
      -- 1. Collect all free type variables appearing in the environment.
      envVars = freeTypeVarsEnv env
      -- 2. Collect all free type variables in the type being generalised.
      typeVars = freeTypeVars t
      -- 3. Find variables that appear in the type, but not in the environment.
      -- These are the variables that can be generalised (quantified over).
      toGen = Set.toList (typeVars `Set.difference` envVars)
  in
  -- 4. If there are no variables to generalise, return a monomorphic type.
  if null toGen
  then L.PtMono t
  else L.PtForall toGen t


-- | Recursively look up a type in the environment
tlookup :: Text -> TypeEnv -> Maybe L.PolyType
tlookup name env =
  case Map.lookup name env.teTypes of
    Just v -> Just v
    Nothing -> case env.teParent of
      Nothing -> Nothing
      Just parent -> tlookup name parent


-- | Get the monomorphic type from a polymorphic type
-- This should not be used often
-- It is used internally here but should not be used in the public API
getMonoType :: L.PolyType -> L.LispType
getMonoType (L.PtMono t) = t
getMonoType (L.PtForall _ t) = t


-- | A free type variable in a type is a type variable that is not bound by a forall in that type,
-- nor already assigned a meaning in the current environment.
freeTypeVars :: L.LispType -> Set Text
freeTypeVars (L.TyVar v) = Set.singleton v
freeTypeVars (L.TyList t) = freeTypeVars t
freeTypeVars (L.TyFunc args ret) = Set.unions (freeTypeVars ret : (freeTypeVars <$> args))
freeTypeVars L.TyNil = Set.empty
freeTypeVars L.TyInt = Set.empty
freeTypeVars L.TyString = Set.empty
freeTypeVars L.TyBool = Set.empty


-- | For PtMono, the free variables are just those of the underlying monotype.
-- For PtForall vs t, the free variables are those in t excluding the ones quantified in vs.
freeTypeVarsInPoly :: L.PolyType -> Set Text
freeTypeVarsInPoly (L.PtMono t) = freeTypeVars t
freeTypeVarsInPoly (L.PtForall vs t) = freeTypeVars t `Set.difference` Set.fromList vs


-- | traverses the environment hierarchy and accumulates free type variables
freeTypeVarsEnv :: TypeEnv -> Set Text
freeTypeVarsEnv env =
  let parent = maybe Set.empty freeTypeVarsEnv env.teParent
      varsInThis = Map.foldr (\pt acc -> Set.union (freeTypeVarsInPoly pt) acc) Set.empty env.teTypes
  in
  Set.union varsInThis parent


-- | Prefix we want to use for fresh type variables
-- Entirely stylistic
typeVarPrefix :: Text
typeVarPrefix = "U"
--typeVarPrefix = "β"


-- | Get a unique fresh type variable
-- See `instantiate` for more details on how this is used.
nextTypeVar :: (Monad a) => StateT TcState a Text
nextTypeVar = do
  st <- get
  let c = st.tsTypeVarCounter
  put $ st { tsTypeVarCounter = c + 1 }
  pure $ typeVarPrefix <> show c
-------------------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------------------------------------------------------------------------------
data TypeError
  = TcResolverError !R.ResolverError
  | TcExpecingMonoType !(Maybe Pos) !Text
  | TcUnboundVariable !Pos !Text
  | TcUnboundTypeVariable !Pos !Text
  | TcTypeError !(Maybe Pos) !Text
  | TcUnexpectedType !(Maybe Pos) L.PolyType !Text
  | TcUnexpectedValue !(Maybe Pos) R.ResolvedLispVal !Text
  | TcTypeMismatch !Text !Pos L.PolyType !(Maybe Pos) L.PolyType
  | TcInfiniteType !Pos !Text !L.LispType
  | TcArityError !(Maybe Pos) !Int !Int
  | TcUnsupported !(Maybe Pos) !Text
  | TcTodo !(Maybe Pos) !Text



instance L.LispError TypeError where
  showLispError :: TypeError -> (Maybe Pos, Text, Text, Text)
  showLispError e =
    let errTyp = "Type Error" in
    case e of
      TcResolverError re -> L.showLispError re
      TcUnboundVariable pos name -> (Just pos, errTyp, "Unbound variable", name)
      TcUnboundTypeVariable pos name -> (Just pos, errTyp, "Unbound type variable", name)
      TcUnsupported pos msg -> (pos, errTyp, "Unsupported", msg)
      TcTodo pos msg -> (pos, errTyp, "TODO", msg)
      TcTypeError pos msg -> (pos, errTyp, "Type Error", msg)

      TcTypeMismatch header expectedPos expectedType gotPos gotType ->
        ( Just $ fromMaybe expectedPos gotPos
        , errTyp
        , header
        , Txt.unwords
          [ "expected:"
          , Pr.ppPolyTypePlainText expectedType <> ","
          , "but found:"
          , Pr.ppPolyTypePlainText gotType <> "."
          , "Type defined at"
          , Pr.showPosSimple gotPos
          ]
        )

      TcExpecingMonoType pos msg ->
        ( pos
        , errTyp
        , "Not expecting a quantified type"
        , msg
        )

      TcUnexpectedType expectedPos unexpectedType msg ->
        ( expectedPos
        , errTyp
        , msg
        , Txt.unwords
          [ "Unexpected type:"
          , Txt.strip $ Pr.ppPolyTypePlainText unexpectedType
          ]
        )

      TcUnexpectedValue expectedPos unexpectedVal msg ->
        ( expectedPos
        , errTyp
        , msg
        , Txt.unwords
          [ "Unexpected value:"
          --, Pr.prnLispVal unexpectedVal
          , show unexpectedVal --TODO resolver printer
          ]
        )

      TcInfiniteType pos name t ->
        ( Just pos
        , errTyp
        , "Infinite type"
        , Txt.unwords
          [ "Infinite type: " <> name
          , Pr.ppTypeAnsi t
          ]
        )

      TcArityError pos expectedCount gotCount ->
        ( pos
        , errTyp
        , "Arity Error"
        , Txt.unwords
          [ "Arity error"
          , "expected " <> show expectedCount <> " arguments"
          , "but got " <> show gotCount
          ]
        )
-------------------------------------------------------------------------------------------------------------------------------------------------------
