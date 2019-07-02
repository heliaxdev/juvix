{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Nets.Bohm where

import           Control.Lens
import           Protolude

import           Juvix.Interaction
import           Juvix.NodeInterface

data ProperPort
 = Or      {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | And     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Not     {_prim :: Primary, _aux1 :: Auxiliary}
 | Eq      {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Neq     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | More    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Less    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Meq     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Leq     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Cons    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Car     {_prim :: Primary, _aux1 :: Auxiliary}
 | Cdr     {_prim :: Primary, _aux1 :: Auxiliary}
 | Nil     {_prim :: Primary}
 | TestNil {_prim :: Primary, _aux1 :: Auxiliary}
 | IfElse  {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary, _aux3 :: Auxiliary}
 | Mu      {_prim :: Primary, _aux1 :: Auxiliary}
 | Div     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Sub     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Add     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Prod    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Mod     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Tru     {_prim :: Primary}
 | Fals    {_prim :: Primary}
 | IntLit  {_prim :: Primary, _int :: Int}
 | Lambda  {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Symbol  {_prim :: Primary, _symb :: SomeSymbol}
 | App     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 deriving Show

makeFieldsNoPrefix ''ProperPort

data Lang
 = Or'
 | And'
 | Not'
 | Eq'
 | Neq'
 | More'
 | Less'
 | Meq'
 | Leq'
 | Cons'
 | Car'
 | Cdr'
 | Nil'
 | TestNil'
 | IfElse'
 | Mu'
 | Div'
 | Sub'
 | Add'
 | Prod'
 | Mod'
 | Tru'
 | Fals'
 | IntLit' Int
 | Lambda'
 | Symbol' SomeSymbol
 | App'
 deriving Show

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort :: Net Lang -> Node -> Maybe ProperPort
langToProperPort net node = langToPort net node f
  where
    f Or'      = aux2FromGraph Or      net node
    f And'     = aux2FromGraph And     net node
    f Not'     = aux1FromGraph Not     net node
    f Meq'     = aux2FromGraph Meq     net node
    f Eq'      = aux2FromGraph Eq      net node
    f Neq'     = aux2FromGraph Neq     net node
    f More'    = aux2FromGraph More    net node
    f Less'    = aux2FromGraph Less    net node
    f Cons'    = aux2FromGraph Cons    net node
    f Car'     = aux1FromGraph Car     net node
    f Cdr'     = aux1FromGraph Cdr     net node
    f Nil'     = aux0FromGraph Nil     net node
    f TestNil' = aux1FromGraph TestNil net node
    f IfElse'  = aux3FromGraph IfElse  net node
    f Mu'      = aux1FromGraph Mu      net node
    f Div'     = aux2FromGraph Div     net node
    f Sub'     = aux2FromGraph Sub     net node
    f Add'     = aux2FromGraph Add     net node
    f Prod'    = aux2FromGraph Prod    net node
    f Mod'     = aux2FromGraph Mod     net node
    f Tru'     = aux0FromGraph Tru     net node
    f Fals'    = aux0FromGraph Fals    net node
    f Leq'     = aux2FromGraph Leq     net node
    f Lambda'  = aux2FromGraph Lambda  net node
    f App'     = aux2FromGraph App     net node
    f (IntLit' i) = aux0FromGraph (\x -> IntLit x i) net node
    f (Symbol' s) = aux0FromGraph (\x -> Symbol x s) net node
-- Rewrite rules----------------------------------------------------------------
reduce net = update undefined
  where
    netNodes = nodes net
    updated c = (c, True)
    update n (net, isChanged)
      | isBothPrimary net n = return (net, isChanged)
      | otherwise =
        case langToProperPort net n of
          Nothing → pure (net, isChanged)
          Just port →
            case port of
              and@(And (Primary node) _ _) →
                case langToProperPort net node of
                  Just Fals {} → updated <$> propPrimary net n node and
                  Just Tru  {} → updated <$> anihilateRewireAux net node n and
                  _            → pure (net, isChanged)

propPrimary ::
  (MonadState StateInfo m, Aux2 s) ⇒ Net a → Node → Node → s → m (Net a)
propPrimary net numDel numProp nodeDel = do
  incGraphSizeStep (-2)
  return $ delMaybeNodes [Just numDel, auxToNode (nodeDel^.aux2)]
         $ rewire net (Aux1, nodeDel^.aux1) (Prim, Auxiliary numProp)

anihilateRewireAux ::
  (MonadState StateInfo m, Aux2 s) ⇒ Net a → Node → Node → s → m (Net a)
anihilateRewireAux net numPrimOnly numAuxs auxs = do
  incGraphSizeStep (-2)
  return $ delNodes [numPrimOnly, numAuxs]
         $ rewire net (Aux1, auxs^.aux1) (Aux2, auxs^.aux2)
