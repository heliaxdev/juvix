{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Nets.Bohm where

import           Control.Lens
import           Prelude (Show(..))

import           Juvix.Library hiding (link, reduce)
import           Juvix.Interaction
import           Juvix.NodeInterface

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
 | FanIn' Int
 | Curried' (Int → Int)
 | CurriedB' (Int → Bool)
 | Erase'
 deriving Show


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
 | Mu      {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
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
 | FanIn   {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary, _lab :: Int}
   -- Lang could be a tighter bound, but that would require more boiler plate
 | Curried  {_prim :: Primary, _aux1 :: Auxiliary, _curried :: (Int → Int)}
 | CurriedB {_prim :: Primary, _aux1 :: Auxiliary, _curriedB :: (Int → Bool)}
 | Erase   {_prim :: Primary}
 deriving Show

instance Show ((->) a b) where
  show _ = "fun"

makeFieldsNoPrefix ''ProperPort

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort :: Net Lang → Node → Maybe ProperPort
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
    f Mu'      = aux2FromGraph Mu      net node
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

    f Erase'   = aux0FromGraph Erase   net node
    f (FanIn' i)    = aux2FromGraph (\p a1 a2 → FanIn p a1 a2 i)  net node
    f (IntLit' i)   = aux0FromGraph (\x -> IntLit x i) net node
    f (Symbol' s)   = aux0FromGraph (\x -> Symbol x s) net node
    f (Curried' c)  = aux1FromGraph (\x y -> Curried x y c) net node
    f (CurriedB' c) = aux1FromGraph (\x y -> CurriedB x y c) net node
-- Rewrite rules----------------------------------------------------------------

reduceAll :: HasState "info" Info m => Int -> Net Lang -> m (Net Lang)
reduceAll = flip (untilNothingNTimesM reduce)

reduce :: HasState "info" Info m ⇒ Net Lang → m (Maybe (Net Lang))
reduce net = do
  (newNetLang, isChanged) ← foldrM update (net,False) netNodes
  if isChanged then do
    modify @"info" (\c → c {parallelSteps = parallelSteps c + 1})
    return (Just newNetLang)
    else
    return Nothing
  where
    netNodes = nodes net
    update n (net, isChanged)
      | isBothPrimary net n = return (net, isChanged)
      | otherwise =
        case langToProperPort net n of
          Nothing → pure (net, isChanged)
          Just port →
            case port of
              And (Primary node) _ _ →
                case langToProperPort net node of
                  Just Fals {} → updated <$> propPrimary net (n, port) node
                  Just Tru  {} → updated <$> anihilateRewireAux net node (n, port)
                  _            → pure (net, isChanged)
              Or (Primary node) _ _ →
                case langToProperPort net node of
                  Just Fals {} → updated <$> anihilateRewireAux net node (n, port)
                  Just Tru  {} → updated <$> propPrimary net (n, port) node
                  _            → pure (net, isChanged)
              Not (Primary node) _ →
                case langToProperPort net node of
                  Nothing      → pure (net, isChanged)
                  Just x       → notExpand net (node, x) (n, port) isChanged
              IfElse (Primary node) _ _ _ →
                case langToProperPort net node of
                  Just Fals {} → updated <$> ifElseRule net node (n, port) False
                  Just Tru  {} → updated <$> ifElseRule net node (n, port) True
                  _            → pure (net, isChanged)
              Lambda (Primary node) _ _ →
                case langToProperPort net node of
                  Just app@(App {}) → updated <$> anihilateRewireAuxTogether net (n, port) (node, app)
                  Just FanIn {_lab} → updated <$> fanInAux2 net node (n, Lambda') _lab
                  _                 → pure (net, isChanged)
              App (Primary node) _ _ →
                case langToProperPort net node of
                  Just lam@(App {}) → updated <$> anihilateRewireAuxTogether net (n, port) (node, lam)
                  Just FanIn {_lab} → updated <$> fanInAux2 net node (n, App') _lab
                  _                 → pure (net, isChanged)
              Cdr (Primary node) _ →
                case langToProperPort net node of
                  Just Nil {} → updated <$> propPrimary net (n, port) node
                  _           → pure (net, isChanged)
              Nil (Primary node) →
                case langToProperPort net node of
                  Just c@Cdr {} → updated <$>  propPrimary net (node, c) n
                  _             → pure (net, isChanged)
              Cons (Primary node) _ _ →
                case langToProperPort net node of
                  Just c@Cdr {} → updated <$> (consCdr net (port, n) (c, node))
                  Just c@Car {} → updated <$> (consCar net (port, n) (c, node))
                  _             → pure (net, isChanged)
              FanIn (Primary node) _ _ level →
                case langToProperPort net node of
                  Just App    {} → updated <$> fanInAux2 net n (node, App')    level
                  Just Lambda {} → updated <$> fanInAux2 net n (node, Lambda') level
                  -- fan in should interact with all!, update later
                  _              → pure (net, isChanged)
              Erase (Primary node) →
                case langToProperPort net node of
                  Just x  → updated <$> eraseAll net (x, node) n
                  Nothing → pure (net, isChanged)
              Eq (Primary node) _ _   → curryOnIntB net ((==)  , n) node isChanged
              More (Primary node) _ _ → curryOnIntB net ((>), n) node isChanged
              Less (Primary node) _ _ → curryOnIntB net ((<), n) node isChanged
              Meq (Primary node) _ _  → curryOnIntB net ((>=) , n) node isChanged
              Leq (Primary node) _ _  → curryOnIntB net ((<=) , n) node isChanged
              Div (Primary node) _ _  → curryOnInt net (div , n) node isChanged
              Sub (Primary node) _ _  → curryOnInt net ((-) , n) node isChanged
              Prod (Primary node) _ _ → curryOnInt net ((*), n) node isChanged
              Mod (Primary node) _ _  → curryOnInt net (mod , n) node isChanged
              _ → pure (net, isChanged)

updated :: a -> (a, Bool)
updated c = (c, True)

curryOnInt :: HasState "info" Info f => Net Lang -> (Int -> Int -> Int, Node) -> Node -> Bool -> f (Net Lang, Bool)
curryOnInt net (x, n) node isChanged =
  case langToProperPort net node of
    Just i@IntLit {} → updated <$> curryRule net (x, n) (i, node)
    _                → pure (net, isChanged)

curryOnIntB :: HasState "info" Info f => Net Lang -> (Int -> Int -> Bool, Node) -> Node -> Bool -> f (Net Lang, Bool)
curryOnIntB net (x, n) node isChanged =
  case langToProperPort net node of
    Just i@IntLit {} → updated <$> curryRuleB net (x, n) (i, node)
    _                → pure (net, isChanged)

propPrimary :: (Aux2 s, HasState "info" Info m) => Net Lang -> (Node, s) -> Node -> m (Net Lang)
propPrimary net (numDel, nodeDel) numProp =
  let wire = relinkAux net (nodeDel^.aux1, Aux1) (numProp, Prim) in
  case auxToNode (nodeDel^.aux2) of
    Just _ -> do
      sequentalStep
      let (eraseNum, net') = newNode wire Erase'
          net''            = relink net' (numDel, Aux2) (eraseNum, Prim)
      return $ deleteRewire [numDel] [eraseNum] $ net''
    Nothing -> do
      incGraphSizeStep (-1)
      return $ delNodes [numDel] $ wire

ifElseRule ::
  (HasState "info" Info m, Aux3 s) ⇒ Net Lang → Node → (Node, s) → Bool → m (Net Lang)
ifElseRule net numPrimOnly (numAuxs, auxs) pred = do
  incGraphSizeStep (-1)
  let (numErase, net') = newNode net Erase'
  return $ deleteRewire [numPrimOnly, numAuxs] [numErase]
         $ if pred
           then rewire (relinkAux net' (auxs^.aux2, Aux2)
                                       (numErase, Prim))
                       (Aux1, auxs^.aux1)
                       (Aux3, auxs^.aux3)
           else rewire (relinkAux  net' (auxs^.aux3, Aux3)
                                        (numErase, Prim))
                       (Aux1, auxs^.aux1)
                       (Aux2, auxs^.aux2)


anihilateRewireAux ::
 (HasState "info" Info m, Aux2 s) ⇒ Net a → Node → (Node, s) → m (Net a)
anihilateRewireAux net numPrimOnly (numAuxs, auxs) = do
  incGraphSizeStep (-2)
  return $ delNodes [numPrimOnly, numAuxs]
         $ rewire net (Aux1, auxs^.aux1) (Aux2, auxs^.aux2)


-- used for app lambda!
anihilateRewireAuxTogether ::
 (HasState "info" Info m, Aux2 s) ⇒ Net a → (Node, s) → (Node, s) → m (Net a)
anihilateRewireAuxTogether net (numNode1, node1) (numNode2, node2) = do
  incGraphSizeStep (-2)
  return $ delNodes [numNode1, numNode2]
         $ rewire (rewire net (Aux1, node1^.aux1) (Aux1, node2^.aux1))
                  (Aux2, node1^.aux2)
                  (Aux2, node2^.aux2)

-- o is Aux1 * is Aux2
muExpand :: HasState "info" Info m ⇒ Net Lang → Node → m (Net Lang)
muExpand net muNum = do
  incGraphSizeStep 2
  return $ deleteRewire [muNum] [fanIn, fanOut, newMu]
         $ foldr (flip linkAll)
                 net'''
                 [nodeFanIn, nodeFanOut]
  where
    (fanIn, net')   = newNode net (FanIn' 0)
    (fanOut, net'') = newNode net' (FanIn' 0)
    (newMu, net''') = newNode net'' (FanIn' 0)
    nodeFanIn = RELAuxiliary2 { node       = fanIn
                              , primary    = ReLink muNum Aux1
                              , auxiliary1 = Link (Port Aux1 newMu)
                              , auxiliary2 = ReLink muNum Prim
                              }
    nodeFanOut = RELAuxiliary2 { node       = fanOut
                               , primary    = ReLink muNum Aux2
                               , auxiliary1 = Link (Port Aux2 newMu)
                               , auxiliary2 = Link (Port Prim newMu)
                               }


fanInAux2 :: HasState "info" Info m ⇒ Net Lang → Node → (Node, Lang) → Int → m (Net Lang)
fanInAux2 net numFan (numOther, otherLang) level = do
  incGraphSizeStep 2
  return $ deleteRewire [numFan, numOther] [other1, other2, fanIn1, fanIn2]
         $ foldr (flip linkAll)
                 net''''
                 [nodeOther1, nodeOther2, nodeFan1, nodeFan2]
  where
    (other1, net')    = newNode net    otherLang
    (other2, net'')   = newNode net'   otherLang
    (fanIn1, net''')  = newNode net''  (FanIn' level)
    (fanIn2, net'''') = newNode net''' (FanIn' level)
    nodeOther1 = RELAuxiliary2 { node       = other1
                               , primary    = ReLink numFan Aux1
                               , auxiliary1 = Link (Port Aux1 fanIn2)
                               , auxiliary2 = Link (Port Aux1 fanIn1)
                               }
    nodeOther2 = RELAuxiliary2 { node       = other2
                               , primary    = ReLink numFan Aux2
                               , auxiliary1 = Link (Port Aux2 fanIn2)
                               , auxiliary2 = Link (Port Aux2 fanIn1)
                               }
    nodeFan1   = RELAuxiliary2 { node       = fanIn1
                               , primary    = ReLink numOther Aux2
                               , auxiliary1 = Link (Port Aux2 other1)
                               , auxiliary2 = Link (Port Aux2 other2)
                               }
    nodeFan2   = RELAuxiliary2 { node       = fanIn2
                               , primary    = ReLink numOther Aux1
                               , auxiliary1 = Link (Port Aux1 other1)
                               , auxiliary2 = Link (Port Aux1 other2)
                               }
-- TODO :: delete node coming in!
notExpand :: (Aux2 s, HasState "info" Info f)
          => Net Lang
          -> (Node, ProperPort)
          -> (Node, s)
          -> Bool
          -> f (Net Lang, Bool)
notExpand net (n, Tru {}) (notNum, notPort) _ =
  updated <$> propPrimary (delNodes [n] net') (notNum, notPort) numFals
  where
    (numFals, net') = newNode net Fals'

notExpand net (n, Fals {}) (notNum, notPort) _ =
  updated <$> propPrimary (delNodes [n] net') (notNum, notPort) numFals
  where
    (numFals, net') = newNode net Tru'

notExpand net _ _ updated = pure (net, updated)


-- Erase should be connected to the main port atm, change this logic later
-- when this isn't the case
eraseAll :: (Aux3 s, HasState "info" Info m) => Net Lang -> (s, Node) -> Node -> m (Net Lang)
eraseAll net (node, numNode) nodeErase = do
  let ((net',mE)    , i)  = auxDispatch net   (node^.aux1) Aux1
      ((net'', mE2) , i2) = auxDispatch net'  (node^.aux2) Aux2
      ((net''', mE3), i3) = auxDispatch net'' (node^.aux3) Aux3
  incGraphSizeStep (i + i2 + i3 - 2)
  return (deleteRewire [numNode, nodeErase] (catMaybes [mE, mE2, mE3]) net''')
  where
    auxDispatch net FreeNode      _   = ((net, Nothing), 0)
    auxDispatch net (Auxiliary _) aux = (erase net aux, 1)
    erase net port = (relink net' (numNode, port) (numE, Prim), Just numE)
      where
        (numE, net') = newNode net Erase'

consCar :: (HasState "info" Info m, Aux2 s1, Aux1 s2) => Net Lang -> (s1, Node) -> (s2, Node) -> m (Net Lang)
consCar net (cons, numCons) (car, numCar) = do
  incGraphSizeStep (-1)
  return $ deleteRewire [numCons, numCar] [erase]
         $ rewire (relinkAux net' (cons^.aux1, Aux1) (erase, Prim))
                  (Aux2, cons^.aux2)
                  (Aux1, car^.aux1)
  where
    (erase, net') = newNode net Erase'


consCdr :: (HasState "info" Info m, Aux2 s) ⇒ Net Lang → (s, Node) → (s, Node) → m (Net Lang)
consCdr net (cons, numCons) (cdr, numCdr) = do
  incGraphSizeStep (-1)
  return $ deleteRewire [numCons, numCdr] [erase]
         $ rewire (relinkAux net' (cons^.aux2, Aux2) (erase, Prim))
                  (Aux1, cons^.aux1)
                  (Aux1, cdr^.aux1)
  where
    (erase, net') = newNode net Erase'

testNilNil :: (HasState "info" Info m, Aux1 s) ⇒ Net Lang → (s, Node) → Node → m (Net Lang)
testNilNil net (test, numTest) numNil = do
  incGraphSizeStep (-1)
  return $ deleteRewire [numTest, numNil] [true]
         $ relinkAux net' (test^.aux1, Aux1) (true, Prim)
  where
    (true, net') = newNode net Tru'

testNilCons :: (HasState "info" Info m, Aux2 s) ⇒ Net Lang → (s, Node) → (s, Node) → m (Net Lang)
testNilCons net (cons, numCons) (test, numTest) = do
  incGraphSizeStep 1
  return $ deleteRewire [numCons, numTest] [erase1, erase2, false]
         $ foldl' (\n (a1,a2) → relinkAux n a1 a2)
                  net'''
                  [((cons^.aux1, Aux1), (erase1, Prim))
                  ,((cons^.aux2, Aux2), (erase2, Prim))
                  ,((test^.aux1, Aux1), (false , Prim))]
  where
    (erase1, net')   = newNode net   Erase'
    (erase2, net'')  = newNode net'  Erase'
    (false,  net''') = newNode net'' Fals'


curryRuleGen
 :: HasState "info" Info m =>
    (t -> a)
    -> Net a
    -> (Int -> t, Node)
    -> (ProperPort, Node)
    -> m (Net a)
curryRuleGen con net (nodeF, numNode) ((IntLit _ i), numInt) = do
  incGraphSizeStep (-1)
  return $ deleteRewire [numNode, numInt] [curr]
         $ linkAll net' currNode
  where
    (curr, net') = newNode net (con (nodeF i))
    currNode     = RELAuxiliary1 { node       = curr
                                 , primary    = ReLink numNode Aux2
                                 , auxiliary1 = ReLink numNode Aux1
                                 }
curryRuleGen _ net _ _ = return net

curryRule :: HasState "info" Info m
         ⇒ Net Lang
         → (Int → Int → Int, Node)
         → (ProperPort, Node)
         → m (Net Lang)
curryRule = curryRuleGen Curried'


curryRuleB :: HasState "info" Info m
         ⇒ Net Lang
         → (Int → Int → Bool, Node)
         → (ProperPort, Node)
         → m (Net Lang)
curryRuleB = curryRuleGen CurriedB'
