module LOpt where

import qualified Data.Map.Strict as M
import           Protolude

{- This is not designed to be efficient, it will be rewritten in Rust later. -}

type NodeId = Integer

type WireId = (Integer, Bool)

type Position = Integer

data Node
  = Constructor WireId WireId WireId
  | Duplicator WireId WireId WireId
  | Eraser WireId

  deriving (Show)

data Port
  = Free Text
  | Node NodeId Position

  deriving (Show)

data Wire
  = Loop
  | Connector Port Port

  deriving (Show)

data Net = Net {
  netNodes :: M.Map Integer Node,
  netWires :: M.Map Integer Wire
  }

  deriving (Show)

type Rule = Net → NodeId → Maybe Net

netFromLists ∷ [Node] → [Wire] → Net
netFromLists nodes wires = Net (M.fromList (zip [0..] nodes)) (M.fromList (zip [0..] wires))

fullReduce ∷ Net → Net
fullReduce net =
  case maybeReduce net of
    Just net' -> fullReduce net'
    Nothing   -> net

maybeReduce ∷ Net → Maybe Net
maybeReduce = flip foldMaybe (map (\r -> \net -> foldMaybe net (map (flip r) (M.keys (netNodes net)))) rules)

foldMaybe ∷ forall a b . a → [a → Maybe b] → Maybe b
foldMaybe x []      = Nothing
foldMaybe x (f:fs)  =
  case f x of
    Just y  -> Just y
    Nothing -> foldMaybe x fs

rules ∷ [Rule]
rules = [maybeCommute1, maybeCommute2, maybeCommute3, maybeAnnihilate1, maybeAnnihilate2, maybeAnnihilate3]

deleteKeys ∷ forall a b . Ord a ⇒ Map a b → [a] → Map a b
deleteKeys = foldl (flip M.delete)

maxKey ∷ forall a b . (Num a, Ord a) ⇒ Map a b → a
maxKey m = fromMaybe 0 $ head $ reverse $ M.keys m

updatePort ∷ Net → Port → WireId → Net
updatePort net@(Net nodes wires) port wire =
  case port of
    Free _ -> net
    Node id position ->
      let node = nodes M.! id in
      case (position, node) of
        (0, Eraser _) -> net { netNodes = M.insert id (Eraser wire) nodes }
        (0, Duplicator _ a b) -> net { netNodes = M.insert id (Duplicator wire a b) nodes }
        (1, Duplicator a _ b) -> net { netNodes = M.insert id (Duplicator a wire b) nodes }
        (2, Duplicator a b _) -> net { netNodes = M.insert id (Duplicator a b wire) nodes }
        (0, Constructor _ a b) -> net { netNodes = M.insert id (Constructor wire a b) nodes }
        (1, Constructor a _ b) -> net { netNodes = M.insert id (Constructor a wire b) nodes }
        (2, Constructor a b _) -> net { netNodes = M.insert id (Constructor a b wire) nodes }

-- Commutation

maybeCommute1 ∷ Rule
maybeCommute1 (Net nodes wires) id =
  let node = nodes M.! id in
  case node of
    Constructor (w, which) one two -> do
      let wire = wires M.! w
      cid <- case (wire, which) of
               (Connector _ (Node cid 0), True)  -> Just cid
               (Connector (Node cid 0) _, False) -> Just cid
               _                                 -> Nothing
      let conn = nodes M.! cid
      case conn of
        Duplicator _ three four -> do
          let Connector p1 p2 = wires M.! (fst one)
              Connector p3 p4 = wires M.! (fst two)
              Connector p5 p6 = wires M.! (fst three)
              Connector p7 p8 = wires M.! (fst four)
              newCAid = maxKey nodes + 1
              newCBid = newCAid + 1
              newDAid = newCBid + 1
              newDBid = newDAid + 1
              -- problem, need to update p1 to new node link ~>> general problem, need to update wires if nodes are recreated, should end up with loops
              newWOne = Connector (if snd one then Node newDAid 0 else p1) (if snd one then p2 else Node newDAid 0)
              newWTwo = Connector (if snd two then Node newDBid 0 else p3) (if snd two then p4 else Node newDBid 0)
              newWThree = Connector (if snd three then Node newCBid 0 else p5) (if snd three then p6 else Node newCBid 0)
              newWFour = Connector (if snd four then Node newCAid 0 else p7) (if snd four then p8 else Node newCAid 0)
              w1id = maxKey wires + 1
              w2id = w1id + 1
              w3id = w2id + 1
              w4id = w3id + 1
              w1 = Connector (Node newCAid 1) (Node newDAid 2)
              w2 = Connector (Node newCAid 2) (Node newDBid 2)
              w3 = Connector (Node newCBid 1) (Node newDAid 1)
              w4 = Connector (Node newCBid 2) (Node newDBid 1)
              ws = [(w1id, w1), (w2id, w2), (w3id, w3), (w4id, w4)]
              newCA = Constructor (fst four, snd four) (w1id, True) (w2id, True)
              newCB = Constructor (fst three, snd three) (w3id, True) (w4id, True)
              newDA = Duplicator (fst one, snd one) (w3id, False) (w1id, False)
              newDB = Duplicator (fst two, snd two) (w4id, False) (w2id, False)
              newNodes = deleteKeys (foldl (flip (uncurry M.insert)) nodes [(newCAid, newCA), (newCBid, newCB), (newDAid, newDA), (newDBid, newDB)]) [id, cid]
              newWires = deleteKeys (foldl (flip (uncurry M.insert)) wires ([(fst one, newWOne), (fst two, newWTwo), (fst three, newWThree), (fst four, newWFour)] ++ ws)) [w]
          return $ Net newNodes newWires
        _ -> Nothing
    _ -> Nothing

maybeCommute2 ∷ Rule
maybeCommute2 (Net nodes wires) id =
  let node = nodes M.! id in
  case node of
    Eraser (w, which) -> do
      let wire = wires M.! w
      cid <- case (wire, which) of
               (Connector _ (Node cid 0), True)  -> Just cid
               (Connector (Node cid 0) _, False) -> Just cid
               _                                 -> Nothing
      let conn = nodes M.! cid
      case conn of
        Constructor _ one two -> do
          let Connector p1 p2 = wires M.! (fst one)
              Connector p3 p4 = wires M.! (fst two)
              newA = Eraser (fst one, snd one)
              newB = Eraser (fst two, snd two)
              newAid = maxKey nodes + 1
              newBid = newAid + 1
              newWone = Connector (if snd one then Node newAid 0 else p1) (if snd one then p2 else Node newAid 0)
              newWtwo = Connector (if snd two then Node newBid 0 else p3) (if snd two then p4 else Node newBid 0)
              newNodes = M.insert newAid newA $ M.insert newBid newB $ deleteKeys nodes [id, cid]
              newWires = M.insert (fst one) newWone $ M.insert (fst two) newWtwo $ deleteKeys wires [w]
          return $ Net newNodes newWires
        _ -> Nothing
    _ -> Nothing

maybeCommute3 ∷ Rule
maybeCommute3 (Net nodes wires) id =
  let node = nodes M.! id in
  case node of
    Eraser (w, which) -> do
      let wire = wires M.! w
      cid <- case (wire, which) of
               (Connector _ (Node cid 0), True)  -> Just cid
               (Connector (Node cid 0) _, False) -> Just cid
               _                                 -> Nothing
      let conn = nodes M.! cid
      case conn of
        Duplicator _ one two -> do
          let Connector p1 p2 = wires M.! (fst one)
              Connector p3 p4 = wires M.! (fst two)
              newA = Eraser (fst one, snd one)
              newB = Eraser (fst two, snd two)
              newAid = maxKey nodes + 1
              newBid = newAid + 1
              newWone = Connector (if snd one then Node newAid 0 else p1) (if snd one then p2 else Node newAid 0)
              newWtwo = Connector (if snd two then Node newBid 0 else p3) (if snd two then p4 else Node newBid 0)
              newNodes = M.insert newAid newA $ M.insert newBid newB $ deleteKeys nodes [id, cid]
              newWires = M.insert (fst one) newWone $ M.insert (fst two) newWtwo $ deleteKeys wires [w]
          return $ Net newNodes newWires
        _ -> Nothing
    _ -> Nothing

-- Annihilation

maybeAnnihilate1 ∷ Rule
maybeAnnihilate1 net@(Net nodes wires) id =
  let node = nodes M.! id in
  case node of
    Constructor (w, True) one two ->
      let wire = wires M.! w in
      case wire of
        Connector _ (Node cid 0) ->
          let conn = nodes M.! cid in
          case conn of
            Constructor _ three four ->
              let Connector p1 p2 = wires M.! (fst one)
                  Connector p3 p4 = wires M.! (fst two)
                  Connector p5 p6 = wires M.! (fst three)
                  Connector p7 p8 = wires M.! (fst four)
                  newA = Connector (if snd one then p2 else p1) (if snd four then p8 else p7)
                  newB = Connector (if snd two then p4 else p3) (if snd three then p6 else p5)
                  newAid = maxKey wires + 1
                  newBid = newAid + 1
                  newNet = foldl (\n (x, y) -> updatePort n x y) net [
                              (if snd one then p2 else p1, (newAid, True)),
                              (if snd four then p8 else p7, (newAid, False)),
                              (if snd two then p4 else p3, (newBid, True)),
                              (if snd three then p6 else p5, (newBid, False))
                              ]
                  newWires = deleteKeys (netWires newNet) (w : map fst [one, two, three, four])
                  newNodes = deleteKeys (netNodes newNet) [id, cid]
              in Just (Net newNodes (M.insert newAid newA (M.insert newBid newB newWires)))
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

maybeAnnihilate2 ∷ Rule
maybeAnnihilate2 net@(Net nodes wires) id =
  let node = nodes M.! id in
  case node of
    Duplicator (w, True) one two ->
      let wire = wires M.! w in
      case wire of
        Connector _ (Node cid 0) ->
          let conn = nodes M.! cid in
          case conn of
            Duplicator _ three four ->
              let Connector p1 p2 = wires M.! (fst one)
                  Connector p3 p4 = wires M.! (fst two)
                  Connector p5 p6 = wires M.! (fst three)
                  Connector p7 p8 = wires M.! (fst four)
                  newA = Connector (if snd one then p2 else p1) (if snd three then p6 else p5)
                  newB = Connector (if snd two then p4 else p3) (if snd four then p8 else p7)
                  newAid = maxKey wires + 1
                  newBid = newAid + 1
                  newNet = foldl (\n (x, y) -> updatePort n x y) net [
                              (if snd one then p2 else p1, (newAid, True)),
                              (if snd three then p6 else p5, (newAid, False)),
                              (if snd two then p4 else p3, (newBid, True)),
                              (if snd four then p8 else p7, (newBid, False))
                              ]
                  newWires = deleteKeys (netWires newNet) (w : map fst [one, two, three, four])
                  newNodes = deleteKeys (netNodes newNet) [id, cid]
              in Just (Net newNodes (M.insert newAid newA (M.insert newBid newB newWires)))
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

maybeAnnihilate3 ∷ Rule
maybeAnnihilate3 net@(Net nodes wires) id =
  let node = nodes M.! id in
  case node of
    Eraser (w, True) ->
      let wire = wires M.! w in
      case wire of
        Loop -> Nothing
        Connector _ (Node cid _) ->
          let conn = nodes M.! cid in
          case conn of
            Eraser _ -> Just (net { netNodes = deleteKeys nodes [id, cid], netWires = M.delete w wires })
            _ -> Nothing
    _ -> Nothing

{- tests -}

trivialCommute1 ∷ Net
trivialCommute1 = netFromLists
    [Constructor (0, True) (1, True) (2, True), Duplicator (0, False) (3, True) (4, True)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Free "a"), Connector (Node 0 2) (Free "b"), Connector (Node 1 1) (Free "c"), Connector (Node 1 2) (Free "d")]

trivialCommute2 ∷ Net
trivialCommute2 = netFromLists
    [Constructor (0, True) (1, True) (2, True), Eraser (0, False)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Free "a"), Connector (Node 0 2) (Free "b")]

trivialCommute3 ∷ Net
trivialCommute3 = netFromLists
    [Duplicator (0, True) (1, True) (2, True), Eraser (0, False)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Free "a"), Connector (Node 0 2) (Free "b")]

trivialAnnihilate1 ∷ Net
trivialAnnihilate1 = netFromLists
    [Constructor (0, True) (1, True) (2, True), Constructor (0, False) (3, True) (4, True)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Free "a"), Connector (Node 0 2) (Free "b"), Connector (Node 1 1) (Free "c"), Connector (Node 1 2) (Free "d")]

testAnnihilate1a ∷ Net
testAnnihilate1a = netFromLists
    [Constructor (0, True) (1, True) (2, True), Constructor (0, False) (3, True) (4, True), Eraser (1, False), Eraser (2, False), Eraser (3, False), Eraser (4, False)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Node 2 0), Connector (Node 0 2) (Node 3 0), Connector (Node 1 1) (Node 4 0), Connector (Node 1 2) (Node 5 0)]

trivialAnnihilate2 ∷ Net
trivialAnnihilate2 = netFromLists
    [Duplicator (0, True) (1, True) (2, True), Duplicator (0, False) (3, True) (4, True)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Free "a"), Connector (Node 0 2) (Free "b"), Connector (Node 1 1) (Free "c"), Connector (Node 1 2) (Free "d")]

testAnnihilate2a ∷ Net
testAnnihilate2a = netFromLists
    [Duplicator (0, True) (1, True) (2, True), Duplicator (0, False) (3, True) (4, True), Eraser (1, False), Eraser (2, False), Eraser (3, False), Eraser (4, False)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Node 2 0), Connector (Node 0 2) (Node 3 0), Connector (Node 1 1) (Node 4 0), Connector (Node 1 2) (Node 5 0)]

trivialAnnihilate3 ∷ Net
trivialAnnihilate3 = netFromLists
    [Eraser (0, True), Eraser (0, False)]
    [Connector (Node 0 0) (Node 1 0)]

nonterminating ∷ Net
nonterminating = netFromLists
    [Constructor (0, True) (1, True) (2, True), Duplicator (0, False) (3, True) (1, False), Eraser (2, False), Eraser (3, False)]
    [Connector (Node 0 0) (Node 1 0), Connector (Node 0 1) (Node 1 2), Connector (Node 0 2) (Node 2 0), Connector (Node 1 1) (Node 3 0)]
