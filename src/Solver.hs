{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | The solver's actual implementation
module Solver (
    -- * The actual interface
      CNF
    , runDPLL
    , runNaive
    -- * Data representation
    , VarMap (..)
    , emptyVarMap
    , vmPositive
    , vmNegative
    , vmPosCount
    , vmNegCount
    , vmByVal
    , vmByValC
    , Clause
    , Occurences
    , Assignments
    , SATState (..)
    , blankState
    , unitClauses
    , pureLiterals
    , occurences
    , freeVariables
    , clauses
    , assignments
    -- * Internals
    -- ** Main computation steps
    , SATMonad
    , initSAT
    , dpll
    , assign
    , unitProps
    , pureElims
    -- ** Smaller computation components
    , popFrom
    , deleteClause
    , removeRef
    , removeVar
    ) where
import           Control.Applicative            ( Alternative((<|>))
                                                , Applicative(liftA2)
                                                )
import           Control.Monad                  ( (>=>)
                                                , guard
                                                , when
                                                )
import qualified Control.Monad.State           as St
import           Data.Function                  ( (&) )
import qualified Data.IntMap                   as M
import qualified Data.IntSet                   as S
import           Data.List                      ( delete
                                                , sortOn
                                                , uncons
                                                )
import           Data.Maybe                     ( isJust )
import           Data.Monoid                    ( Sum(..) )
import           Data.Tuple                     ( swap )
import           Lens.Micro.Platform            ( (%=)
                                                , (+=)
                                                , (-=)
                                                , (.=)
                                                , (^.)
                                                , (^..)
                                                , makeLenses
                                                )
import qualified Lens.Micro.Platform           as L

-- | Used to represent variable occurences / clause contents.
--   That is, it maps vars to clauses or clauses to vars, hence the name.
data VarMap = VarMap
    { _vmPositive :: S.IntSet
    -- ^ positive occurences of a variable / positive literals in a clause
    , _vmNegative :: S.IntSet
    -- ^ negative occurences of a variable / negative literals in a clause
    , _vmPosCount :: Int
    -- ^ should be equal to the size of '_vmPositive'
    , _vmNegCount :: Int
    -- ^ should be equal to the size of '_vmNegative'
    }
    deriving (Eq, Show)

makeLenses ''VarMap

-- | An empty 'VarMap'
emptyVarMap :: VarMap
emptyVarMap = VarMap { _vmPositive = S.empty
                     , _vmNegative = S.empty
                     , _vmPosCount = 0
                     , _vmNegCount = 0
                     }

-- | Returns 'vmPositive' if the argument is 'True', 'vmNegative' if it's 'False'
vmByVal :: Bool -> L.Lens' VarMap S.IntSet
vmByVal True  = vmPositive
vmByVal False = vmNegative
-- | Returns 'vmPosCount' if the argument is 'True', 'vmNegCount' if it's 'False'
vmByValC :: Bool -> L.Lens' VarMap Int
vmByValC True  = vmPosCount
vmByValC False = vmNegCount

-- | Represents a clause
type Clause = VarMap
-- | Represents occurences of a variable
type Occurences = VarMap
-- | Represents variable assignments
type Assignments = [(Int, Bool)]

-- | Represents computation state
data SATState = SATState
    { _clauses       :: M.IntMap Clause
    , _occurences    :: M.IntMap Occurences
    , _assignments   :: Assignments
    , _unitClauses   :: [Int]
    -- ^ indices of unit clause candidates (can point to already satisfied clauses)
    , _pureLiterals  :: [(Int, Bool)]
    -- ^ pure literal candidates (possibly already assigned / completely free)
    , _freeVariables :: S.IntSet
    }
    deriving (Eq, Show)

makeLenses ''SATState

-- | An blank state to be initialised by 'initSAT'
blankState :: SATState
blankState = SATState { _clauses       = M.empty
                      , _occurences    = M.empty
                      , _assignments   = []
                      , _unitClauses   = []
                      , _pureLiterals  = []
                      , _freeVariables = S.empty
                      }

-- | A condensed description of a formula in CNF.
--   Contains the number of variables, the number of clauses, 
--   and the clauses represened as lists of integers
type CNF = (Int, Int, [[Int]])
-- | Monad for nondeterministic stateful computation
type SATMonad = St.StateT SATState []

-- | Initialise the computation state based on a 'CNF' representation
initSAT :: CNF -> SATMonad ()
initSAT (varCount, clauseCount, clauseList) = do
    -- create empty VarMaps
    clauses .= M.fromList (zip [1 .. clauseCount] $ repeat emptyVarMap)
    occurences .= M.fromList (zip [1 .. varCount] $ repeat emptyVarMap)
    -- process clauses
    mapM_ (uncurry addClause)    (zip [1 ..] clauseList)
    -- count occurences & literals now to avoid issues
    -- with malformed clauses containing duplicates
    mapM_ (addCounts occurences) [1 .. varCount]
    mapM_ (addCounts clauses)    [1 .. clauseCount]
    -- check for pure literals
    ocMap <- L.use occurences
    mapM_ (uncurry checkPure) (zip [1 ..] (M.elems ocMap))
    -- initialise free variable set
    freeVariables .= S.fromAscList [1 .. varCount]
  where
    addClause :: Int -> [Int] -> SATMonad ()
    addClause clauseIx literals = do
        -- fail if the clause is empty
        guard $ not (null literals)
        mapM_ (addLiteral clauseIx) literals
        when (length literals == 1) $ unitClauses %= (clauseIx :)
    addLiteral :: Int -> Int -> SATMonad ()
    addLiteral clauseIx literal = do
        let val = literal > 0
        let var = abs literal
        clauses . L.ix clauseIx . vmByVal val %= S.insert var
        occurences . L.ix var . vmByVal val %= S.insert clauseIx
    addCounts :: L.Lens' SATState (M.IntMap VarMap) -> Int -> SATMonad ()
    addCounts vm i = L.zoom (vm . L.ix i) $ do
        [posCount, negCount] <- St.gets
            (^.. (vmPositive <> vmNegative) . L.to S.size)
        vmPosCount .= posCount
        vmNegCount .= negCount
    checkPure :: Int -> Occurences -> SATMonad ()
    checkPure var os = do
        let [posCount, negCount] = os ^.. (vmPosCount <> vmNegCount)
        case [posCount, negCount] of
            [0, 0] -> return ()
            [_, 0] -> pureLiterals %= ((var, True) :)
            [0, _] -> pureLiterals %= ((var, False) :)
            [_, _] -> return ()


-- | Do unit propagation, pure literal elimination & nondeterministically 
--   assign values to free variables until there are none left.
--
--   Not guaranteed to find every solution due to how pure literal elimination works.
dpll :: SATMonad ()
dpll = do
    -- deterministic steps (can reach a conflict and fail)
    unitProps
    pureElims
    free <- L.use freeVariables
    if free == S.empty
        -- no more free variables means we're done
        then return ()
        else do
            let var = S.findMin free
            freeVariables %= S.deleteMin
            -- try assigning the variable both ways
            val <- St.lift [True, False]
            assign var val
            dpll

-- | Perform the unit propagation step until there are no unit clauses left
unitProps :: SATMonad ()
unitProps = do
    maybeUnit <- popFrom unitClauses
    -- return if the stack was empty, otherwise process & recur
    case maybeUnit of
        Just unitIx -> assignUnitIx unitIx >> unitProps
        Nothing     -> return ()
  where
    assignUnitIx clauseIx = do
        clauseMaybe <- L.use $ clauses . L.at clauseIx
        -- clause could have been satisfied during another step
        maybe (return ()) assignUnit clauseMaybe
    assignUnit clause = do
        let [pos, neg] = clause ^.. (vmPositive <> vmNegative) . L.to S.toList
        case (pos, neg) of
            ([var], []   ) -> assign var True
            ([]   , [var]) -> assign var False
            ([]   , []   ) -> error "Empty clause on unit clause stack"
            _ ->
                error $ "Non-unit clause on unit clause stack: " ++ show clause

-- | Perform the pure literal elimination step until there are no pure literals left
pureElims :: SATMonad ()
pureElims = do
    maybePure <- popFrom pureLiterals
    case maybePure of
        Just (var, val) -> assignPure var val >> pureElims
        Nothing         -> return ()
  where
    assignPure :: Int -> Bool -> SATMonad ()
    assignPure var val = do
        Sum varOcs <-
            L.use
            $ occurences
            . L.ix var
            . (vmPosCount <> vmNegCount)
            . L.to Sum
        -- 0 occurences mean the variable is either already assigned
        -- or completely free (and should not be deterministically assigned)
        when (varOcs /= 0) $ assign var val

-- | Pop a thing from a stack of things a lens ('unitClauses' or 'pureLiterals') points to
popFrom :: L.Lens' SATState [a] -> SATMonad (Maybe a)
popFrom stackL = do
    stack <- L.use stackL
    case uncons stack of
        Just (item, newStack) -> do
            stackL .= newStack
            return $ Just item
        Nothing -> return Nothing

-- | Assign a value to a variable
--
--   Deletes satisfied clauses, removes opposite literals. Fails if a clause became empty,
--   otherwise checks for new unit clauses and pure literals.
assign :: Int -> Bool -> SATMonad ()
assign var val = do
    Just os <- L.use $ occurences . L.at var
    let [toDelete, toRemoveFrom] = os ^.. (vmByVal val <> vmByVal (not val))
    mapM_ (removeVar var (not val)) (S.toList toRemoveFrom)
    mapM_ deleteClause              (S.toList toDelete)
    freeVariables %= S.delete var
    assignments %= ((var, val) :)

-- | Deletes a satisfied clause, removing it from occurence maps of variables it contained
deleteClause :: Int -> SATMonad ()
deleteClause clauseIx = do
    -- it is an error if the clause does not exist
    Just clause <- L.use $ clauses . L.at clauseIx
    clause & L.traverseOf_ (vmPositive . L.to S.toList . L.traversed)
                           (removeRef clauseIx True)
    clause & L.traverseOf_ (vmNegative . L.to S.toList . L.traversed)
                           (removeRef clauseIx False)
    -- not strictly neccessary, but might as well actually delete it
    clauses %= M.delete clauseIx

-- | Remove reference to a clause (1st arg) from a variable's (3rd arg) occurence map,
--   adding it to the pure literal list if it becomes pure.
removeRef :: Int -> Bool -> Int -> SATMonad ()
removeRef clauseIx val var = do
    L.zoom (occurences . L.ix var) $ do
        vmByVal val . L.at clauseIx .= Nothing
        vmByValC val -= 1
    -- this might result in completely free variables being added to the list,
    -- potentially multiple times, but that does not matter
    Just newOC <- L.preuse $ occurences . L.ix var . vmByValC val
    when (newOC == 0) $ pureLiterals %= ((var, not val) :)

-- | Remove a newly assigned variable (1st arg) from a clause (3rd arg), failing if it becomes empty.
removeVar :: Int -> Bool -> Int -> SATMonad ()
removeVar var val clauseIx = do
    L.zoom (clauses . L.ix clauseIx) $ do
        vmByVal val . L.at var .= Nothing
        vmByValC val -= 1
    Sum newSize <-
        L.use $ clauses . L.ix clauseIx . (vmPosCount <> vmNegCount) . L.to Sum
    -- fail if the clause became empty
    guard $ newSize /= 0
    removeRef clauseIx val var
    when (newSize == 1) $ unitClauses %= (clauseIx :)

-- | Initialise, run, return a list of solutions in a nice format
runDPLL :: CNF -> [[Int]]
runDPLL cnfDesc =
    St.execStateT (initSAT cnfDesc >> dpll) blankState
        ^.. L.each
        .   assignments
        .   L.to toLiterals
  where
    toLiterals :: Assignments -> [Int]
    toLiterals = map toLiteral . sortOn fst
    toLiteral (var, True ) = var
    toLiteral (var, False) = -var

-- | A naïve backtracking SAT solver.
--
--   Its only purpose is to make the actual solver look less bad.
runNaive :: CNF -> [[Int]]
runNaive (varCount, _, clauseList) = reverse
    <$> naive [1 .. varCount] [] (S.fromList <$> clauseList)
  where
    naive :: [Int] -> [Int] -> [S.IntSet] -> [[Int]]
    naive []       as _  = return as
    naive (v : vs) as cs = do
        a <- [v, -v]
        let cs' = S.delete (-a) <$> filter (S.notMember a) cs
        guard $ S.empty `notElem` cs'
        naive vs (a : as) cs'
