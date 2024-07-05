-- | Utilities for testing the solver
module Test where
import qualified Control.Monad.State           as St
import           Data.Either                    ( lefts )
import qualified Data.IntSet                   as S
import           Data.Maybe                     ( catMaybes )
import qualified Lens.Micro.Platform           as L
import           Lens.Micro.Platform            ( (^.) )
import           Solver                         ( CNF
                                                , SATMonad
                                                , SATState
                                                , blankState
                                                , initSAT
                                                , runDPLL
                                                )

-- | Debug function that initialises the state and runs a component of the algorithm
runStep :: SATMonad () -> CNF -> [SATState]
runStep step cnf = St.execStateT (initSAT cnf >> step) blankState

-- | Check if a formula is satisfied bu an assignent
check :: CNF -> [Int] -> Bool
check (_, _, clauses) assignments = all
    (\x -> S.intersection assignments' x /= S.empty)
    clauses'
  where
    assignments' = S.fromList assignments
    clauses'     = S.fromList <$> clauses

-- | Runs 'runDPLL' then wraps correct solutions in 'Right', incorrect ones in 'Left'
runChecked :: CNF -> [Either [Int] [Int]]
runChecked cnf = tag <$> runDPLL cnf
  where
    tag res | check cnf res = Right res
            | otherwise     = Left res

-- | Run the solver expecting no solutions
expectUnsat :: CNF -> Maybe String
expectUnsat cnf = case runChecked cnf of
    cs : _ -> case cs of
        Left  s -> Just $ "Unexpected false positive: " ++ show s
        Right s -> Just $ "Unexpected solution: " ++ show s
    [] -> Nothing

-- | Run the solver expecting at least one solution
expectSat :: CNF -> Maybe String
expectSat cnf = case runChecked cnf of
    cs : _ -> case cs of
        Left  s -> Just $ "Unexpected false positive: " ++ show s
        Right s -> Nothing
    [] -> Just "Unexpectedly unsatisfiable"

-- | Run the solver expecting all solutions to be correct (i.e. actual solutions).
--   Probably very slow for formulas with many solutions.
expectAllCorrect :: CNF -> Maybe String
expectAllCorrect cnf = case lefts $ runChecked cnf of
    []  -> Nothing
    fps -> Just $ "Unexpected false positives: " ++ show fps

