-- | The SAT solver's front end / IO part
-- 
--   Functions with @IO Bool@ return type return 'True' if an error has occured,
--   'False' if OK. (This is to allow 'onDir' to summarise after)
module SimpleSat
    ( parseCNF
    , runPrintResults
    , runNaivePrintResults
    , runTest
    , onFile
    , onDir
    ) where
import           Control.Applicative            ( Alternative((<|>))
                                                , Applicative(liftA2)
                                                )
import           Control.Monad                  ( guard
                                                , replicateM
                                                , void
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Solver                         ( CNF
                                                , runDPLL
                                                , runNaive
                                                )
import           System.Directory               ( listDirectory )
import           Test
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC
import           Text.Read                      ( readMaybe )

-- | Read problem from file, run a function on it
--   ('runPrintResults' or 'runTest' are made for this)
--
--   Ex.: @ runPrintResults 5 \`onFile\` "input.cnf" @
onFile :: (CNF -> IO Bool) -> FilePath -> IO Bool
onFile f file = do
    putStrLn $ file ++ ":"
    contents <- readFile file
    case parseCNF contents of
        Nothing  -> putStrLn "Could not parse file" >> return True
        Just cnf -> f cnf

-- | Run 'onFile' on all files in a directory
--
--   Ex.: @ runTest expectAllCorrect \`onDir\` "input_dir" @
onDir :: (CNF -> IO Bool) -> FilePath -> IO ()
onDir fun dir = do
    files <- listDirectory dir
    let files' = fmap (\file -> dir ++ "/" ++ file) files
    results <- mapM (onFile fun) files'
    case snd <$> filter fst (zip results files) of
        []     -> putStrLn "ALL OK"
        failed -> putStrLn $ "Encountered errors in: " ++ unwords failed



-- | Run the DPLL solver and print up to n solutions
-- 
--   Ex.: @ runPrintResults 2 (3, 2, [[-1, 2], [1, 3]]) @
runPrintResults :: Int -> CNF -> IO Bool
runPrintResults resCount cnf = do
    let results = take resCount $ runDPLL cnf
    case results of
        [] -> putStrLn "Unsatisfiable" >> return True
        _  -> do
            putStrLn "Satisfiable:"
            mapM_ (putStrLn . unwords . map show) results
            return False

-- | Run the naive solver and print up to n solutions
runNaivePrintResults :: Int -> CNF -> IO Bool
runNaivePrintResults resCount cnf = do
    let results = take resCount $ runNaive cnf
    case results of
        [] -> putStrLn "Unsatisfiable" >> return True
        _  -> do
            putStrLn "Satisfiable:"
            mapM_ (putStrLn . unwords . map show) results
            return False

-- | Run a test, print \"OK\" or reported error
--
--   Ex.: @ runTest 'expectUnsat' (1, 2, [[-1], [1]]) @
runTest :: (CNF -> Maybe String) -> CNF -> IO Bool
runTest test cnf = case test cnf of
    Nothing  -> putStrLn "OK" >> return False
    Just err -> putStrLn err >> return True

-- | Parse a problem in the DIMACS CNF format
parseCNF :: String -> Maybe CNF
parseCNF = MP.parseMaybe $ do
    MP.many commentLine
    (varCount, clauseCount) <- problemLine
    clauses                 <- MP.many (MP.try clauseLine)
    -- some .cnf files I've found had some trailing characters
    MP.takeRest
    return (varCount, clauseCount, clauses)
  where
    commentLine :: MP.Parsec () String ()
    commentLine = void $ do
        MPC.char 'c'
        MP.many (MP.noneOf "\r\n")
        MPC.eol
    problemLine :: MP.Parsec () String (Int, Int)
    problemLine = do
        MPC.char 'p'
        MPC.space
        MPC.string "cnf"
        MPC.space
        varCount <- readMaybe <$> MP.some MPC.digitChar
        MPC.space
        clauseCount <- readMaybe <$> MP.some MPC.digitChar
        -- MPC.space would consume the EOL as well
        MP.many $ MPC.char ' '
        MPC.eol
        case (varCount, clauseCount) of
            (Just vc, Just cc) -> return (vc, cc)
            _                  -> MP.customFailure ()
    clauseLine :: MP.Parsec () String [Int]
    clauseLine = do
        literals <- MP.many (MP.try $ MPC.space *> nonZeroInt)
        MPC.space
        MPC.char '0'
        MPC.eol
        return literals
    nonZeroInt :: MP.Parsec () String Int
    nonZeroInt = do
        numLiteral <-
            liftA2 (:) (MPC.char '-') (MP.some $ MP.oneOf ['0' .. '9'])
                <|> MP.some (MP.oneOf ['0' .. '9'])
        case readMaybe numLiteral of
            Just 0  -> MP.customFailure ()
            Just n  -> return n
            Nothing -> MP.customFailure ()

