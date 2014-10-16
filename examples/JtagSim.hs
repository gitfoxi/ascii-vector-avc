
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Only very simple AVCs are supported.
-- Will choke on SQPG
-- Rrep repeats are echoed but it's assumed all signals are X1 so
-- the clock won't tick during them so it doesn't affect the simulation.
-- State characters are 0, 1 for inputs and L, H, X, Z for outputs but still
-- X, Z are treated like L.

import           Control.Arrow ((***))
import           Control.Applicative ((<$>))
import           Control.Monad (when)
import           Control.Monad.State

import           Data.Bits (shiftL)
import qualified Data.ByteString.Char8 as BS
import           Data.List (elemIndex, intercalate, zip4, zip5)
import qualified Data.List as List
import           Data.List.Split (splitOn)
import           Data.Maybe (isNothing, catMaybes, fromJust)

import           Numeric (showHex)

import           System.Environment (getArgs)
import           System.IO (stderr, hPutStrLn)

import Data.Avc
import Data.Avc.Util
import qualified JtagStateMachine as J
import           JtagStateMachine (JtagState(..), jtagStateTrans)

-- Place state of Jtag state machine in avc comments
-- assume jtag signals use standard Cavium names for this simple example

[trst, tck, tms, tdi, tdo] = splitOn " " "jtg_trst_n jtg_tck jtg_tms jtg_tdi jtg_tdo"
sigNames = [trst, tck, tms, tdi, tdo]

main = do
    args <- getArgs
    when (1 /= length args) $ error "Usage JtagSim file.avc"
    let [filename] = args
    statements <- parseAvcFile filename
    -- TODO: catch exception when no FORMAT found
    -- TODO: throw when AVC has unhadnled statement like LOOP which could throw me off a lot
    let idxs = getFormatIdxs statements sigNames
    -- mapM (BS.putStrLn . toString) $ selectSigs idxs statements
    -- let flatStates = flattenStatements statements
    let rep_jtagio = unpackSigs $ selectSigs idxs statements
    let jtagio = map snd rep_jtagio
    let sim = simStateMachine jtagio
    -- TODO: May simplify things to expand the repeats up front
    let ir = simIRegister sim jtagio
    let dr = simDRegister sim jtagio
    let irReg = reg ir sim UpdateIr
    let drReg = reg dr sim UpdateDr
    mapM_ print $ zip jtagio (squelch2 $ zip5 sim (hexStr <$> ir) (hexStr <$> dr) (hexStr <$> irReg) (hexStr <$> drReg))
    -- mapM_ print $ zip rep_jtagio sim
    -- Property: length sim == length jtagio -- but Len sim: 109744 Len jtagio: 109743
    hPutStrLn stderr $ "Len sim: " ++ show (length sim) ++ " Len jtagio: " ++ show (length jtagio)

-- remember the last updated value
reg :: [(Integer, Integer)]                        -- simIRegister / simDRegister
    -> [JtagState]                                 -- sim
    -> JtagState                                   -- UpdateIr
    -> [(Integer, Integer)]                        -- updated contents
reg inOut jst0 updst = evalState (mapM reg' (zip inOut jst0)) (0, 0)
  where
    reg' :: ((Integer, Integer), JtagState)
         -> State (Integer, Integer) (Integer, Integer)
    reg' ((inReg, outReg), jst)
      | jst == updst = do
        put (inReg, outReg)
        get
      | otherwise = get

hexStr :: (Show a, Integral a, Show b, Integral b) => (a, b) -> String
hexStr (a, b) = show $ concat ["(", showHex a ",", showHex b ")"]

squelch :: (Eq a, Show a) => [a] -> [String]
squelch a = zipWith (curry squelch') (Nothing:(Just <$> a)) (Just <$> a)
  where
    squelch' (Just old, Just new)
      | old /= new = show new
      | otherwise  = ""
    squelch' _   = ""

-- squelch using StateT
squelch2 :: (Eq a, Show a) => [a] -> [Maybe a]
squelch2 as = evalState (mapM (squelch2' . Just) as) (Nothing :: Maybe a)
  where
    squelch2' :: (Eq a, Show a) => Maybe a -> State (Maybe a) (Maybe a)
    squelch2' new = do
        old <- get
        put new
        return $ if old /= new then new else Nothing


-- (Integer, Integer) is value scanned (in, out) most recently
-- simIRegister :: [JtagState] -> [BS.ByteString] -> [(Integer, Integer)]
-- simIRegister jss0 ios0 = simIRegister' $ zip jss0 ios0

simIRegister :: [JtagState] -> [BS.ByteString] -> [(Integer, Integer)]
simIRegister = simRegister CaptureIr ShiftIr

simDRegister :: [JtagState] -> [BS.ByteString] -> [(Integer, Integer)]
simDRegister = simRegister CaptureDr ShiftDr

simRegister :: JtagState -> JtagState -> [JtagState] -> [BS.ByteString] -> [(Integer, Integer)]
simRegister capState shiftState jss0 ios0 = simIRegister' $ zip jss0 ios0
  where
    simIRegister' :: [(JtagState, BS.ByteString)] -> [(Integer, Integer)]
    simIRegister' jss_ios = map nums bools
      where
        nums = fromBools *** fromBools
        bools = scanl go ([], []) jss_ios
        -- TODO: only respond to rising TCK not just TCK=1
        go (regIn, regOut) (_, "") = (regIn, regOut)
        go (regIn, regOut) (jtState, io)
          | stck && jtState == capState   = ([], [])
          | stck && jtState == shiftState = (stdi:regIn, stdo:regOut)
          | otherwise                     = (regIn, regOut)
          where stdi = '1' == io `BS.index` 3
                stdo = 'H' == io `BS.index` 4
                stck = '1' == io `BS.index` 1

-- head is MSB
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
fromBools :: [Bool] -> Integer
fromBools = List.foldl' f 0
  where
    f acc b = shiftL acc 1 + if b then 1 else 0

scanl' f = scanl (flip f)

-- TODO: Repeated vectors should have no effect on my simulation since
--       the clock doesn't tick. (On the other hand, it's possible to have
--       vectors representing a cycling clock, just not the vector I'm looking at now.)
-- TODO: really might want to QuickCheck this shit here
-- TODO: use trst in getting next state too
simStateMachine :: [BS.ByteString] -> [JtagState]
simStateMachine repIo
    = map fst (scanl' nextState (TestLogicReset, "1111Z") repIo)
  where --        new io             old state  old io             new state  new io
    nextState :: BS.ByteString -> (JtagState, BS.ByteString) -> (JtagState, BS.ByteString)
    nextState "" (oldState, oldIo) = (oldState, oldIo) -- blank new state, ignore
    nextState newIo (oldState, oldIo) = if tckHighEdge oldIo newIo
        then (newState, newIo)  -- repeats
        else (oldState, newIo) -- no tck tick don't change state
      where
        newState = updateState oldState newIo
    tckHighEdge oldIo newIo
        = not (stck oldIo) && stck newIo
    strst io = '1' == io `BS.index` 0
    stck io = '1' == io `BS.index` 1
    stms io = '1' == io `BS.index` 2
    updateState oldState newIo = if not $ strst newIo
        then TestLogicReset
        else jtagStateTrans oldState (stms newIo)


-- Returns (Repeat, [State]) where State is represented by a bytestring
unpackSigs :: [Statement] -> [(Int, BS.ByteString)]
unpackSigs = map unpackRepeat
    where
        unpackRepeat (Repeat rep _ vec _) = (rep, vec)
        unpackRepeat _ = (0, "") -- a non-state statement

-- | Take a format and a list of signal names and return the indecies
getFormatIdxs :: [Statement] -> [String] -> [Int]
getFormatIdxs statements names =
    let format = head $ filter isFormat statements
        midxs  = getSigIdxs format (map BS.pack names)
    in if Nothing `elem` midxs
        then error $ "Signals not found in format: " ++ nameMissingSignals midxs names
        else catMaybes midxs
    
