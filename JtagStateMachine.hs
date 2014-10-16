
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JtagStateMachine where

import System.Environment (getArgs)
import Control.Monad (forM_, mapM_, forM, mapM)
import Control.Monad.IO.Class
import System.IO (withFile, IOMode(..), Handle, hGetContents)
--TODO: Strict or Lazy?
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Trans.Class
import Data.Functor
import Control.Applicative

-- TODO: Jtag state decoder
-- TODO: map ejtg_trst_n and usb_trst_n -> 0 and 1 -- double-check documentation
-- TODO: prepend all 3 `trst`s, chip_reset_n, wait
-- TODO: Could be cool to use pretty-printer to line up format names with data
--
-- TMS_TDI_TDO

-- TODO: filter out any blank line in input


data JtagState = TestLogicReset
               | RunTestIdle
               | SelectDrScan
               | CaptureDr
               | ShiftDr
               | Exit1Dr
               | PauseDr
               | Exit2Dr
               | UpdateDr
               | SelectIrScan
               | CaptureIr
               | ShiftIr
               | Exit1Ir
               | PauseIr
               | Exit2Ir
               | UpdateIr
               deriving (Eq, Show)


jtagStateTrans :: JtagState -> Bool -> JtagState
jtagStateTrans TestLogicReset True = TestLogicReset
jtagStateTrans RunTestIdle    True = SelectDrScan
jtagStateTrans SelectDrScan   True = SelectIrScan
jtagStateTrans CaptureDr      True = Exit1Dr
jtagStateTrans ShiftDr        True = Exit1Dr
jtagStateTrans Exit1Dr        True = UpdateDr
jtagStateTrans PauseDr        True = Exit2Dr
jtagStateTrans Exit2Dr        True = UpdateDr
jtagStateTrans UpdateDr       True = SelectDrScan
jtagStateTrans SelectIrScan   True = TestLogicReset
jtagStateTrans CaptureIr      True = Exit1Ir
jtagStateTrans ShiftIr        True = Exit1Ir
jtagStateTrans Exit1Ir        True = UpdateIr
jtagStateTrans PauseIr        True = Exit2Ir
jtagStateTrans Exit2Ir        True = UpdateIr
jtagStateTrans UpdateIr       True = SelectDrScan
jtagStateTrans TestLogicReset False = RunTestIdle
jtagStateTrans RunTestIdle    False = RunTestIdle
jtagStateTrans SelectDrScan   False = CaptureDr
jtagStateTrans CaptureDr      False = ShiftDr
jtagStateTrans ShiftDr        False = ShiftDr
jtagStateTrans Exit1Dr        False = PauseDr
jtagStateTrans PauseDr        False = PauseDr
jtagStateTrans Exit2Dr        False = ShiftDr
jtagStateTrans UpdateDr       False = RunTestIdle
jtagStateTrans SelectIrScan   False = CaptureIr
jtagStateTrans CaptureIr      False = ShiftIr
jtagStateTrans ShiftIr        False = ShiftIr
jtagStateTrans Exit1Ir        False = PauseIr
jtagStateTrans PauseIr        False = PauseIr
jtagStateTrans Exit2Ir        False = ShiftIr
jtagStateTrans UpdateIr       False = RunTestIdle

newtype JtagS a = JTS { runJTS :: StateT JtagState Identity a }
           deriving (Functor, Monad, Applicative) --MonadIO, 

-- runJTS :: JtagS a -> StateT JtagState IO a
--runJTS (JTS m) = m

-- TODO: Use Debug.Trace
avcFromVec :: String -> JtagS String
avcFromVec is = do
  let tms = is !! 0
  let tdi = is !! 2
  let tdo = is !! 4
  JTS $ modify (flip jtagStateTrans (tms == '1'))
  s <- JTS get
  return $ preLine ++ [tms, tdi, tdo] ++ tck ++ rstNone ++ postLine ++ (show s)

-- unJtagS :: JtagS String -> String
-- unJtagS js = runJTS js

convertHandle :: Handle -> IO ()
convertHandle h = do
  c <- hGetContents h
  let ls = words c
  let as = evalStateT
          (runJTS (forM ls $ \l -> (avcFromVec l)))
          TestLogicReset
  mapM_ putStrLn (runIdentity as)

convertFileName :: String -> IO ()
convertFileName filename = do
  withFile filename ReadMode convertHandle

formatLine = "FORMAT jtg_tms jtg_tdi jtg_tdo jtg_tck jtg_trst_n ejtg_trst_n usb_jtg_trst_n chip_reset_n dcok_n;"
preLine = "cyc 1 "
postLine = " ; "
rstHard = "00000"
rstJtag = "00001"
rstChip = "10101"
rstNone = "10111"
tck     = "T" -- tck always ticks
tapInRst = "00XT" -- jtag pins while reset

-- TODO: use types to make it clearer what's going on rather than just gluing strings
-- TODO: Can I hold chip_reset_n while testing the phys?

resetHard = rptLine 10   $ tapInRst ++ rstHard ++ (comment "Hard Reset")
resetJtag = rptLine 10   $ tapInRst ++ rstJtag ++ (comment "JTAG Reset")
resetChip = rptLine 10   $ tapInRst ++ rstChip ++ (comment "Chip Reset")
resetNone = rptLine 1000 $ tapInRst ++ rstNone ++ (comment "Wait after reset")
resetDone = rptLine 10   $ tapInRst ++ rstHard ++ (comment "DCOK low before disconnect")

comment s = " ; " ++ s
reset = unlines [resetHard, resetJtag, resetChip, resetNone]

rptLine n s = "cyc " ++ (show n) ++ " " ++ s

-- TODO: Write an .avc file per .vec input file instead of concatenating them like now
main :: IO ()
main = do
  putStrLn formatLine
  putStrLn reset
  mapM_ convertFileName =<< getArgs
  putStrLn resetDone

-- TODO: Make reset and resetDone their own vectors place use multi-port burst to prepend them or not
