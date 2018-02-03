{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------
-- |
-- Module    :  CPU
-- Copyright :  (c) Nnotm 2018
-- License   :  BSD3
-- Maintainer:  mail.nnotm@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

-- TODO: write proper unit tests

module CPU where

import ClassyPrelude
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Reader
import Data.Bits
import Data.Vector.Unboxed
import Lens.Micro.Platform

type RAMSize = Int

data CPUModel = Intel8086
              | Intel8088
              deriving (Show)

data CPUConfig = CPUConfig
  { _cpumodel :: CPUModel
  , _cpufreq :: Int
  } deriving (Show)

data Flag = Set | Unset deriving (Show)

data Flags = Flags
  { _carryFlag :: Flag
  , _parityFlag :: Flag
  , _adjustFlag :: Flag
  , _zeroFlag :: Flag
  , _signFlag :: Flag
  , _trapFlag :: Flag
  , _intEnableFlag :: Flag
  , _directionFlag :: Flag
  , _overflowFlag :: Flag
  } deriving (Show)

-- TODO: implement ah and al as lenses (alternatively have ah and al as
-- registers and implement ax as lens, might be easier)
data Registers = Registers
  { _ax :: Word
  , _bx :: Word
  , _cx :: Word
  , _dx :: Word
  , _stackPointer :: Word
  , _basePointer :: Word
  , _sourceIndex :: Word
  , _destIndex :: Word
  , _instPointer :: Word
  , _codeSegment :: Word
  , _dataSegment :: Word
  , _stackSegment :: Word
  , _extraSegment :: Word
  } deriving (Show)

newtype RAM = RAM [Word8] deriving (Show)

-- TODO: think about whether stack should be modelled separately or as part of
-- RAM
data CPUState = CPUState
  { _flags :: Flags
  , _registers :: Registers
  , _ram :: RAM
  } deriving (Show)

makeLenses ''CPUState
makeLenses ''Flags
makeLenses ''Registers

type Register = Lens' Registers Word

data AddressVal = Register (Lens' Registers Word)
                | Address Word32
                | Value Word

data Instruction = Mov { movSource :: AddressVal, movDest :: AddressVal }
                 | Push { pushAddr :: AddressVal }
                 | Pop { popAddr :: AddressVal }
                 | Xchg { xchgL :: AddressVal, xchgR :: AddressVal }

-- TODO: put combining two Word8s into its own function (probably also write
-- functions for getting the high and low parts of Word16s)
-- TODO: make an Address type instead of using Word32
-- TODO: indexRAM8
-- TODO: these should be lenses
-- TODO: think about endianness
indexRAM16 :: RAM -> Word32 -> Word
indexRAM16 (RAM xs) (fromIntegral -> i) = shift (fromIntegral wordHigh :: Word) 8 + wordLow
  where wordLow :: Word
        wordLow = fromIntegral $ indexEx xs (i + 1)
        wordHigh :: Word
        wordHigh = fromIntegral $ indexEx xs i

(.=<) :: MonadState s m => ASetter s s a b -> Getting b s b -> m ()
a .=< b = assign a =<< use b

-- TODO: distinguish Word vs Byte instructions
step :: Instruction -> ReaderT CPUConfig (State CPUState) ()
step (Mov (Register ra) (Register rb)) = zoom registers $ ra .=< rb
step (Mov (Register r) (Address addr)) = do mram <- use ram
                                            registers.r .= indexRAM16 mram addr
step (Xchg (Register rl) (Register rr)) = zoom registers $ do mrl <- use rl
                                                              mrr <- use rr
                                                              rl .= mrr
                                                              rr .= mrl

-- initializeRAM :: RAMSize -> MVector Word8
-- initializeRAM :: Int -> ST (MVector RealWorld Double) ()
-- initializeRAM size = thaw $ Data.Vector.Unboxed.replicate size 0
