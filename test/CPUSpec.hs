module CPUSpec where

import ClassyPrelude
import Control.Monad.State
import Control.Monad.Reader
import CPU
import Lens.Micro.Platform
import Test.Hspec

spec :: Spec
spec = do
  describe "step" $ do
    it "can move a value between registers" $ do
      (testInsIndex 0)^.registers.ax `shouldBe` 4
    it "can move a value from a RAM address into a register" $ do
      (testInsIndex 1)^.registers.cx `shouldBe` 1
    it "can move a value into a register" $ do
      (testInsIndex 3)^.registers.instPointer `shouldBe` 101
    it "can exchange two registers" $ do
      let newState = testInsIndex 11 in (newState^.registers.ax, newState^.registers.dx)
        `shouldBe` (testState^.registers.dx, testState^.registers.ax)

testIns :: Instruction -> CPUState
testIns instruction = snd $ runState (runReaderT (step instruction) testConfig) testState

testInsIndex :: Int -> CPUState
testInsIndex i = testIns (indexEx testInstructions i)

testFlags :: Flags
testFlags = Flags Unset Unset Unset Unset Unset Unset Unset Unset Unset

testRegisters :: Registers
testRegisters = Registers 2 4 3 1 0 0 0 0 0 0 0 0 0

testRam :: RAM
testRam = RAM [0, 0, 234, 100, 0, 1, 0, 0]

testState :: CPUState
testState = CPUState testFlags testRegisters testRam

testConfig :: CPUConfig
testConfig = CPUConfig Intel8086 5000000

testInstructions :: [Instruction]
testInstructions = [ Mov (Register ax) (Register bx)
                   , Mov (Register cx) (Address 4)
                   , Mov (Address 5) (Register dx)
                   , Mov (Register instPointer) (Value 101)
                   , Mov (Address 6) (Value 103)
                   , Push (Register dx)
                   , Push (Address 3)
                   , Push (Value 105) -- should fail
                   , Pop (Register ax)
                   , Pop (Address 4)
                   , Pop (Value 105) -- should fail
                   , Xchg (Register ax) (Register dx)
                   , Xchg (Address 6) (Register bx)
                   , Xchg (Register cx) (Address 0)
                   , Xchg (Address 3) (Address 4) -- should fail
                   ]
