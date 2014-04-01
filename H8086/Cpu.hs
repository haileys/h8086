module H8086.Cpu where

import qualified H8086.Memory as M
import H8086.Utils
import H8086.Instructions

import Control.Monad.State
import Data.Word

data RegisterFile = RegisterFile {
    ax :: Word16,
    bx :: Word16,
    cx :: Word16,
    dx :: Word16,
    sp :: Word16,
    bp :: Word16,
    di :: Word16,
    si :: Word16,
    cs :: Word16,
    ds :: Word16,
    ss :: Word16,
    es :: Word16,
    ip :: Word16,
    flags :: Word16
}

zeroRegisterFile :: RegisterFile
zeroRegisterFile = RegisterFile 0 0 0 0 0 0 0 0 0 0 0 0 0 0

ah = hi . ax
al = lo . ax

bh = hi . bx
bl = lo . ax

data Cpu = Cpu { registers :: RegisterFile, memory :: M.Memory }

initialCpu :: Cpu
initialCpu = Cpu zeroRegisterFile M.zeroMemory

type CpuState a = State Cpu a

runCpu :: Cpu -> CpuState a -> a
runCpu = flip evalState

getRegisterFile :: CpuState RegisterFile
getRegisterFile = fmap registers get

putRegisterFile :: (RegisterFile -> RegisterFile) -> CpuState ()
putRegisterFile fn = do
    cpu <- get
    put cpu { registers = (fn $ registers cpu) }

getMemory :: CpuState M.Memory
getMemory = fmap memory get

readRegister :: (RegisterFile -> Word16) -> CpuState Word16
readRegister fn = fmap fn getRegisterFile

readByte :: M.PhysAddr -> CpuState Word8
readByte addr = fmap (flip M.readByte addr) getMemory

readWord :: M.PhysAddr -> CpuState Word16
readWord addr = fmap (flip M.readWord addr) getMemory

writeByte :: M.PhysAddr -> Word8 -> CpuState ()
writeByte addr byte = do
    cpu <- get
    put cpu { memory = M.writeByte (memory cpu) addr byte }

writeWord :: M.PhysAddr -> Word16 -> CpuState ()
writeWord addr word = do
    cpu <- get
    put cpu { memory = M.writeWord (memory cpu) addr word }

fetchByte :: CpuState Word8
fetchByte = do
    cs' <- readRegister cs
    ip' <- readRegister ip
    byte <- readByte $ M.fromSegmentOffset cs' ip'
    putRegisterFile (\reg -> reg { ip = ip' + 1 })
    return byte

fetchAndDecode :: CpuState Instruction
fetchAndDecode = do
    byte <- fetchByte
    return $ case byte of
        0x37 -> Aaa
        0xc3 -> Ret

disasm :: CpuState [Instruction]
disasm = sequence $ repeat fetchAndDecode
