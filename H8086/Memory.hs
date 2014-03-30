module H8086.Memory (
    Memory,
    zeroMemory,
    readByte,
    readWord,
    writeByte,
    writeWord,
    PhysAddr,
    mkPhysAddr,
    fromSegmentOffset,
    addrAdd
) where

import H8086.Utils

import Data.Word
import Data.Bits
import Data.Array

type Memory = Array PhysAddr Word8

zeroMemory :: Memory
zeroMemory = listArray (0, 0xfffff) $ replicate 0x100000 0

readByte :: Memory -> PhysAddr -> Word8
readByte = (!)

readWord :: Memory -> PhysAddr -> Word16
readWord mem addr = fromLoHi (readByte mem addr) (readByte mem $ addrAdd addr 1)

writeByte :: Memory -> PhysAddr -> Word8 -> Memory
writeByte mem addr byte = mem // [(addr, byte)]

writeWord :: Memory -> PhysAddr -> Word16 -> Memory
writeWord mem addr word = mem // [(addr, lo word), (addrAdd addr 1, hi word)]

type PhysAddr = Int

mkPhysAddr :: Int -> PhysAddr
mkPhysAddr addr = fromIntegral $ (.&.) addr 0xfffff

fromSegmentOffset :: Word16 -> Word16 -> PhysAddr
fromSegmentOffset segment offset = mkPhysAddr $ (fromIntegral segment) * 16 + (fromIntegral offset)

addrAdd :: PhysAddr -> Int -> PhysAddr
addrAdd phys offset = mkPhysAddr $ phys + offset
