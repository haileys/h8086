module H8086.Instructions where

import H8086.Memory

data Register
    = AX | BX | CX | DX
    | AH | BH | CH | DH
    | AL | BL | CL | DL
    | DI | SI | BP | SP
    deriving (Show)

data Segment
    = CS | DS | ES | SS
    deriving (Show)

data Instruction
    = Aaa
    | Aad
    | Aam
    | Aas
    | AdcRegMem Register PhysAddr
    | AdcMemReg PhysAddr Register
    | AdcRegReg Register Register
    --
    | Ret
    deriving (Show)
