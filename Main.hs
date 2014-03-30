import H8086.Cpu

main =
    print $ head $ runCpu initialCpu $ do
        writeByte 0 0xc3
        disasm
