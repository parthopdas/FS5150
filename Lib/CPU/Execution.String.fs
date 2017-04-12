namespace Lib.CPU.Execution

(* 
    REP REPE/REPZ REPNE/REPNZ MOVS/MOVSB/MOVSW COMPS/COMPSB/COMPSW SCAS/SCASB/SCASW LODS/LODSB/LODSW STOS/STOSB/STOSW
    
    [ ] MOVS/MOVSB/MOVSW
    - DS:SI -> ES:DI
      - DS can be overridden
    - rep with CX
      - After each repetition
        - Initial CX 0 => 0 repetitions
        - CX decremented after each iteration
        - SI/DI: if DF -- else ++
    - No flags affected
    
    [ ] CMPS/CMPSB/CMPSW 
    - cmp DS:SI ES:DI
      - DS can be overridden
    - repz/repnz with CX & ZF
      - After each repetition
        - Initial CX 0 => 0 repetitions
        - CX decremented after each iteration
        - SI/DI: if DF -- else ++
    - OSZAPC affected
    
    [v] SCAS/SCASB/SCASW 
    - cmp A? ES:DI
    - repz/repnz with CX & ZF
      - After each repetition
        - Initial CX 0 => 0 repetitions
        - CX decremented after each iteration
        - SI/DI: if DF -- else ++
    - ZF has the result of the scan
    - OSZAPC affected
    
    [v] LODS/LODSB/LODSW 
    - mov A? ES:DI
    - rep with CX
      - After each repetition
        - Initial CX 0 => 0 repetitions
        - CX decremented after each iteration
        - SI/DI: if DF -- else ++
    - No flags affected
    
    [v] STOS/STOSB/STOSW
    - mov ES:DI A?
    - rep with CX
      - After each repetition
        - Initial CX 0 => 0 repetitions
        - CX decremented after each iteration
        - SI/DI: if DF -- else ++
    - No flags affected
*)

module String = 
    open FSharpx
    open FSharpx.State
    open Lib.CPU.Execution.Common
    open Lib.Domain.InstructionSet
    open Lib.Domain.PC
    open YaFunTK
    
    module MOVSX = 
        let execMOVSB _ = nyi
        let execMOVSW _ = nyi
    
    module CMPSX = 
        let execCMPSB _ = nyi
        let execCMPSW _ = nyi
    
    module SCASX = 
        let coreSCASX getAcc read sub n = 
            let readFromESDI = (@|@) <!> getRegSeg ES <*> getReg16 DI >>= read
            let subAccESDI = Prelude.tuple2 <!> getAcc <*> readFromESDI >>= sub
            
            let updateDI = 
                getFlag Flags.DF >>= (fun df -> 
                let update = 
                    if df then (-)
                    else (+)
                getReg16 DI >>= (Prelude.flip update n >> setReg16 DI))
            
            let goBack = getLogicalInstrStart >>= (Some >> State.returnM)
            
            let whileDf f = 
                getFlag Flags.ZF >>= fun zf -> 
                    if f zf then goBack
                    else ns
            Prelude.tuple2 <!> getRepetitionType <*> getReg16 CX >>= (function 
            | Some _, 0us -> ns
            | None, 0us -> subAccESDI *> updateDI *> ns
            | None, _ -> subAccESDI *> updateDI *> ns
            | Some WhileZero, cx -> subAccESDI *> updateDI *> (setReg16 CX (cx - 1us)) *> whileDf id
            | Some WhileNotZero, cx -> subAccESDI *> updateDI *> (setReg16 CX (cx - 1us)) *> whileDf not)
        
        let coreSCASB = coreSCASX (getReg8 AL) readWord8 setSub8Flags 1us
        let inline execSCASB _ = coreSCASB
        let coreSCASW = coreSCASX (getReg16 AX) readWord16 setSub16Flags 2us
        let inline execSCASW _ = coreSCASW
    
    module LODXSTOX = 
        let core cpData xi n = 
            let updateXI = 
                getFlag Flags.DF >>= (fun df -> 
                let update = 
                    if df then (-)
                    else (+)
                getReg16 xi >>= (Prelude.flip update n >> setReg16 xi))
            
            let goBack = getLogicalInstrStart >>= (Some >> State.returnM)
            Prelude.tuple2 <!> getRepetitionType <*> getReg16 CX >>= (function 
            | Some _, 0us -> ns
            | None, 0us -> cpData *> updateXI *> ns
            | None, _ -> cpData *> updateXI *> ns
            | Some _, cx -> cpData *> updateXI *> (setReg16 CX (cx - 1us)) *> goBack)
    
    module LODSX = 
        let inline private coreLODSX read setAcc n = 
            let readFromDSSI = (@|@) <!> getRegSeg DS <*> getReg16 SI >>= read
            LODXSTOX.core (readFromDSSI >>= setAcc) SI n
        
        let inline execLODSB _ = coreLODSX readWord8 (setReg8 AL) 1us
        let inline execLODSW _ = coreLODSX readWord16 (setReg16 AX) 2us
    
    module STOSX = 
        let inline private coreSTOSX readAcc write n = 
            let writeToESDI v = (@|@) <!> getRegSeg ES <*> getReg16 DI >>= write v
            LODXSTOX.core (readAcc >>= writeToESDI) DI n
        
        let inline execSTOSB _ = coreSTOSX (getReg8 AL) writeWord8 1us
        let inline execSTOSW _ = coreSTOSX (getReg16 AX) writeWord16 2us
