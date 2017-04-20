namespace Lib.Chips.I8088

module Common =
    
    let signExtend (w8 : uint8) : uint16 = 
        if w8 < 0x80uy then 
            (uint16)w8 
        else 
            (uint16)w8 ||| 0xFF00us
    