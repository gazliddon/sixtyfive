
{:opcode :ADC
 :mnemonic       "ADC"
 :help-text      "ADd with Carry"

 :flags          "S V Z C"

 :addressing-modes {0x69 :immediate    ;; size 2  2 cycles
                    0x65 :zero-page    ;; size 2  3 cycles
                    0x75 :zero-page-x  ;; size 2  4 cycles
                    0x6d :absolute     ;; size 3  4 cycles
                    0x7d :absolute-x   ;; size 3  4+ cycles
                    0x79 :absolute-y   ;; size 3  4+ cycles
                    0x61 :indirect-x   ;; size 2  6 cycles
                    0x71 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :AND
 :mnemonic       "AND"
 :help-text      "bitwise AND with accumulator"

 :flags          "S Z"

 :addressing-modes {0x29 :immediate    ;; size 2  2 cycles
                    0x25 :zero-page    ;; size 2  3 cycles
                    0x35 :zero-page-x  ;; size 2  4 cycles
                    0x2d :absolute     ;; size 3  4 cycles
                    0x3d :absolute-x   ;; size 3  4+ cycles
                    0x39 :absolute-y   ;; size 3  4+ cycles
                    0x21 :indirect-x   ;; size 2  6 cycles
                    0x31 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :ASL
 :mnemonic       "ASL"
 :help-text      "Arithmetic Shift Left"

 :flags          "S Z C"

 :addressing-modes {0x0a :accumulator  ;; size 1  2 cycles
                    0x06 :zero-page    ;; size 2  5 cycles
                    0x16 :zero-page-x  ;; size 2  6 cycles
                    0x0e :absolute     ;; size 3  6 cycles
                    0x1e :absolute-x   ;; size 3  7 cycles
                    } }

{:opcode :BIT
 :mnemonic       "BIT"
 :help-text      "test BITs"

 :flags          "N V Z"

 :addressing-modes {0x24 :zero-page ;; size 2  3 cycles
                    0x2c :absolute  ;; size 3  4 cycles
                    } }

{:opcode :BRK
 :mnemonic       "BRK"
 :help-text      "BReaK"

 :flags          "B"

 :addressing-modes {0x00 :implied  ;; size 1  7 cycles
                    } } 

{:opcode :CMP
 :mnemonic       "CMP"
 :help-text      "CoMPare accumulator"

 :flags          "S Z C"

 :addressing-modes {0xc9 :immediate    ;; size 2  2 cycles
                    0xc5 :zero-page    ;; size 2  3 cycles
                    0xd5 :zero-page-x  ;; size 2  4 cycles
                    0xcd :absolute     ;; size 3  4 cycles
                    0xdd :absolute-x   ;; size 3  4+ cycles
                    0xd9 :absolute-y   ;; size 3  4+ cycles
                    0xc1 :indirect-x   ;; size 2  6 cycles
                    0xd1 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :CPX
 :mnemonic       "CPX"
 :help-text      "ComPare X register"

 :flags          "S Z C"

 :addressing-modes {0xe0 :immediate  ;; size 2  2 cycles
                    0xe4 :zero-page  ;; size 2  3 cycles
                    0xec :absolute   ;; size 3  4 cycles
                    } }

{:opcode :CPY
 :mnemonic       "CPY"
 :help-text      "ComPare Y register"

 :flags          "S Z C"

 :addressing-modes {0xc0 :immediate  ;; size 2  2 cycles
                    0xc4 :zero-page  ;; size 2  3 cycles
                    0xcc :absolute   ;; size 3  4 cycles
                    } }

{:opcode :DEC
 :mnemonic       "DEC"
 :help-text      "DECrement memory"

 :flags          "S Z"

 :addressing-modes {0xc6 :zero-page    ;; size 2  5 cycles
                    0xd6 :zero-page-x  ;; size 2  6 cycles
                    0xce :absolute     ;; size 3  6 cycles
                    0xde :absolute-x   ;; size 3  7 cycles
                    } }

{:opcode :EOR
 :mnemonic       "EOR"
 :help-text      "bitwise Exclusive OR"

 :flags          "S Z"

 :addressing-modes {0x49 :immediate    ;; size 2  2 cycles
                    0x45 :zero-page    ;; size 2  3 cycles
                    0x55 :zero-page-x  ;; size 2  4 cycles
                    0x4d :absolute     ;; size 3  4 cycles
                    0x5d :absolute-x   ;; size 3  4+ cycles
                    0x59 :absolute-y   ;; size 3  4+ cycles
                    0x41 :indirect-x   ;; size 2  6 cycles
                    0x51 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :INC
 :mnemonic       "INC"
 :help-text      "INCrement memory"

 :flags          "S Z"

 :addressing-modes {0xe6 :zero-page    ;; size 2  5 cycles
                    0xf6 :zero-page-x  ;; size 2  6 cycles
                    0xee :absolute     ;; size 3  6 cycles
                    0xfe :absolute-x   ;; size 3  7 cycles
                    } }

{:opcode :JMP
 :mnemonic       "JMP"
 :help-text      "JuMP"

 :flags          "none"

 :addressing-modes {0x4c :absolute  ;; size 3  3 cycles
                    0x6c :indirect  ;; size 3  5 cycles
                    } }

{:opcode :JSR
 :mnemonic       "JSR"
 :help-text      "Jump to SubRoutine"

 :flags          "none"

 :addressing-modes {0x20 :absolute  ;; size 3  6 cycles
                    } } 

{:opcode :LDA
 :mnemonic       "LDA"
 :help-text      "LoaD Accumulator"

 :flags          "S Z"

 :addressing-modes {0xa9 :immediate    ;; size 2  2 cycles
                    0xa5 :zero-page    ;; size 2  3 cycles
                    0xb5 :zero-page-x  ;; size 2  4 cycles
                    0xad :absolute     ;; size 3  4 cycles
                    0xbd :absolute-x   ;; size 3  4+ cycles
                    0xb9 :absolute-y   ;; size 3  4+ cycles
                    0xa1 :indirect-x   ;; size 2  6 cycles
                    0xb1 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :LDX
 :mnemonic       "LDX"
 :help-text      "LoaD X register"

 :flags          "S Z"

 :addressing-modes {0xa2 :immediate    ;; size 2  2 cycles
                    0xa6 :zero-page    ;; size 2  3 cycles
                    0xb6 :zero-page-y  ;; size 2  4 cycles
                    0xae :absolute     ;; size 3  4 cycles
                    0xbe :absolute-y   ;; size 3  4+ cycles
                    } }

{:opcode :LDY
 :mnemonic       "LDY"
 :help-text      "LoaD Y register"

 :flags          "S Z"

 :addressing-modes {0xa0 :immediate    ;; size 2  2 cycles
                    0xa4 :zero-page    ;; size 2  3 cycles
                    0xb4 :zero-page-x  ;; size 2  4 cycles
                    0xac :absolute     ;; size 3  4 cycles
                    0xbc :absolute-x   ;; size 3  4+ cycles
                    } }

{:opcode :LSR
 :mnemonic       "LSR"
 :help-text      "Logical Shift Right"

 :flags          "S Z C"

 :addressing-modes {0x4a :accumulator  ;; size 1  2 cycles
                    0x46 :zero-page    ;; size 2  5 cycles
                    0x56 :zero-page-x  ;; size 2  6 cycles
                    0x4e :absolute     ;; size 3  6 cycles
                    0x5e :absolute-x   ;; size 3  7 cycles
                    } }

{:opcode :NOP
 :mnemonic       "NOP"
 :help-text      "No OPeration"

 :flags          "none"

 :addressing-modes {0xea :implied  ;; size 1  2 cycles
                    } } 

{:opcode :ORA
 :mnemonic       "ORA"
 :help-text      "bitwise OR with Accumulator"

 :flags          "S Z"

 :addressing-modes {0x09 :immediate    ;; size 2  2 cycles
                    0x05 :zero-page    ;; size 2  3 cycles
                    0x15 :zero-page-x  ;; size 2  4 cycles
                    0x0d :absolute     ;; size 3  4 cycles
                    0x1d :absolute-x   ;; size 3  4+ cycles
                    0x19 :absolute-y   ;; size 3  4+ cycles
                    0x01 :indirect-x   ;; size 2  6 cycles
                    0x11 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :ROL
 :mnemonic       "ROL"
 :help-text      "ROtate Left"

 :flags          "S Z C"

 :addressing-modes {0x2a :accumulator  ;; size 1  2 cycles
                    0x26 :zero-page    ;; size 2  5 cycles
                    0x36 :zero-page-x  ;; size 2  6 cycles
                    0x2e :absolute     ;; size 3  6 cycles
                    0x3e :absolute-x   ;; size 3  7 cycles
                    } }

{:opcode :ROR
 :mnemonic       "ROR"
 :help-text      "ROtate Right"

 :flags          "S Z C"

 :addressing-modes {0x6a :accumulator  ;; size 1  2 cycles
                    0x66 :zero-page    ;; size 2  5 cycles
                    0x76 :zero-page-x  ;; size 2  6 cycles
                    0x6e :absolute     ;; size 3  6 cycles
                    0x7e :absolute-x   ;; size 3  7 cycles
                    } }

{:opcode :RTI
 :mnemonic       "RTI"
 :help-text      "ReTurn from Interrupt"

 :flags          "all"

 :addressing-modes {0x40 :implied  ;; size 1  6 cycles
                    } } 

{:opcode :RTS
 :mnemonic       "RTS"
 :help-text      "ReTurn from Subroutine"

 :flags          "none"

 :addressing-modes {0x60 :implied  ;; size 1  6 cycles
                    } }

{:opcode :SBC
 :mnemonic       "SBC"
 :help-text      "SuBtract with Carry"

 :flags          "S V Z C"

 :addressing-modes {0xe9 :immediate    ;; size 2  2 cycles
                    0xe5 :zero-page    ;; size 2  3 cycles
                    0xf5 :zero-page-x  ;; size 2  4 cycles
                    0xed :absolute     ;; size 3  4 cycles
                    0xfd :absolute-x   ;; size 3  4+ cycles
                    0xf9 :absolute-y   ;; size 3  4+ cycles
                    0xe1 :indirect-x   ;; size 2  6 cycles
                    0xf1 :indirect-y   ;; size 2  5+ cycles
                    } }

{:opcode :STA
 :mnemonic       "STA"
 :help-text      "STore Accumulator"

 :flags          "none"

 :addressing-modes {0x85 :zero-page    ;; size 2  3 cycles
                    0x95 :zero-page-x  ;; size 2  4 cycles
                    0x8d :absolute     ;; size 3  4 cycles
                    0x9d :absolute-x   ;; size 3  5 cycles
                    0x99 :absolute-y   ;; size 3  5 cycles
                    0x81 :indirect-x   ;; size 2  6 cycles
                    0x91 :indirect-y   ;; size 2  6 cycles
                    } }

{:opcode :STX
 :mnemonic       "STX"
 :help-text      "STore X register"

 :flags          "none"

 :addressing-modes {0x86 :zero-page    ;; size 2  3 cycles
                    0x96 :zero-page-y  ;; size 2  4 cycles
                    0x8e :absolute     ;; size 3  4 cycles
                    } }

{:opcode :STY
 :mnemonic       "STY"
 :help-text      "STore Y register"

 :flags          "none"

 :addressing-modes {0x84 :zero-page    ;; size 2  3 cycles
                    0x94 :zero-page-x  ;; size 2  4 cycles
                    0x8c :absolute     ;; size 3  4 cycles
                    } }
