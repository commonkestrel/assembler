/* Registers */

pub macro load {
    (%reg:reg, %other:reg) => {
        LD %reg, %other
    }
    (%reg:reg, %other:imm) => {
        LDI %reg, %other
    }
}
