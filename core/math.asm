macro add16 {
    (%a0:reg, %a1:reg, %b0:reg|imm, %b1:reg|imm) {
        add %a0, %b0
        adc %a1, %b1
    }

    (%a0:reg, %a1:reg, %b:imm16) {
        add %a0, ((%b >> 0) & 0xFF)
        adc %a1, ((%b >> 8) & 0xFF)
    }
}

macro inc {
    (%reg:reg) {
        add %reg, 1
    }
}

macro dec {
    (%reg:reg) {
        sub %reg, 1
    }
}

/// Increment if carry.
macro iic {
    (%reg:reg) {
        adc %reg, 0
    }
}

// I used borrow here because I
// don’t want this to be `dic`.
/// Increment if borrow.
macro dib {
    (%reg:reg) {
        sbc %reg, 0
    }
}