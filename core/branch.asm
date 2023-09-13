pub macro brge {
    (%label:label) {
        ld Z, SREG
        and Z, (1 << 4 | 1 << 6)
        jnz %label
    }

    (%label, %a:reg, %b:reg|imm) {
        cmp %a, %b
        brge %label
    }
}

pub macro brle {
    (%label:label) {
        ld Z, SREG
        and Z, (1 << 4 | 1 << 5)
        jnz %label
    }

    (%label, %a:reg, %b:reg|imm) {
        cmp %a, %b
        brle %label
    }
}

pub macro brgt {
    (%label:label) {
        ld Z, SREG
        and Z, (1 << 6)
        jnz %label
    }

    (%label, %a:reg, %b:reg|imm) {
        cmp %a, %b
        brgt %label
    }
}

pub macro brlt {
    (%label:label) {
        ld Z, SREG
        and Z, (1 << 5)
        jnz %label
    }

    (%label, %a:reg, %b:reg|imm) {
        cmp %a, %b
        brlt %label
    }
}

pub macro breq {
    (%label:label) {
        ld Z, SREG
        and Z, (1 << 4)
        jnz %label
    }

    (%label, %a:reg, %b:reg|imm) {
        cmp %a, %b
        breq %label
    }
}