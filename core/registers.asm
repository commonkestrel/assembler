macro pusha {
    () {
        push A
        push B
        push C
        push D
        push Z
    }
}

macro popa {
    () {
        pop Z
        pop D
        pop C
        pop B
        pop A
    }
}
