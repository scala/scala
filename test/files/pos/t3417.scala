trait X extends NotNull {
    def foo = 1
}

trait Y extends Object with NotNull {
    def bar = 1
}

class Z extends NotNull

class W extends Object with NotNull
