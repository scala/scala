trait Crash {

    def foo(i: => Int) (j: Int): Int

    def t = {
        // var count = 0
        foo {
            var count = 0
            count
        } _
    }

}
