//> using options -Werror
package p {
  package _root_ {
    package scala {
      object Option
    }
  }
}
package p {
  object Test {
    import _root_.scala.Option
    def f = Option(null)
  }
}

// was:
// test/files/neg/t6217.scala:12: error: p._root_.scala.Option.type does not take parameters
/*
t6217.scala:11: warning: ignoring relative package named _root_ in root position
    import _root_.scala.Option
               ^
 */
