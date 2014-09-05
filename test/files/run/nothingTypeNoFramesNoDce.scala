// See comment in BCodeBodyBuilder

// -target:jvm-1.5 -Ybackend:GenBCode -Yopt:l:none
// target disables stack map frame generation. in this mode, the ClssWriter just emits dead code as is.

class C {
  // can't just emit a call to ???, that returns value of type Nothing$ (not Int).
  def f1: Int = ???

  def f2: Int = throw new Error("")

  def f3(x: Boolean) = {
    var y = 0
    // cannot assign an object of type Nothing$ to Int
    if (x) y = ???
    else   y = 1
    y
  }

  def f4(x: Boolean) = {
    var y = 0
    // tests that whatever is emitted after the throw is valid (what? depends on opts, presence of stack map frames)
    if (x) y = throw new Error("")
    else   y = 1
    y
  }

  def f5(x: Boolean) = {
    // stack heights need to be the smae. ??? looks to the jvm like returning a value of
    // type Nothing$, need to drop or throw it.
    println(
      if (x) { ???; 10 }
      else 20
    )
  }

  def f6(x: Boolean) = {
    println(
      if (x) { throw new Error(""); 10 }
      else 20
    )
  }

  def f7(x: Boolean) = {
    println(
      if (x) throw new Error("")
      else 20
    )
  }

  def f8(x: Boolean) = {
    println(
      if (x) throw new Error("")
      else 20
    )
  }
}
