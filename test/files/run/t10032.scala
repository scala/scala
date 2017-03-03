object Test extends App {
  def a1(): Unit = println(" a1")
  def a2(): Unit = println(" a2")
  def a3(): Unit = println(" a3")

  def i1: Int = { println(" i1"); 1 }
  def i2: Int = { println(" i2"); 2 }
  def i3: Int = { println(" i3"); 3 }

  def e1: Int = { println(" e1"); throw new Exception() }

  def t1: Int = {
    println("t1")
    try {
      synchronized { return i1 }
    } finally {
      synchronized { a1() }
    }
  }

  def t2: Int = {
    println("t2")
    try {
      try { return i1 }
      finally { a1() }
    } finally {
      try { a2() }
      finally { a3() }
    }
  }

  def t3(i: => Int): Int = {
    println("t3")
    try {
      try { return i }
      finally { a1() }
    } catch {
      case _: Throwable =>
        try { i2 }
        finally { a2() } // no cleanup version
    } finally {
      a3()
    }
  }

  def t4(i: => Int): Int = {
    println("t4")
    try {
      return i
    } finally {
      return i2
    }
  }

  def t5(i: => Int): Int = {
    println("t5")
    try {
      try {
        try { return i }
        finally { a1() }
      } catch {
        case _: Throwable => i2
      }
    } finally {
      a3()
    }
  }

  def t6(i: => Int): Int = {
    println("t6")
    try {
      try { return i }
      finally { return i2 }
    } finally {
      return i3
    }
  }

  def t7(i: => Int): Int = {
    println("t7")
    try { i }
    catch {
      case _: Throwable =>
        return i2
    } finally {
      a1() // cleanup required, early return in handler
    }
  }

  def t8(i: => Int): Int = {
    println("t8")
    try {
      try { i }
      finally {           // no cleanup version
        try { return i2 }
        finally { a1() }  // cleanup version required
      }
    } finally {           // cleanup version required
      a2()
    }
  }

  def t9(i: => Int): Int = {
    println("t9")
    try {
      return i
    } finally {
      try { return i2 }
      finally { a1() }
    }
  }

  def t10(i: => Int): Int = {
    println("t10")
    try {
      return i
    } finally {
      try { return i2 }
      finally { return i3 }
    }
  }

  // this changed semantics between 2.12.0 and 2.12.1, see https://github.com/scala/scala/pull/5509#issuecomment-259291609
  def t11(i: => Int): Int = {
    println("t11")
    try {
      try { return i }
      finally { return i2 }
    } finally {
      a1()
    }
  }

  assert(t1 == 1)

  assert(t2 == 1)

  assert(t3(i1) == 1)
  assert(t3(e1) == 2)

  assert(t4(i1) == 2)
  assert(t4(e1) == 2)

  assert(t5(i1) == 1)
  assert(t5(e1) == 2)

  assert(t6(i1) == 3)
  assert(t6(e1) == 3)

  assert(t7(i1) == 1)
  assert(t7(e1) == 2)

  assert(t8(i1) == 2)
  assert(t8(e1) == 2)

  assert(t9(i1) == 2)
  assert(t9(e1) == 2)

  assert(t10(i1) == 3)
  assert(t10(e1) == 3)

  assert(t11(i1) == 2)
  assert(t11(e1) == 2)
}
