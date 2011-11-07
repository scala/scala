class A {    
  val n = {
        val z = {
            lazy val bb = 1
            bb
        }
        val a = {
            lazy val cc = 2
            cc
        }
        lazy val b = {
            lazy val dd = 3
            dd
        }
        z
    }
}

class B {
  locally {
    lazy val ms = "as"
    ms
  }
}

class C {
  val things = List("things")
  if(things.size < 100) {
    lazy val msg = "foo"
    msg
  }
}

class D {
  val things = List("things")
  if(things.size < 100) {
    if (things.size > 10) {
      lazy val msg = "foo"
      msg
    }
  }
}

