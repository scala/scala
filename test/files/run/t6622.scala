import Test.check

object O1 {
  lazy val resultVal = {
    class A
    check("O1.resultVal", classOf[A])
  }

  def resultDef = {
    class A
    check("O1.resultDef", classOf[A])
  }
}

class C2 {
  val resultVal = {
    val tmp = {
      class B
      check("C2.resultVal", classOf[B])
    }
  }
}

object O3 {
  def resultDef = {
    class C
    check("O3.resultDef", classOf[C])
  }
}

object O4 {
  def resultDefDefault(a: Any = {
    class C
    check("O4.resultDefDefault", classOf[C])
  }) = ();
}


object Test extends App {
  def check(desc: String, clazz: Class[_]) {
    println(s" $desc isMemberClass = ${clazz.isMemberClass}, ${clazz.getEnclosingMethod}")
    println(reflect.runtime.currentMirror.classSymbol(clazz))
  }

  O1.resultVal
  O1.resultDef
  new C2().resultVal
  O3.resultDef
  O4.resultDefDefault()
}
