

object Test extends App {
  JavaTest.main(null)

  var a1 : SomeClass = new SomeClass
  var a2 : SomeClass2 = new SomeClass2
  //import language.implicitConversions
  //implicit def setParentType2SomeClass(x:Any) = x.asInstanceOf[SomeClass]
  //implicit def setParentType2SomeClass2(x:Any) = x.asInstanceOf[SomeClass2]
  //var b : SomeClass = a.f.set(23).asInstanceOf[SomeClass].f.set(23).asInstanceOf[SomeClass]
  //var b2 : SomeClass2 = a2.f.set(23).asInstanceOf[SomeClass2].f.set(23).asInstanceOf[SomeClass2]
  var b1 : SomeClass =  a1.f.set(23).f.set(23)
  var b2 : SomeClass2 = a2.f.set(23).f.set(23)
}
