class Foo extends Dynamic {
  // need to distinguish selectDynamic and applyDynamic somehow: the former must return the selected value, the latter must accept an apply or an update
  //  - could have only selectDynamic and pass it a boolean whether more is to come,
  //    so that it can either return the bare value or something that can handle the apply/update
  //      HOWEVER that makes it hard to return unrelated values for the two cases
  //      --> selectDynamic's return type is now dependent on the boolean flag whether more is to come
  //  - simplest solution: have two method calls
  // what if we want foo.field == foo.selectDynamic("field") == 1
  def selectDynamic(name: String) = {println(Foo.this  + "." + name); new Selector(name)}
  // called if there's more to come after foo.applyDynamic("bar") -- apply or update
  def applyDynamic(name: String) = new Selector(name)
  def updateDynamic(name: String)(rhs: Any) = println(Foo.this + "." +  name  +" = "+ rhs)

  class Selector(name: String) {
    def apply(args: Any*) = println(Foo.this  + "." + name + args.mkString("(", ", ", ")"))
    def update(args: Any*) = println(Foo.this + "." +  name + (if(args.tail isEmpty) "" else args.init.mkString("(", ", ", ")")) +" = "+ args.last)
  }
}

class Curried extends Dynamic {
  def applyDynamic(name: String)(args: Any*) = println(this + "." + name + args.mkString("(", ", ", ")"))
}

object Test extends App {
  val foo = new Foo { override def toString = "foo" }
  val curried = new Curried { override def toString = "curried" }

// if FUNmode
  foo.method("blah")        // foo.applyDynamic("method").apply("blah")
  curried.method("blah")        // curried.applyDynamic("method")("blah")

// else if BYVALmode
  foo.field                 // foo.selectDynamic("field")

// else if LHSmode
  foo.varia = 10            // foo.updateDynamic("varia")(10)

// else if QUALmode
  foo.arr(10) = 13          // foo.selectDynamic("arr").update(10, 13)
  foo.arr("a", 20) = "new"  // foo.selectDynamic("arr").update("a", 20, "new")
}
