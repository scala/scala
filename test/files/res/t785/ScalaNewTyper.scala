package t785;
trait ScalaNewTyper {
  private var typed : String = null;
  trait HasSymbol {
    protected def foo() : Unit = {}
  }
  trait HasArgsTypeParametersImpl extends HasSymbol {
    private var argss : List[List[String]] = Nil;
  }
}
