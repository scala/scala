// This used to compile under 2.10.3 but the ambiguity is now noticed
// in 2.11.x (after a70c8219). I think the new behaviour is correct;
// we shouldn't discard names based on "expected stability" before
// evaluating ambiguity.
object ColumnOption {
  object PrimaryKey
}

class A {
  def PrimaryKey: Any = ???

  {
    import ColumnOption._

    (null: Any) match { case PrimaryKey => }

    PrimaryKey // was already ambiguous in 2.10.3
  }
}

