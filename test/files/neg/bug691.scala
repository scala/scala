trait Base {
  trait AssignArrow {
    type T <: Ti0;
    trait Ti0;
  }
  abstract class Arrow extends AssignArrow;
  val arrow : Arrow;
}

trait Ext0 extends Base {
  trait AssignArrow extends super.AssignArrow {
    type T <: Ti1;
    trait Ti1 extends super.Ti0;
  }
}
trait Ext1 extends Base {
  trait Arrow extends super.Arrow {
    type T <: Ti2;
    trait Ti2 extends super.Ti0;
    trait TiXX extends Ti2;
  }
  val arrow : Arrow;
}
trait Composition extends Ext0 with Ext1 {
  object arrow0 extends Arrow with AssignArrow {
    type T = TiC
    trait TiC extends super[Arrow].Ti2 with super[AssignArrow].Ti1;
  }
}
