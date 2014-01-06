import language.existentials

object Test {
  class Row

  abstract class MyRelation [R <: Row, +Relation <: MyRelation[R, Relation]]

  type M = MyRelation[R, Relation] forSome {type R <: Row; type Relation <: MyRelation[R, Relation]}

  var (x,y): (String, M) = null
}
