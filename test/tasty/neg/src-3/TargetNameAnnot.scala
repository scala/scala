package tastytest

object TargetNameAnnot {

  @annotation.targetName("doubleplus") def ++ : Unit = println("++")

  def foo = 23

}
