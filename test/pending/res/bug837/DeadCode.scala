package test;
trait DeadcodeAnalysis {
  object liveness extends Liveness;  
  val a = new liveness.LivenessAnalysis();
  var live = a.out("hello");
}
