package spectest {
  class Sp[@specialized A, B](val a: A, val b: B) { }
  class Fsp[@specialized A, B](a: A, b: B) extends Sp(a,b) { def ab = (a,b) }
}
