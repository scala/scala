// /scala/trac/5777/a.scala
// Wed May  9 08:44:57 PDT 2012

trait Ring {
  trait E
}

class Poly[C <: Ring](val ring: C) extends Ring
// This definition of Poly triggers the same failure on *both* versions
// class Poly(val ring: Ring) extends Ring

object BigInt extends Ring

object MyApp {
  val r = new Poly(BigInt)

  implicitly[r.ring.E <:< BigInt.E]

  // fail on 2.10, works on 2.9.2
  (null.asInstanceOf[BigInt.E] : r.ring.E)

  // works on both versions
  val r1 = new Poly[BigInt.type](BigInt)
  (null.asInstanceOf[BigInt.E] : r1.ring.E)

  // Oddly, -Xprint:typer reports that r and r1 have the same inferred type.
  //
  // private[this] val r: Poly[BigInt.type] = new Poly[BigInt.type](BigInt);
  // <stable> <accessor> def r: Poly[BigInt.type] = MyApp.this.r;
  // (null.asInstanceOf[BigInt.E]: MyApp.r.ring.E);
  // private[this] val r1: Poly[BigInt.type] = new Poly[BigInt.type](BigInt);
  // <stable> <accessor> def r1: Poly[BigInt.type] = MyApp.this.r1;
  // (null.asInstanceOf[BigInt.E]: MyApp.r1.ring.E)

  // diff typer-2.9.2.txt typer-2.10.txt
  // ...
  // ---
  // >   object MyApp extends scala.AnyRef {
  // >     def <init>(): MyApp.type = {
  // >       MyApp.super.<init>();
  // 30c30
  // <     scala.this.Predef.implicitly[<:<[BigInt.E,MyApp.r.ring.E]](scala.this.Predef.conforms[BigInt.E]);
  // ---
  // >     scala.this.Predef.implicitly[<:<[BigInt.E,MyApp.r.ring.E]]();
}
