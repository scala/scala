abstract class C() {

  type t <: C;

  val x: t;
  val y: x.type;
  val z: x.type;

  val xt: x.t;
  val yt: y.t;
  val zt: z.t;

  def fx(a: x.t): Unit;
  def fy(a: y.t): Unit;
  def fz(a: z.t): Unit;

  fx(xt); fx(yt); fx(zt);
  fy(xt); fy(yt); fy(zt);
  fz(xt); fz(yt); fz(zt);

}
