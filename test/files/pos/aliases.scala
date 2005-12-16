abstract class C() {

  type t <: C;

  val x: t;
  val y: x.type;
  val z: x.type;
  val u: z.type;

  val xt: x.t;
  val yt: y.t;
  val zt: z.t;
  val ut: z.t;

  def fx(a: x.t): Unit;
  def fy(a: y.t): Unit;
  def fz(a: z.t): Unit;
  def fu(a: u.t): Unit;

  fx(xt); fx(yt); fx(zt); fx(ut);
  fy(xt); fy(yt); fy(zt); fy(ut);
  fz(xt); fz(yt); fz(zt); fz(ut);
  fu(xt); fu(yt); fu(zt); fu(ut);

}
