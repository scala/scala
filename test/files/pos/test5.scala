import scala._;

object test {

  trait F[If] {}

  def f[Jf](h: Jf):F[Jf] = f[Jf](h);

  trait G[Ig] {}

  def g[Jg](h: Jg):G[Jg] = g[Jg](h);

  class M[P]() {
      abstract class I[X]() {
	  // Methods to check the type X and P as seen from instances of I
	  def chk_ix(x: X): Unit = ();
	  def chk_ip(p: P): Unit;

	  // Value with type X as seen from instances of I
	  def val_ix: X = val_ix;
      }

      val i:I[G[P]] = null;

      // Values with types P and i.X as seen from instances of M
      def val_mp: P = val_mp;
      def val_mix: G[P] = g[P](val_mp);
  }

  class N[Q]() extends M[F[Q]]() {
      val j:J[G[Q]] = null;

      abstract class J[Y]() extends I[G[Y]]() {
	  // Values with types Y and X as seen from instances of J
	  def val_jy: Y = val_jy;
	  def val_jx: G[Y] = g[Y](val_jy);

	  // Check type P
	  chk_ip(val_mp);
	  chk_ip(val_np);
      }

      // Values with types Q, X.P, i.X, j.Y and j.X as seen from instances of N
      def val_nq: Q = val_nq;
      def val_np: F[Q] = f[Q](val_nq);
      def val_nix: G[F[Q]] = g[F[Q]](val_np);
      def val_njy: G[Q] = g[Q](val_nq);
      def val_njx: G[G[Q]] = g[G[Q]](val_njy);

      // Check type i.P
      i.chk_ip(val_mp);
      i.chk_ip(val_np);

      // Check type j.P
      j.chk_ip(val_mp);
      j.chk_ip(val_np); 

      // Check type i.X
      i.chk_ix(i.val_ix);
      i.chk_ix(val_mix);
      i.chk_ix(val_nix);

      // Check j.X
      j.chk_ix(j.val_ix);
      j.chk_ix(j.val_jx);
      j.chk_ix(val_njx); 
  }
}
