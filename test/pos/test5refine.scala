import scala._;

module test {

  abstract trait F { type If; }

  def f[Jf](h: Jf):F with { type If = Jf } = f[Jf](h);

  abstract trait G { type Ig; }

  def g[Jg](h: Jg):G with { type Ig = Jg } = g[Jg](h);

  abstract class M() {
      type P;
      abstract class I() {
	  type X;

	  // Methods to check the type X and P as seen from instances of I
	  def chk_ix(x: X): Unit = {}
	  def chk_ip(p: P): Unit = {}

	  // Value with type X as seen from instances of I
	  def val_ix: X = val_ix;
      }

      val i: I with { type X = G with { type Ig = P } } = null;

      // Values with types P and i.X as seen from instances of M
      def val_mp: P = val_mp;
      def val_mix: G with { type Ig = P } = g[P](val_mp);
  }

  abstract class N() extends M() {
      type Q;
      type P = F with { type If = Q };
      val j:J with { type Y = G with { type Ig = Q } } = null;

      abstract class J() extends I() {
	  type Y;
	  type X = G with { type Ig = Y; };
	  // Values with types Y and X as seen from instances of J
	  def val_jy: Y = val_jy;
	  def val_jx: G with { type Ig = Y; } = g[Y](val_jy);

	  // Check type P
	  chk_ip(val_mp);
	  chk_ip(val_np);
      }

      // Values with types Q, X.P, i.X, j.Y and j.X as seen from instances of N
      def val_nq: Q = val_nq;
      def val_np: F with { type If = Q } = f[Q](val_nq);
      def val_nix: G with { type Ig = F with { type If = Q } } = g[F with { type If = Q }](val_np);
      def val_njy: G with { type Ig = Q; } = g[Q](val_nq);
      def val_njx: G with { type Ig = G with { type Ig = Q }} = g[G with { type Ig = Q; }](val_njy);

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