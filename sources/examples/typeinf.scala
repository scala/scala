package examples;

trait Term {}
case class Var(x: String)                   extends Term {}
case class Lam(x: String, e: Term)          extends Term {}
case class App(f: Term, e: Term)            extends Term {}
case class Let(x: String, e: Term, f: Term) extends Term {}

module types {
  trait Type {}
  case class Tyvar(a: String) extends Type {}
  case class Arrow(t1: Type, t2: Type) extends Type {}
  case class Tycon(k: String, ts: List[Type]) extends Type {}
  private var n: Int = 0;
  def newTyvar: Type = { n = n + 1 ; Tyvar("a" + n) }
}
import types._;

case class ListSet[a](elems: List[a]) {

  def contains(y: a): Boolean = elems match {
    case List() => False
    case x :: xs => (x == y) || (xs contains y)
  }

  def union(ys: ListSet[a]): ListSet[a] = elems match {
    case List() => ys
    case x :: xs =>
      if (ys contains x) ListSet(xs) union ys
      else ListSet(x :: (ListSet(xs) union ys).elems)
  }

  def diff(ys: ListSet[a]): ListSet[a] = elems match {
    case List() => ListSet(List())
    case x :: xs =>
      if (ys contains x) ListSet(xs) diff ys
      else ListSet(x :: (ListSet(xs) diff ys).elems)
  }
}

module typeInfer {

  trait Subst with Function1[Type,Type] {
    def lookup(x: Tyvar): Type;
    def apply(t: Type): Type = t match {
      case Tyvar(a) => val u = lookup(Tyvar(a)); if (t == u) t else apply(u);
      case Arrow(t1, t2) => Arrow(apply(t1), apply(t2))
      case Tycon(k, ts) => Tycon(k, ts map apply)
    }
    def extend(x: Tyvar, t: Type) = new Subst {
      def lookup(y: Tyvar): Type = if (x == y) t else Subst.this.lookup(y);
    }
  }

  val emptySubst = new Subst { def lookup(t: Tyvar): Type = t }

  def tyvars(t: Type): ListSet[String] = t match {
    case Tyvar(a) => new ListSet(List(a))
    case Arrow(t1, t2) => tyvars(t1) union tyvars(t2)
    case Tycon(k, ts) => tyvars(ts)
  }
  def tyvars(ts: TypeScheme): ListSet[String] = ts match {
    case TypeScheme(vs, t) => tyvars(t) diff new ListSet(vs)
  }
  def tyvars(ts: List[Type]): ListSet[String] = ts match {
    case List() => new ListSet[String](List())
    case t :: ts1 => tyvars(t) union tyvars(ts1)
  }
  def tyvars(env: Env): ListSet[String] = env match {
    case List() => new ListSet[String](List())
    case Pair(x, t) :: env1 => tyvars(t) union tyvars(env1)
  }

  case class TypeScheme(vs: List[String], t: Type) {
    def newInstance: Type =
      (emptySubst foldl_: vs) { (s, a) => s.extend(Tyvar(a), newTyvar) } (t);
  }

  type Env = List[Pair[String, TypeScheme]];

  def lookup(env: Env, x: String): TypeScheme = env match {
    case List() => null
    case Pair(y, t) :: env1 => if (x == y) t else lookup(env1, x)
  }

  def gen(env: Env, t: Type): TypeScheme =
    TypeScheme((tyvars(t) diff tyvars(env)).elems, t);

  def mgu(t: Type, u: Type)(s: Subst): Subst = Pair(s(t), s(u)) match {
    case Pair(Tyvar( a), Tyvar(b)) if (a == b) =>
      s
    case Pair(Tyvar(a), _) =>
      if (tyvars(u) contains a) error("unification failure: occurs check")
      else s.extend(Tyvar(a), u)
    case Pair(_, Tyvar(a)) =>
      mgu(u, t)(s)
    case Pair(Arrow(t1, t2), Arrow(u1, u2)) =>
      mgu(t1, u1)(mgu(t2, u2)(s))
    case Pair(Tycon(k1, ts), Tycon(k2, us)) if (k1 == k2) =>
      (s foldl_: ((ts zip us) map {case Pair(t,u) => mgu(t,u)})) { (s, f) => f(s) }
    case _ => error("unification failure");
  }

  def tp(env: Env, e: Term, t: Type)(s: Subst): Subst = e match {
    case Var(x) => {
      val u = lookup(env, x);
      if (u == null) error("undefined: x");
      else mgu(u.newInstance, t)(s)
    }
    case Lam(x, e1) => {
      val a = newTyvar, b = newTyvar;
      val s1 = mgu(t, Arrow(a, b))(s);
      val env1 = Pair(x, TypeScheme(List(), a)) :: env;
      tp(env1, e1, b)(s1)
    }
    case App(e1, e2) => {
      val a = newTyvar;
      val s1 = tp(env, e1, Arrow(a, t))(s);
      tp(env, e2, a)(s1)
    }
    case Let(x, e1, e2) => {
      val a = newTyvar;
      val s1 = tp(env, e1, a)(s);
      tp(Pair(x, gen(env, s1(a))) :: env, e2, t)(s1)
    }
  }

  def typeOf(env: Env, e: Term): Type = {
    val a = newTyvar;
    tp(env, e, a)(emptySubst)(a)
  }
}

module predefined {
  val booleanType = Tycon("Boolean", List());
  val intType = Tycon("Int", List());
  def listType(t: Type) = Tycon("List", List(t));

  private def gen(t: Type): typeInfer.TypeScheme = typeInfer.gen(List(), t);
  private val a = newTyvar;
  val env = List(
    Pair("true", gen(booleanType)),
    Pair("false", gen(booleanType)),
    Pair("if", gen(Arrow(booleanType, Arrow(a, Arrow(a, a))))),
    Pair("zero", gen(intType)),
    Pair("succ", gen(Arrow(intType, intType))),
    Pair("nil", gen(listType(a))),
    Pair("cons", gen(Arrow(a, Arrow(listType(a), listType(a))))),
    Pair("isEmpty", gen(Arrow(listType(a), booleanType))),
    Pair("head", gen(Arrow(listType(a), a))),
    Pair("tail", gen(Arrow(listType(a), listType(a)))),
    Pair("fix", gen(Arrow(Arrow(a, a), a)))
  )
}

module test {

  def showType(e: Term) = typeInfer.typeOf(predefined.env, e);

  showType(Lam("x", App(App(Var("cons"), Var("x")), Var("nil"))));

}
