//Class hierarchy
trait Term[a]
object Var{ def unapply[a](x:Var[a]) = Some(x.name) }
class Var[a] (val name : String) extends Term[a]
object Num{ def unapply(x:Num) = Some(x.value) }
class Num (val value : int) extends Term[int]
object Lam{ def unapply[b,c](l:Lam[b,c]) = Some{l.x,l.e}}
class Lam[b, c] (val x : Var[b], val e : Term[c]) extends Term[b => c]
object App{ def unapply[b,c](a:App[b,c]) = Some{a.f,a.e}}
class App[b, c] (val f : Term[b => c], val e : Term[b]) extends Term[c]
object Suc{ def unapply(a:Suc) = true }
class Suc () extends Term[int => int]
// Environments :
abstract class Env { 
  def apply[a](v : Var[a]): a
  def extend[a](v : Var[a], x : a) = new Env { 
    def apply[b](w: Var[b]): b = w match { 
      case _ : v.type => x // v eq w, hence a = b
      case _ => Env.this.apply(w)
    }}
}

object empty extends Env { 
  def apply[a](x : Var[a]): a = throw new Error("not found : "+x.name) 
}
object Test {
// Evaluation :
def eval[a](t : Term[a], env : Env): a = t match { 
  case v : Var[b]        => env(v)                     // a = b
  case n @ Num(value)    => value                      // a = int
  case i @ Suc()         => { y: int => y + 1 }       // a = int=>int
  case f @ Lam[b,c](x,e) => { y: b   => eval(e, env.extend(x, y))}  // a = b=>c
  case a @ App(f,e)      => eval(f, env)(eval(e, env)) // a = c
}
}
