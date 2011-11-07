/** Cleaned up in october 2010 by paulp.
 *  Hey, we should get this working.
 */

// Class hierarchy
trait Term[a]

object Var{ def unapply[a](x:Var[a]) = Some(x.name) }
class Var[a] (val name : String) extends Term[a]

object Num{ def unapply(x:Num) = Some(x.value) }
class Num (val value : Int) extends Term[Int]

object Lam{ def unapply[b,c](l: Lam[b,c]) = Some(l.x, l.e) }
class Lam[b, c](val x : Var[b], val e : Term[c]) extends Term[b => c]

object App{ def unapply[b,c](a: App[b,c]) = Some(a.f, a.e) }
class App[b, c] (val f : Term[b => c], val e : Term[b]) extends Term[c]

object Suc { def unapply(a: Suc) = true }
class Suc() extends Term[Int => Int]

// Environments :
abstract class Env { 
  def apply[a](v: Var[a]): a
  def extend[a](v: Var[a], x : a) = new Env {
    def apply[b](w: Var[b]): b = w match { 
      case _ : v.type => x // v eq w, hence a = b
      case _ => Env.this.apply(w)
    }}
}

object empty extends Env { 
  def apply[a](x: Var[a]): a = throw new Error("not found : "+x.name) 
}

object Test {
  val v1 = new Var[util.Random]("random")
  val v2 = new Var[Int]("Int")
  val v3 = new Var[List[String]]("list")
  
  val anEnv = (empty
    .extend(v1, new util.Random)
    .extend(v2, 58)
    .extend(v3, Nil)
  )
  
  def eval[a](t: Term[a], env : Env): a = t match {
    // First three work
    case v : Var[b]         => env(v)                     // a = b
    case n @ Num(value)     => value                      // a = Int
    case a @ App(f,e)       => eval(f, env)(eval(e, env)) // a = c

    // Next one fails like:
    //
    // found   : (Int) => Int
    // required: a    
    case i @ Suc()          => { (y: Int) => y + 1 }      // a = Int => Int
    
    // Next one fails like:
    //
    // error: '=>' expected but '[' found.
    //     case f @ Lam[b,c](x, e) => { (y: b) => eval(e, env.extend(x, y)) }  // a = b=>c
    //                 ^
    case f @ Lam[b,c](x, e) => { (y: b) => eval(e, env.extend(x, y)) }  // a = b=>c
  }
  
  val f1 = () => eval(v1, anEnv)
  val f2 = () => eval(v2, anEnv)
  val f3 = () => eval(v3, anEnv)
  
  def main(args: Array[String]): Unit = {
    println(f1())
    println(f2())
    println(f3())
  }
}
