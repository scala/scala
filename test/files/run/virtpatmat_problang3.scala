trait ProbIntf {
  type Prob = Double
  type Rand[+A] <: RandImpl[A]
  trait RandImpl[+A] {
    var name = super.toString
    override def toString = name
    def dbg(n:String): this.type = { name = n; this }
    def flatMap[B](f: A => Rand[B]): Rand[B]
    def map[B](f: A => B): Rand[B] = flatMap(x => always(f(x)))
    def orElse[B >: A](that: Rand[B]): Rand[B]
  }
  def always[A](x: A) = choice(x -> 1.0)
  def never = choice()
  def flip(p: Double): Rand[Boolean] = choice(true -> p, false -> (1-p))
  def uniform[A](xs: A*): Rand[A] = choice(xs.map((_,1.0)):_*)
  def choice[A](xs: (A,Prob)*): Rand[A]
  def collapse[A](r: Rand[A]): List[(A,Prob)]
  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)]  
}

trait ProbCore extends ProbIntf {
  type Rand[+A] = RandVar[A]
  case class Choice[+A](rv: Int, v: A, p: Prob)
  type Path = List[Choice[Any]]
  type Dist[+A] = List[Path]

  case class RandVar[+A](dist: Dist[A]) extends RandImpl[A] {
    def flatMap[B](f: A => Rand[B]): Rand[B] =
      RandVar(dist.flatMap(path => f(path.last.v.asInstanceOf[A]).dist.map(post => path ++ post)))
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVar(dist ++ that.dist)
  }

  def factor[A](w: Prob, xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.map{case (x,p) => (x,p*w)}
  }
  def consolidate[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)}
  }
  def normalize[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    val weight = xs.map(_._2).sum
    factor(1/weight,xs) // 1/0 ?
  }

  var numChoices = 0
  def freshChoiceId() = { numChoices += 1; numChoices - 1 }

  def choice[A](xs: (A,Prob)*): Rand[A] = {
    val id = freshChoiceId()
    RandVar[A](xs.toList.map{case(x,p) => List(Choice(id,x,p))})
  }

  def collapse[A](r: Rand[A]): List[(A,Prob)] = {
    def prob(path: Path, env: Map[Int,Any] = Map.empty): Prob = path match {
      case Choice(r,x,p)::rest =>
        env.get(r) match {
          case Some(`x`) => prob(rest,env)
          case None => p * prob(rest,env + (r -> x))
          case _ => 0
        }
      case _ => 1.0
    }
    normalize(consolidate(r.dist.map(path => (path.last.v, prob(path))))).asInstanceOf[List[(A,Prob)]]
  }

  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)] = ???
}


trait ProbCoreLazy extends ProbIntf {
  type Rand[+A] = RandVar[A]
  abstract class RandVar[+A] extends RandImpl[A] { self =>
    def flatMap[B](f: A => Rand[B]): Rand[B] =
      RandVarFlatMap(this, f)
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVarOrElse(this, that)
  }

  case class RandVarChoice[+A](id: Int, dist: List[(A,Prob)]) extends RandVar[A]
  case class RandVarFlatMap[A,+B](x: RandVar[A], f: A => Rand[B]) extends RandVar[B]
  case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A]) extends RandVar[A]



  def factor[A](w: Prob, xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.map{case (x,p) => (x,p*w)}
  }
  def consolidate[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)}
  }
  def normalize[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    val weight = xs.map(_._2).sum
    factor(1/weight,xs) // 1/0 ?
  }

  var numChoices: Int = _ //0 <--- BEWARE OF INIT ORDER!!!
  def freshChoiceId() = { numChoices += 1; numChoices - 1 }

  def choice[A](xs: (A,Prob)*): Rand[A] = {
    val id = freshChoiceId()
    new RandVarChoice[A](id, xs.toList)
  }

  def collapse[A](r: Rand[A]): List[(A,Prob)] = {
    type R = List[(A,Prob)]
    def prob[B](path: RandVar[B], p: Prob, env: Map[Int,Any] = Map.empty)(next: (B,Prob,Map[Int,Any]) => R): R = path match {
      case RandVarChoice(id,dist) =>
        env.get(id) match {
          case Some(x) =>
            assert(dist exists (_._1 == x), x+" not in "+dist+" for "+id)
            next(x.asInstanceOf[B],p,env)
          case None => 
            dist flatMap { case (x,q) =>
              next(x, p*q,env + (id -> x))
            }
        }
      case RandVarFlatMap(x,f) =>
        prob(x,p,env) { (y,q,e) => prob(f(y),q,e)(next) }
      case RandVarOrElse(x,y) =>
        prob(x,p,env)(next) ++ prob(y,p,env)(next)
    }
    normalize(consolidate(prob(r,1)((x,p,e)=>List(x->p))))
  }

  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)] = {
    println("searching for min "+solutions+" solutions")
    type R = List[(A,Prob)]
    var more = true
    type Mem = List[(Int, AnyRef, Any, Any)] // memo table: (id hash, function, arg, res)
    type Env = Map[Int,Any]

    def prob[B](path: RandVar[B], p: Prob, mem: Mem, env: Env, budget: Int)(next: (B,Prob,Mem,Env,Int) => R): R = 
    if (budget < 0) { more = true; Nil } else path match {
      case RandVarChoice(id,dist) =>
        env.get(id) match {
          case Some(x) =>
            assert(dist exists (_._1 == x), x+" not in "+dist+" for "+id)
            next(x.asInstanceOf[B],p,mem,env,budget)
          case None => 
            val budget1 = if (dist.lengthCompare(1) <= 0) budget else budget-1 // certain choice doesn't count for depth
            dist flatMap { case (x,q) =>
              next(x, p*q, mem, env + (id -> x), budget1)
            }
        }
      case RandVarFlatMap(x,f) =>
        prob(x,p,mem,env,budget) { (y,q,m,e,k) => 
          // we memoize (continuation,value) pairs.
          // thankfully, closures have identity in Scala.
          // we could also attach ids to flatMap objects?
          m.find(el => (el._2 eq f) && (el._3 == y)) match {
            case Some((id,el,arg,res)) => prob(res.asInstanceOf[RandVar[B]],q,m,e,k)(next)
            case None => val res = f(y); prob(res,q,(System.identityHashCode(f),f,y,res)::m,e,k)(next) 
          }
        }
      case RandVarOrElse(x,y) =>
        prob(x,p,mem,env,budget)(next) ++ prob(y,p,mem,env,budget)(next)
    }

    var res: R = Nil
    var depth = 1
    while (res.length < solutions && more) {
      println("trying depth "+depth)
      more = false
      res = prob(r,1,Nil,Map.empty,depth)((x,p,m,e,k)=>List(x->p))
      depth += 1
    }
    //println(ids.sorted.mkString("\n"))
    // todo: don't throw away all solutions each time, print them as 
    // they are discovered (solutions=5 will never give an answer if 
    // there are only 3)
    normalize(consolidate(res))
  }

}




trait ProbPrettyPrint extends ProbIntf {
  def pp[A](r: Rand[A], strategy: String, solutions: Int) = 
    (if (solutions > 0) collapse2(r,strategy,solutions) else collapse(r)).map{case (x,p) => x + " : " + p}.mkString("\n")
  def show[A](r: Rand[A], desc: String = "", strategy: String = "", solutions: Int = -1) = {
    println(desc)
    println(pp(r,strategy,solutions))
    println("")
  }
}

trait ProbLang extends EmbeddedControls with ProbIntf {

  def liftOp2[A,B,C](x: Rand[A], y: Rand[B])(f: (A,B) => C): Rand[C] = for (a <- x; b <- y) yield f(a,b)

  def infix_&&(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] = liftOp2(x,y)(_ && _) // short circuit ??
  def infix_===[A](x: Rand[A], y: Rand[A]): Rand[Boolean] =         liftOp2(x,y)(_ == _)
  def infix_+(x: Rand[Int], y: Rand[Int]): Rand[Int] =              liftOp2(x,y)(_ + _)

}



trait ProbRuleLang extends ProbLang {
  object __match {
    def one[T](x: T): Rand[T] = always(x)
    def zero = never
    def guard[T](cond: Boolean, result: => T): Rand[T] =
      if (cond) one(result) else zero
    def runOrElse[T, U](in: T)(matcher: T => Rand[U]): Rand[U] =
      matcher(in)
  }

  implicit class Rule[A,B](f: A => Rand[B]) {
    def unapply(x: A): Rand[B] = f(x)
  }

  implicit class SRule(f: String => Rand[String]) {
    def unapply(x: String): Rand[String] = f(x)
  }


  def rule[A,B](f: A => Rand[B]) = new Rule[A,B](f)

  def infix_rule[A, B](f: A => Rand[B]): Rule[A,B] = new Rule(f)

  val && = ((x: Any) => x match {
    case x => (x,x)
  }) rule

  val Likes: SRule = { x: String => x match {
    case "A" => "Coffee"
    case "B" => "Coffee"
    case "D" => "Coffee"
    case "D" => "Coffee" // likes coffee very much!
    case "E" => "Coffee"
  }}

  val Friend: SRule = { x: String => x match {
    case "A" => "C"
    case "A" => "C" // are really good friends!
    case "C" => "D"
    case "B" => "D"
    case "A" => "E"
  }}

  val Knows: SRule = { x: String => x match {
    case Friend(Knows(y)) => y
    case x => x
  }}

  val ShouldGrabCoffee: SRule = { x: String => x match {
    case Likes("Coffee") && Knows(y @ Likes("Coffee")) if x != y =>
      x + " and " + y + " should grab coffee"
  }}


  val coffeeModel1: Rand[String] = uniform("A","B","C","D","E").flatMap({ case ShouldGrabCoffee(y) => always(y) }).flatMap(x=>x)

}



trait ProbLangExTraffic extends ProbLang {
  val lightsIt = Iterator("Red", "Yellow", "Green")
  val actionIt = Iterator("Stop", "Drive")
  val resultIt = Iterator("Crash", "NoCrash")

  val Red, Yellow, Green = lightsIt.next
  val Stop, Drive        = actionIt.next
  val Crash, NoCrash     = resultIt.next

  type Light = String
  type Action = String
  type Driver = Rand[Light] => Rand[Action]


  val trafficLight = choice(Red -> 0.5, Yellow -> 0.1, Green -> 0.4) dbg "light"

  def otherLight(light: Rand[Light]) = light map {
    case Red => Green
    case Yellow => Red
    case Green => Red
  } dbg "otherLight"
  def cautiousDriver(light: Rand[Light]) = light flatMap {
    case Red => always(Stop)
    case Yellow => choice(Stop -> 0.9, Drive -> 0.1)
    case Green => always(Drive)
  } dbg "cautiousDriver"
  def aggressiveDriver(light: Rand[Light]) = light flatMap {
    case Red => choice(Stop -> 0.9, Drive -> 0.1)
    case Yellow => choice(Stop -> 0.1, Drive -> 0.9)
    case Green => always(Drive)
  } dbg "aggressiveDriver"


  def crash(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    light flatMap { l =>
      val light = always(l)

      val d1 = driver1(light)
      val d2 = driver2(otherLight(light))
      (driver1(light) === always(Drive) && (driver2(otherLight(light)) === always(Drive))) flatMap {
        case true =>
          choice(Crash -> 0.9, NoCrash -> 0.1)
        case _ =>
          always(NoCrash)
      }
    }
  }

  def crash2(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    (driver1(light) === always(Drive) && (driver2(otherLight(light)) === always(Drive))) flatMap {
      case true =>
        choice(Crash -> 0.9, NoCrash -> 0.1) dbg "result"
      case _ =>
        always(NoCrash)
    }
  }



  val trafficModel = crash(cautiousDriver, aggressiveDriver, trafficLight)
  val trafficModel2 = crash(aggressiveDriver, aggressiveDriver, trafficLight)

  val trafficModel3 = crash2(cautiousDriver, aggressiveDriver, trafficLight)
  val trafficModel4 = crash2(aggressiveDriver, aggressiveDriver, trafficLight)


  val cond1 = {
    val x = flip(0.5)
    x flatMap { 
      case true => always(1) 
      case _ => x map  {
        case true => 2 
        case _ => 3
      }
    }
  }

  val coinModel1 = {
    val coin = choice(0 -> 0.5, 1 -> 0.5)
    val sum1 = coin + coin
    val sum2 = sum1 + coin
    sum2
  }

  val coinModel2 = {
    val coin = choice(0 -> 0.5, 1 -> 0.5)
    val sum1 = coin + coin
    val sum2 = sum1 + coin
    (sum2 === always(3)) flatMap {
      case true => sum1
      case false => coin
    }
  }

}

trait AppendProg extends ProbLang {

  def randomList(): Rand[List[Boolean]] = flip(0.5).flatMap {
    case false => always(Nil)
    case true  => 
      val x = flip(0.5)
      val tail = randomList()
      x.flatMap(x => tail.map(xs=>x::xs))
  }

  def append[T](x: Rand[List[T]], y: Rand[List[T]]): Rand[List[T]] = x flatMap {
    case Nil => y
    case h::tl => append(always(tl),y).map(xs=>h::xs) // full list as input, not very efficient?
  }

  val t3 = List(true, true, true)
  val f2 = List(false, false)

  val appendModel1 = {
    append(always(t3),always(f2))
  }

  val appendModel2 = {
    append(flip(0.5).map(_::Nil),always(f2))
  }

  def appendModel3 = { // needs lazy strategy
    append(always(t3),randomList())
  }

  def appendModel4 = {
    // query: X:::f2 == t3:::f2 solve for X
    randomList().flatMap{ x =>
      append(always(x),always(f2)).flatMap {
        case res if res == t3:::f2 => always((x,f2,res))
        case _ => never
      }
    }
  }

  def appendModel5 = {
    // query: X:::Y == t3:::f2 solve for X,Y
    randomList().flatMap{ x =>
      randomList().flatMap{ y =>
        append(always(x),always(y)).flatMap {
          case res if res == t3:::f2 => always((x,y))
          case _ => never
    }}}
  }



  // now try lists where the tail itself is a random var

  abstract class CList[+A]
  case object CNil extends CList[Nothing]
  case class CCons[+A](hd: A, tl: Rand[CList[A]]) extends CList[A]

  def asCList[A](x: List[A]): Rand[CList[A]] = x match {
    case Nil => always(CNil)
    case x::xs => always(CCons(x, asCList(xs)))
  }
  def asLists[A](x: Rand[CList[A]]): Rand[List[A]] = x flatMap {
    case CNil => always(Nil)
    case CCons(x, xs) => asLists(xs).map(xs=>x::xs)
  }

  def randomCList(): Rand[CList[Boolean]] = flip(0.5).flatMap {
    case false => always(CNil)
    case true  => 
      val x = flip(0.5)
      val tail = randomCList()
      x.map(x => CCons(x, tail))
  }

  def appendC[T](x: Rand[CList[T]], y: Rand[CList[T]]): Rand[CList[T]] = x flatMap {
    case CNil => y
    case CCons(h,t) => always(CCons(h, appendC(t,y)))
  }

  def listSameC[T](x: Rand[CList[T]], y: Rand[CList[T]]): Rand[Boolean] = 
    x.flatMap { u => y.flatMap { v => (u,v) match {
      case (CCons(a,x),CCons(b,y)) if a == b => listSameC(x,y)
      case (CNil,CNil) => always(true)
      case _ => always(false)
    }}}


  val t3c = asCList(t3)
  val f2c = asCList(f2)

  def appendModel3b = {
    asLists(appendC(t3c,randomCList()))
  }

  def appendModel4b = {
    // query: X:::f2 == t3:::f2 solve for X
    val x = randomCList()
    val t3f2 = t3++f2
    listSameC(appendC(x,f2c), asCList(t3f2)).flatMap {
      case true => 
        // here we rely on memoization: otherwise x.tail would be making new choices all the time,
        asLists(x).map(x=>(x,f2,t3f2))
      case _ => never
    }
  }

  def appendModel5b = {
    // query: X:::Y == t3:::f2 solve for X,Y
    val x = randomCList()
    val y = randomCList()
    listSameC(appendC(x,y), asCList(t3++f2)).flatMap {
      case true => for (a <- asLists(x); b <- asLists(y)) yield (a,b)
      case _    => never
    }
  }


}


trait RunTests extends ProbLang with ProbPrettyPrint with ProbLangExTraffic with ProbRuleLang with AppendProg {
  show(cond1, "cond1")

  show(coinModel1, "coinModel1")
  show(coinModel2, "coinModel2")

  show(coffeeModel1, "coffeeModel1")

  show(trafficModel, "trafficModel")
  show(trafficModel2, "trafficModel2")
  show(trafficModel3, "trafficModel3")
  show(trafficModel4, "trafficModel4")
}


object Test extends App {
  new RunTests with ProbCore {}
  new RunTests with ProbCoreLazy {
    // append needs lazyness
    show(appendModel1, "appendModel1")
    show(appendModel2, "appendModel2")
    show(appendModel3, "appendModel3", "", 5)
    show(appendModel4, "appendModel4", "", 1)
    show(appendModel5, "appendModel5", "", 5)

    show(appendModel3b, "appendModel3b", "", 5)
    show(appendModel4b, "appendModel4b", "", 1)
    show(appendModel5b, "appendModel5b", "", 5)
  }
}
