
import scala.language.{ existentials, postfixOps }
import scala.reflect.runtime.universe._

object r {
  class A
  class B
  class List[+A]
  object List { def apply[A](xs: A*): List[A] = new List[A] }
  object Nil extends List[Nothing]

  trait OuterP[A] {
    trait Inner
    trait InnerP[B]
  }
  trait Outer {
    trait Inner
    trait InnerP[B]
  }
  object Un    { def unapply(x: Any)    = Some(5) }
  object UnSeq { def unapplySeq(x: Any) = Some(Seq(5)) }
  class C[T]
  class D
  trait E

  trait SN {
    def q: Any = null
  }
}

object s {
  import r._

  trait NN extends SN {
    def act[T](expr: Expr[T]): Unit

    act(reify { s                                           /* Ident */ })
    act(reify { r.List                                      /* Select */ })
    act(reify { List()                                      /* Apply */ })
    act(reify { List(1)                                     /* Literal */ })
    act(reify { List[Int]()                                 /* TypeApply */ })
    act(reify { 1: Int                                      /* Typed */ })
    act(reify { null: List[Int]                             /* AppliedTypeTree */ })
    act(reify { () ; ()                                     /* Block */ })
    act(reify { val x: Int = 0                              /* ValDef */ })
    act(reify { val x = 0                                   /* TypeTree */ })
    act(reify { if (true) ()                                /* If */ })
    act(reify { def f { }                                   /* DefDef */ })
    act(reify { def m = super.q                             /* Super */ })
    act(reify { trait A                                     /* ClassDef Template */ })
    act(reify { def f(x: Any) { }                           /* EmptyTree */ })
    act(reify { null: D with E                              /* CompoundTypeTree */ })
    act(reify { type T = Int                                /* TypeDef */ })
    act(reify { type CC[T <: D] = C[T]                      /* TypeBoundsTree */ })
    act(reify { try 0 finally println("")                   /* Try */ })
    act(reify { (x: Int) => x                               /* Function */ })
    act(reify { var v = 1 ; v = 2                           /* Assign */ })
    act(reify { class A() { def this(x: A) = this() }       /* This */ })
    act(reify { new List[Int]                               /* New */ })
    act(reify { 0: @unchecked                               /* Annotated */ })
    act(reify { null: Outer#Inner                           /* SelectFromTypeTree */ })
    act(reify { null: Nil.type                              /* SingletonTypeTree */ })
    act(reify { null: (T forSome { type T })                /* ExistentialTypeTree */ })
    act(reify { import r.{ A, B => C };                     /* Import */ })
    act(reify { def f: Int = return 0                       /* Return */ })
    act(reify { object x                                    /* ModuleDef */ })
    act(reify { throw new java.lang.Exception               /* Throw */ })
    act(reify { 0 match { case _ => 0 }                     /* Match CaseDef */ })
    act(reify { 0 match { case 1 | 2 => 0 }                 /* Alternative */ })
    act(reify { q match { case x @ List => 0 }              /* Bind */ })
    act(reify { q match { case UnSeq(1, _*) => 0 }          /* Star */ })

    // ``unexpected: bound type that doesn't have a tpe: Ident(newTypeName("Int"))''
    // act(reify { r.List[T forSome { type T <: Int }]() })    // Was crashing , no longer
    //
    // error: exception during macro expansion:
    // scala.MatchError: collection.this.Seq.unapplySeq[A] (of class scala.reflect.internal.Trees$TypeApply)
    //   at scala.reflect.reify.phases.Reshape$$anon$1.extractExtractor$1(Reshape.scala:73)
    //   at scala.reflect.reify.phases.Reshape$$anon$1.transform(Reshape.scala:82)
    //   at scala.reflect.reify.phases.Reshape$$anon$1.transform(Reshape.scala:24)
    //   at scala.reflect.internal.Trees$class.itransform(Trees.scala:1290)
    //
    // act(reify { r.List[Any]() match { case Seq(1, _*) => 1 } } )

    // act(reify { List[OuterP[Int]#InnerP[Byte]]() })
    //
    // SI-7243
    //
    // test/files/run/reify-each-node-type.scala:85: error: Cannot materialize r.List.apply[r.OuterP[Int]#InnerP[Byte]]() as { ... } because:
    // scala.reflect.macros.TypecheckException: value TypeTreeWithDeferredRefCheck is not a member of type parameter U
    //     act(reify { List[OuterP[Int]#InnerP[Byte]]() })
    //               ^
    // one error found
  }
}

object Test {
  var idx = 0
  val seen = scala.collection.mutable.Set[String]()

  object N extends s.NN {
    def act[T](expr: Expr[T]): Unit = {
      idx += 1
      val ts = expr.tree filter (_ => true) map (_.getClass.getName split "[.$]" last) filterNot seen distinct;
      println("%2d  %60s  %s".format(idx, expr.tree.toString.replaceAll("""\s+""", " ").take(60), ts mkString " "))
      seen ++= ts
    }
  }
  def main(args: Array[String]): Unit = N
}
