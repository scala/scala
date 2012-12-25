import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TypeDeconstructionProps extends Properties("type deconstruction")
                                  with TreeSimiliarity
                                  with ArbitraryTreesAndNames {

  property("ident(type name)") = forAll { (name: TypeName) =>
    val t = Ident(name)
    val tq"$t1" = t
    t1 ≈ t
  }

  property("applied type tree") = forAll { (name1: TypeName, name2: TypeName) =>
    val tq"$a[$b]" = AppliedTypeTree(Ident(name1), List(Ident(name2)))
    a ≈ Ident(name1) && b ≈ Ident(name2)
  }

  property("dependent type tree single arg") = forAll { (name: TypeName, arg: Tree) =>
    val tq"$name1($arg1)" = DependentTypeTree(Ident(name), List(arg))
    name1 ≈ Ident(name) && arg1 ≈ arg
  }

  property("dependent type tree multiple args") = forAll { (name: TypeName, args: List[Tree]) =>
    val tq"$name1(..$args1)" = DependentTypeTree(Ident(name), args)
    name1 ≈ Ident(name) && args1.zip(args).forall { case (a1, a) => a1 ≈ a }
  }

}