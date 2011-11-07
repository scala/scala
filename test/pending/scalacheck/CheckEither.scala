
// To run these tests:
// 1) wget http://scalacheck.googlecode.com/files/ScalaCheck-1.2.jar
// 2) scalac -classpath ScalaCheck-1.2.jar Either.scala
// 3) scala -classpath .:ScalaCheck-1.2.jar scala.CheckEither
// 4) observe passing tests

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{arbitrary, arbThrowable}
import org.scalacheck.Gen.oneOf
import org.scalacheck.{Prop, StdRand}
import org.scalacheck.Prop._
import org.scalacheck.ConsoleReporter.{testReport, propReport}
import org.scalacheck.Test.{Params, check}
import org.scalacheck.ConsoleReporter.testStatsEx
import Function.tupled

object CheckEither {
  implicit def arbitraryEither[X, Y](implicit xa: Arbitrary[X], ya: Arbitrary[Y]): Arbitrary[Either[X, Y]] = 
    Arbitrary[Either[X, Y]](oneOf(arbitrary[X].map(Left(_)), arbitrary[Y].map(Right(_))))

  val prop_either1 = property((n: Int) => Left(n).either(x => x, b => error("fail")) == n)

  val prop_either2 = property((n: Int) => Right(n).either(a => error("fail"), x => x) == n)

  val prop_swap = property((e: Either[Int, Int]) => e match {
    case Left(a) => e.swap.right.value == a
    case Right(b) => e.swap.left.value == b
  })
  
  val prop_isLeftRight = property((e: Either[Int, Int]) => e.isLeft != e.isRight)

  object CheckLeftProjection {
    val prop_valueE = property((n: Int, s: String) => Left(n).left.valueE(s) == n)

    val prop_value = property((n: Int) => Left(n).left.value == n)

    val prop_on = property((e: Either[Int, Int]) => e.left.on((_: Int) + 1) == (e match {
      case Left(a) => a
      case Right(b) => b + 1
    }))

    val prop_getOrElse = property((e: Either[Int, Int], or: Int) => e.left.getOrElse(or) == (e match {
      case Left(a) => a
      case Right(_) => or
    }))

    val prop_forall = property((e: Either[Int, Int]) =>
      e.left.forall(_ % 2 == 0) == (e.isRight || e.left.value % 2 == 0))

    val prop_exists = property((e: Either[Int, Int]) =>
      e.left.exists(_ % 2 == 0) == (e.isLeft && e.left.value % 2 == 0))
  
    val prop_flatMapLeftIdentity = property((e: Either[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
      Left(n).left.flatMap(f(_)) == f(n)})

    val prop_flatMapRightIdentity = property((e: Either[Int, Int]) => e.left.flatMap(Left(_)) == e)

    val prop_flatMapComposition = property((e: Either[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
      def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
      e.left.flatMap(f(_)).left.flatMap(g(_)) == e.left.flatMap(f(_).left.flatMap(g(_)))})

    val prop_mapIdentity = property((e: Either[Int, Int]) => e.left.map(x => x) == e)

    val prop_mapComposition = property((e: Either[String, Int]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.left.map(x => f(g(x))) == e.left.map(x => g(x)).left.map(f(_))})

    val prop_filter = property((e: Either[Int, Int], x: Int) => e.left.filter(_ % 2 == 0) ==
      (if(e.isRight || e.left.value % 2 != 0) None else Some(e)))

    val prop_ap = property((e1: Either[Int, Int], e2: Either[Int, Int]) => {
      val ee2 = e2.left map (n => (x: Int) => x + n)
      e1.left.ap(ee2) == (ee2 match {
        case Left(f) => e1 match {
          case Left(a) => Left(f(a))
          case Right(b) => Right(b)
        }
        case Right(b) => Right(b)
      })
    })

    val prop_seq = property((e: Either[Int, Int]) => e.left.seq == (e match {
      case Left(a) => Seq.singleton(a)
      case Right(_) => Seq.empty
    }))

    val prop_option = property((e: Either[Int, Int]) => e.left.option == (e match {
      case Left(a) => Some(a)
      case Right(_) => None
    }))
  }

  object CheckRightProjection {
    val prop_valueE = property((n: Int, s: String) => Right(n).right.valueE(s) == n)

    val prop_value = property((n: Int) => Right(n).right.value == n)

    val prop_on = property((e: Either[Int, Int]) => e.right.on((_: Int) + 1) == (e match {
      case Left(a) => a + 1
      case Right(b) => b
    }))

    val prop_getOrElse = property((e: Either[Int, Int], or: Int) => e.right.getOrElse(or) == (e match {
      case Left(_) => or
      case Right(b) => b
    }))

    val prop_forall = property((e: Either[Int, Int]) =>
      e.right.forall(_ % 2 == 0) == (e.isLeft || e.right.value % 2 == 0))

    val prop_exists = property((e: Either[Int, Int]) =>
      e.right.exists(_ % 2 == 0) == (e.isRight && e.right.value % 2 == 0))

    val prop_flatMapLeftIdentity = property((e: Either[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
      Right(n).right.flatMap(f(_)) == f(n)})

    val prop_flatMapRightIdentity = property((e: Either[Int, Int]) => e.right.flatMap(Right(_)) == e)

    val prop_flatMapComposition = property((e: Either[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
      def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
      e.right.flatMap(f(_)).right.flatMap(g(_)) == e.right.flatMap(f(_).right.flatMap(g(_)))})

    val prop_mapIdentity = property((e: Either[Int, Int]) => e.right.map(x => x) == e)

    val prop_mapComposition = property((e: Either[Int, String]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.right.map(x => f(g(x))) == e.right.map(x => g(x)).right.map(f(_))})

    val prop_filter = property((e: Either[Int, Int], x: Int) => e.right.filter(_ % 2 == 0) ==
      (if(e.isLeft || e.right.value % 2 != 0) None else Some(e)))

    val prop_ap = property((e1: Either[Int, Int], e2: Either[Int, Int]) => {
      val ee2 = e2.right map (n => (x: Int) => x + n)
      e1.right.ap(ee2) == (ee2 match {
        case Left(a) => Left(a)
        case Right(f) => e1 match {
          case Left(a) => Left(a)
          case Right(b) => Right(f(b))
        }
      })
    })

    val prop_seq = property((e: Either[Int, Int]) => e.right.seq == (e match {
      case Left(_) => Seq.empty
      case Right(b) => Seq.singleton(b)
    }))

    val prop_option = property((e: Either[Int, Int]) => e.right.option == (e match {
      case Left(_) => None
      case Right(b) => Some(b)
    }))
  }

  val prop_Either_left = property((n: Int) => Either.left(n).left.value == n)
  
  val prop_Either_right = property((n: Int) => Either.right(n).right.value == n)

  val prop_Either_joinLeft = property((e: Either[Either[Int, Int], Int]) => e match {
    case Left(ee) => Either.joinLeft(e) == ee
    case Right(n) => Either.joinLeft(e) == Right(n)
  })

  val prop_Either_joinRight = property((e: Either[Int, Either[Int, Int]]) => e match {
    case Left(n) => Either.joinRight(e) == Left(n)
    case Right(ee) => Either.joinRight(e) == ee
  })

  val prop_Either_lefts = property((es: List[Either[Int, Int]]) => 
    Either.lefts(es) == es.filter(_.isLeft).map(_.left.value))
  
  val prop_Either_rights = property((es: List[Either[Int, Int]]) => 
    Either.rights(es) == es.filter(_.isRight).map(_.right.value))
  
  val prop_Either_leftRights = property((es: List[Either[Int, Int]]) => 
    Either.rights(es) == es.filter(_.isRight).map(_.right.value))
    
  val prop_Either_throws = property((n: Int) => 
    Either.throws(n) == Right(n) && Either.throws(error("error")).isLeft)
  
  val prop_Either_throwIt = property((e: Either[Throwable, Int]) => 
    try {
      Either.throwIt(e) == e.right.value
    } catch {
      case (t) => e.isLeft && e.left.value == t
    })

  val prop_Either_reduce = property((e: Either[Int, Int]) => 
    Either.reduce(e) == (e match {
      case Left(a) => a
      case Right(a) => a
    }))
    
  val prop_Either_iif = property((c: Boolean, a: Int, b: Int) =>
    Either.iif(c, a, b) == (if(c) Right(b) else Left(a))) 

  val tests = List(
      ("prop_either1", prop_either1),
      ("prop_either2", prop_either2),
      ("prop_swap", prop_swap),
      ("prop_isLeftRight", prop_isLeftRight),
      ("Left.prop_valueE", CheckLeftProjection.prop_valueE),
      ("Left.prop_value", CheckLeftProjection.prop_value),
      ("Left.prop_getOrElse", CheckLeftProjection.prop_getOrElse),
      ("Left.prop_forall", CheckLeftProjection.prop_forall),
      ("Left.prop_exists", CheckLeftProjection.prop_exists),
      ("Left.prop_flatMapLeftIdentity", CheckLeftProjection.prop_flatMapLeftIdentity),
      ("Left.prop_flatMapRightIdentity", CheckLeftProjection.prop_flatMapRightIdentity),
      ("Left.prop_flatMapComposition", CheckLeftProjection.prop_flatMapComposition),
      ("Left.prop_mapIdentity", CheckLeftProjection.prop_mapIdentity),
      ("Left.prop_mapComposition", CheckLeftProjection.prop_mapComposition),
      ("Left.prop_filter", CheckLeftProjection.prop_filter),
      ("Left.prop_ap", CheckLeftProjection.prop_ap),
      ("Left.prop_seq", CheckLeftProjection.prop_seq),
      ("Left.prop_option", CheckLeftProjection.prop_option),
      ("Right.prop_valueE", CheckRightProjection.prop_valueE),
      ("Right.prop_value", CheckRightProjection.prop_value),
      ("Right.prop_getOrElse", CheckRightProjection.prop_getOrElse),
      ("Right.prop_forall", CheckRightProjection.prop_forall),
      ("Right.prop_exists", CheckRightProjection.prop_exists),
      ("Right.prop_flatMapLeftIdentity", CheckRightProjection.prop_flatMapLeftIdentity),
      ("Right.prop_flatMapRightIdentity", CheckRightProjection.prop_flatMapRightIdentity),
      ("Right.prop_flatMapComposition", CheckRightProjection.prop_flatMapComposition),
      ("Right.prop_mapIdentity", CheckRightProjection.prop_mapIdentity),
      ("Right.prop_mapComposition", CheckRightProjection.prop_mapComposition),
      ("Right.prop_filter", CheckRightProjection.prop_filter),
      ("Right.prop_ap", CheckRightProjection.prop_ap),
      ("Right.prop_seq", CheckRightProjection.prop_seq),
      ("Right.prop_option", CheckRightProjection.prop_option),
      ("prop_Either_left", prop_Either_left),
      ("prop_Either_right", prop_Either_right),      
      ("prop_Either_joinLeft", prop_Either_joinLeft),
      ("prop_Either_joinRight", prop_Either_joinRight),      
      ("prop_Either_lefts", prop_Either_lefts),
      ("prop_Either_rights", prop_Either_rights),      
      ("prop_Either_leftRights", prop_Either_leftRights),      
      ("prop_Either_throws", prop_Either_throws),      
      ("prop_Either_throwIt", prop_Either_throwIt),      
      ("prop_Either_reduce", prop_Either_reduce),      
      ("prop_Either_iif", prop_Either_iif)
    )
  
  def main(args: Array[String]) = 
    tests foreach (tupled((name, prop) => 
    testStatsEx(name, testReport(check(Params(500, 0, 0, 500, StdRand), prop, propReport)))))
}

