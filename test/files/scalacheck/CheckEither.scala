import org.scalacheck.{ Arbitrary, Prop, Properties }
import org.scalacheck.Arbitrary.{arbitrary, arbThrowable}
import org.scalacheck.Gen.oneOf
import org.scalacheck.Prop._
import org.scalacheck.Test.check
import Function.tupled

object Test extends Properties("Either") {
  implicit def arbitraryEither[X, Y](implicit xa: Arbitrary[X], ya: Arbitrary[Y]): Arbitrary[Either[X, Y]] =
    Arbitrary[Either[X, Y]](oneOf(arbitrary[X].map(Left(_)), arbitrary[Y].map(Right(_))))

  val prop_either1 = forAll((n: Int) => Left(n).fold(x => x, b => sys.error("fail")) == n)

  val prop_either2 = forAll((n: Int) => Right(n).fold(a => sys.error("fail"), x => x) == n)

  val prop_swap = forAll((e: Either[Int, Int]) => e match {
    case Left(a) => e.swap.right.get == a
    case Right(b) => e.swap.left.get == b
  })

  val prop_isLeftRight = forAll((e: Either[Int, Int]) => e.isLeft != e.isRight)

  object CheckLeftProjection {
    val prop_value = forAll((n: Int) => Left(n).left.get == n)

    val prop_getOrElse = forAll((e: Either[Int, Int], or: Int) => e.left.getOrElse(or) == (e match {
      case Left(a) => a
      case Right(_) => or
    }))

    val prop_forall = forAll((e: Either[Int, Int]) =>
      e.left.forall(_ % 2 == 0) == (e.isRight || e.left.get % 2 == 0))

    val prop_exists = forAll((e: Either[Int, Int]) =>
      e.left.exists(_ % 2 == 0) == (e.isLeft && e.left.get % 2 == 0))

    val prop_flatMapLeftIdentity = forAll((e: Either[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
      Left(n).left.flatMap(f(_)) == f(n)})

    val prop_flatMapRightIdentity = forAll((e: Either[Int, Int]) => e.left.flatMap(Left(_)) == e)

    val prop_flatMapComposition = forAll((e: Either[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
      def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
      e.left.flatMap(f(_)).left.flatMap(g(_)) == e.left.flatMap(f(_).left.flatMap(g(_)))})

    val prop_mapIdentity = forAll((e: Either[Int, Int]) => e.left.map(x => x) == e)

    val prop_mapComposition = forAll((e: Either[String, Int]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.left.map(x => f(g(x))) == e.left.map(x => g(x)).left.map(f(_))})

    val prop_filter = forAll((e: Either[Int, Int], x: Int) => e.left.filter(_ % 2 == 0) ==
      (if(e.isRight || e.left.get % 2 != 0) None else Some(e)))

    val prop_seq = forAll((e: Either[Int, Int]) => e.left.toSeq == (e match {
      case Left(a) => Seq(a)
      case Right(_) => Seq.empty
    }))

    val prop_option = forAll((e: Either[Int, Int]) => e.left.toOption == (e match {
      case Left(a) => Some(a)
      case Right(_) => None
    }))
  }

  object CheckRightProjection {
    val prop_value = forAll((n: Int) => Right(n).right.get == n)

    val prop_getOrElse = forAll((e: Either[Int, Int], or: Int) => e.right.getOrElse(or) == (e match {
      case Left(_) => or
      case Right(b) => b
    }))

    val prop_forall = forAll((e: Either[Int, Int]) =>
      e.right.forall(_ % 2 == 0) == (e.isLeft || e.right.get % 2 == 0))

    val prop_exists = forAll((e: Either[Int, Int]) =>
      e.right.exists(_ % 2 == 0) == (e.isRight && e.right.get % 2 == 0))

    val prop_flatMapLeftIdentity = forAll((e: Either[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
      Right(n).right.flatMap(f(_)) == f(n)})

    val prop_flatMapRightIdentity = forAll((e: Either[Int, Int]) => e.right.flatMap(Right(_)) == e)

    val prop_flatMapComposition = forAll((e: Either[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
      def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
      e.right.flatMap(f(_)).right.flatMap(g(_)) == e.right.flatMap(f(_).right.flatMap(g(_)))})

    val prop_mapIdentity = forAll((e: Either[Int, Int]) => e.right.map(x => x) == e)

    val prop_mapComposition = forAll((e: Either[Int, String]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.right.map(x => f(g(x))) == e.right.map(x => g(x)).right.map(f(_))})

    val prop_filter = forAll((e: Either[Int, Int], x: Int) => e.right.filter(_ % 2 == 0) ==
      (if(e.isLeft || e.right.get % 2 != 0) None else Some(e)))

    val prop_seq = forAll((e: Either[Int, Int]) => e.right.toSeq == (e match {
      case Left(_) => Seq.empty
      case Right(b) => Seq(b)
    }))

    val prop_option = forAll((e: Either[Int, Int]) => e.right.toOption == (e match {
      case Left(_) => None
      case Right(b) => Some(b)
    }))
  }

  val prop_Either_left = forAll((n: Int) => Left(n).left.get == n)

  val prop_Either_right = forAll((n: Int) => Right(n).right.get == n)

  val prop_Either_joinLeft = forAll((e: Either[Either[Int, Int], Int]) => e match {
    case Left(ee) => e.joinLeft == ee
    case Right(n) => e.joinLeft == Right(n)
  })

  val prop_Either_joinRight = forAll((e: Either[Int, Either[Int, Int]]) => e match {
    case Left(n) => e.joinRight == Left(n)
    case Right(ee) => e.joinRight == ee
  })

  val prop_Either_reduce = forAll((e: Either[Int, Int]) =>
    e.merge == (e match {
      case Left(a) => a
      case Right(a) => a
    }))

  val prop_getOrElse = forAll((e: Either[Int, Int], or: Int) => e.getOrElse(or) == (e match {
    case Left(_) => or
    case Right(b) => b
  }))

  val prop_contains = forAll((e: Either[Int, Int], n: Int) =>
    e.contains(n) == (e.isRight && e.right.get == n))

  val prop_forall = forAll((e: Either[Int, Int]) =>
    e.forall(_ % 2 == 0) == (e.isLeft || e.right.get % 2 == 0))

  val prop_exists = forAll((e: Either[Int, Int]) =>
    e.exists(_ % 2 == 0) == (e.isRight && e.right.get % 2 == 0))

  val prop_flatMapLeftIdentity = forAll((e: Either[Int, Int], n: Int, s: String) => {
    def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
    Right(n).flatMap(f(_)) == f(n)})

  val prop_flatMapRightIdentity = forAll((e: Either[Int, Int]) => e.flatMap(Right(_)) == e)

  val prop_flatMapComposition = forAll((e: Either[Int, Int]) => {
    def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
    def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
    e.flatMap(f(_)).flatMap(g(_)) == e.flatMap(f(_).flatMap(g(_)))})

  val prop_mapIdentity = forAll((e: Either[Int, Int]) => e.map(x => x) == e)

  val prop_mapComposition = forAll((e: Either[Int, String]) => {
    def f(s: String) = s.toLowerCase
    def g(s: String) = s.reverse
    e.map(x => f(g(x))) == e.map(x => g(x)).map(f(_))})

  val prop_filterOrElse = forAll((e: Either[Int, Int], x: Int) => e.filterOrElse(_ % 2 == 0, -x) ==
    (if(e.isLeft) e
     else if(e.right.get % 2 == 0) e
     else Left(-x)))

  val prop_seq = forAll((e: Either[Int, Int]) => e.toSeq == (e match {
    case Left(_)  => collection.immutable.Seq.empty
    case Right(b) => collection.immutable.Seq(b)
  }))

  val prop_option = forAll((e: Either[Int, Int]) => e.toOption == (e match {
    case Left(_)  => None
    case Right(b) => Some(b)
  }))

  val prop_try = forAll((e: Either[Throwable, Int]) => e.toTry == (e match {
    case Left(a)  => util.Failure(a)
    case Right(b) => util.Success(b)
  }))

  /** Hard to believe I'm "fixing" a test to reflect B before A ... */
  val prop_Either_cond = forAll((c: Boolean, a: Int, b: Int) =>
    Either.cond(c, a, b) == (if(c) Right(a) else Left(b)))

  val tests = List(
      ("prop_either1", prop_either1),
      ("prop_either2", prop_either2),
      ("prop_swap", prop_swap),
      ("prop_isLeftRight", prop_isLeftRight),
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
      ("Left.prop_seq", CheckLeftProjection.prop_seq),
      ("Left.prop_option", CheckLeftProjection.prop_option),
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
      ("Right.prop_seq", CheckRightProjection.prop_seq),
      ("Right.prop_option", CheckRightProjection.prop_option),
      ("prop_Either_left", prop_Either_left),
      ("prop_Either_right", prop_Either_right),
      ("prop_Either_joinLeft", prop_Either_joinLeft),
      ("prop_Either_joinRight", prop_Either_joinRight),
      ("prop_Either_reduce", prop_Either_reduce),      
      ("prop_getOrElse", prop_getOrElse),
      ("prop_contains", prop_contains),
      ("prop_forall", prop_forall),
      ("prop_exists", prop_exists),
      ("prop_flatMapLeftIdentity", prop_flatMapLeftIdentity),
      ("prop_flatMapRightIdentity", prop_flatMapRightIdentity),
      ("prop_flatMapComposition", prop_flatMapComposition),
      ("prop_mapIdentity", prop_mapIdentity),
      ("prop_mapComposition", prop_mapComposition),
      ("prop_filterOrElse", prop_filterOrElse),
      ("prop_seq", prop_seq),
      ("prop_option", prop_option),
      ("prop_try", prop_try),
      ("prop_Either_cond", prop_Either_cond))

  for ((label, prop) <- tests) {
    property(label) = prop
  }
}
