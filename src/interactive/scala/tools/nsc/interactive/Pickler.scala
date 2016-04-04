package scala.tools.nsc.interactive

import Lexer._
import java.io.Writer

/** An abstract class for writing and reading Scala objects to and
 *  from a legible representation. The representation follows the following grammar:
 *  {{{
 *  Pickled = `true` | `false` | `null` | NumericLit | StringLit |
 *            Labelled | Pickled `,` Pickled
 *  Labelled = StringLit `(` Pickled? `)`
 *  }}}
 *
 *  All ...Lit classes are as in JSON. @see scala.tools.nsc.io.Lexer
 *
 *  Subclasses of `Pickler` each can write and read individual classes
 *  of values.
 *
 *  @tparam  T   the type of values handled by this pickler.
 *
 *  These Picklers build on the work of Andrew Kennedy. They are most closely inspired by
 *  Iulian Dragos' picklers for Scala to XML. See:
 *
 *  <a href="http://code.google.com/p/gdata-scala-client/wiki/DevelopersGuide">
 *  http://code.google.com/p/gdata-scala-client/wiki/DevelopersGuide
 *  </a>
 */
abstract class Pickler[T] {

  import Pickler._

  /** Writes value in pickled form
   *  @param  wr   the writer to which pickled form is written
   *  @param  x    the value to write
   */
  def pickle(wr: Writer, x: T)

  /** Reads value from pickled form.
   *
   *  @param  rd   the lexer from which lexemes are read
   *  @return An `UnpickleSuccess value if the current input corresponds to the
   *          kind of value that is unpickled by the current subclass of `Pickler`,
   *          an `UnpickleFailure` value otherwise.
   *  @throws  `Lexer.MalformedInput` if input is invalid, or if
   *          an `Unpickle
   */
  def unpickle(rd: Lexer): Unpickled[T]

  /** A pickler representing a `~`-pair of values as two consecutive pickled
   *  strings, separated by a comma.
   *  @param  that   the second pickler which together with the current pickler makes
   *                 up the pair `this ~ that` to be pickled.
   */
  def ~ [U] (that: => Pickler[U]): Pickler[T ~ U] = seqPickler(this, that)

  /** A pickler that adds a label to the current pickler, using the representation
   *   `label ( <current pickler> )`
   *
   *  @label  the string to be added as a label.
   */
  def labelled(label: String): Pickler[T] = labelledPickler(label, this)

  /** A pickler obtained from the current pickler by a pair of transformer functions
   *  @param   in   the function that maps values handled by the current pickler to
   *                values handled by the wrapped pickler.
   *  @param   out  the function that maps values handled by the wrapped pickler to
   *                values handled by the current pickler.
   */
  def wrapped [U] (in: T => U)(out: U => T): Pickler[U] = wrappedPickler(this)(in)(out)

  /** A conditional pickler obtained from the current pickler.
   *  @param   p   the condition to test to find out whether pickler can handle
   *               some Scala value.
   */
  def cond(p: Any => Boolean): CondPickler[T] = conditionalPickler(this, p)

  /** A conditional pickler handling values of some Scala class. It adds the
   *  class name as a label to the representation of the current pickler and
   *  @param    c     the class of values handled by this pickler.
   */
  def asClass[U <: T](c: Class[U]): CondPickler[T] = this.labelled(c.getName).cond(c isInstance _)
}

object Pickler {
  /** A base class representing unpickler result. It has two subclasses:
   *  `UnpickleSuccess` for successful unpicklings and `UnpickleFailure` for failures,
   *  where a value of the given type `T` could not be unpickled from input.
   *  @tparam  T the type of unpickled values in case of success.
   */
  abstract class Unpickled[+T] {
    /** Transforms success values to success values using given function,
     *  leaves failures alone
     *  @param   f the function to apply.
     */
    def map[U](f: T => U): Unpickled[U] = this match {
      case UnpickleSuccess(x) => UnpickleSuccess(f(x))
      case f: UnpickleFailure => f
    }
    /** Transforms success values to successes or failures using given function,
     *  leaves failures alone.
     *  @param   f the function to apply.
     */
    def flatMap[U](f: T => Unpickled[U]): Unpickled[U] = this match {
      case UnpickleSuccess(x) => f(x)
      case f: UnpickleFailure => f
    }
    /** Tries alternate expression if current result is a failure
     *  @param alt  the alternate expression to be tried in case of failure
     */
    def orElse[U >: T](alt: => Unpickled[U]): Unpickled[U] = this match {
      case UnpickleSuccess(x) => this
      case f: UnpickleFailure => alt
    }

    /** Transforms failures into thrown `MalformedInput` exceptions.
     *  @throws  MalformedInput   if current result is a failure
     */
    def requireSuccess: UnpickleSuccess[T] = this match {
      case s @ UnpickleSuccess(x) => s
      case f: UnpickleFailure =>
        throw new MalformedInput(f.rd, "Unrecoverable unpickle failure:\n"+f.errMsg)
    }
  }

  /** A class representing successful unpicklings
   *  @tparam T       the type of the unpickled value
   *  @param result   the unpickled value
   */
  case class UnpickleSuccess[+T](result: T) extends Unpickled[T]

  /** A class representing unpickle failures
   *  @param msg      an error message describing what failed.
   *  @param rd       the lexer unpickled values were read from (can be used to get
   *                  error position, for instance).
   */
  class UnpickleFailure(msg: => String, val rd: Lexer) extends Unpickled[Nothing] {
    def errMsg = msg
    override def toString = "Failure at "+rd.tokenPos+":\n"+msg
  }

  private def errorExpected(rd: Lexer, msg: => String) =
    new UnpickleFailure("expected: "+msg+"\n" +
                        "found   : "+rd.token,
                        rd)

  private def nextSuccess[T](rd: Lexer, result: T) = {
    rd.nextToken()
    UnpickleSuccess(result)
  }

  /** The implicit `Pickler` value for type `T`. Equivalent to `implicitly[Pickler[T]]`.
   */
  def pkl[T: Pickler] = implicitly[Pickler[T]]

  /** A class representing `~`-pairs */
  case class ~[+S, +T](fst: S, snd: T)

  /** A wrapper class to be able to use `~` s an infix method */
  implicit class TildeDecorator[S](x: S) {
    /** Infix method that forms a `~`-pair. */
    def ~ [T](y: T): S ~ T = new ~ (x, y)
  }

  /** Same as `p.labelled(label)`.
   */
  def labelledPickler[T](label: String, p: Pickler[T]): Pickler[T] = new Pickler[T] {
    def pickle(wr: Writer, x: T) = {
      wr.write(quoted(label))
      wr.write("(")
      p.pickle(wr, x)
      wr.write(")")
    }
    def unpickle(rd: Lexer): Unpickled[T] =
      rd.token match {
        case StringLit(`label`) =>
          rd.nextToken()
          rd.accept('(')
          val result = p.unpickle(rd).requireSuccess
          rd.accept(')')
          result
        case _ =>
          errorExpected(rd, quoted(label)+"(...)")
      }
  }

  /** Same as `p.wrap(in)(out)`
   */
  def wrappedPickler[S, T](p: Pickler[S])(in: S => T)(out: T => S) = new Pickler[T] {
    def pickle(wr: Writer, x: T) = p.pickle(wr, out(x))
    def unpickle(rd: Lexer) = p.unpickle(rd) map in
  }

  /** Same as `p.cond(condition)`
   */
  def conditionalPickler[T](p: Pickler[T], condition: Any => Boolean) = new CondPickler[T](condition) {
    def pickle(wr: Writer, x: T) = p.pickle(wr, x)
    def unpickle(rd: Lexer) = p.unpickle(rd)
  }

  /** Same as `p ~ q`
   */
  def seqPickler[T, U](p: Pickler[T], q: => Pickler[U]) = new Pickler[T ~ U] {
    lazy val qq = q
    def pickle(wr: Writer, x: T ~ U) = {
      p.pickle(wr, x.fst)
      wr.write(',')
      q.pickle(wr, x.snd)
    }
    def unpickle(rd: Lexer) =
      for (x <- p.unpickle(rd); y <- { rd.accept(','); qq.unpickle(rd).requireSuccess })
      yield x ~ y
  }

  /** Same as `p | q`
   */
  def eitherPickler[T, U <: T, V <: T](p: CondPickler[U], q: => CondPickler[V]) =
    new CondPickler[T](x => p.canPickle(x) || q.canPickle(x)) {
      lazy val qq = q
      override def tryPickle(wr: Writer, x: Any): Boolean =
        p.tryPickle(wr, x) || qq.tryPickle(wr, x)
      def pickle(wr: Writer, x: T) =
        require(tryPickle(wr, x),
                "no pickler found for "+x+" of class "+x.getClass.getName)
      def unpickle(rd: Lexer) = p.unpickle(rd) orElse qq.unpickle(rd)
    }

  /** A conditional pickler for singleton objects. It represents these
   *  with the object's underlying class as a label.
   *  Example: Object scala.None would be represented as `scala.None$()`.
   */
  def singletonPickler[T <: AnyRef](x: T): CondPickler[T] =
    unitPickler
      .wrapped { _ => x } { x => () }
      .labelled (x.getClass.getName)
      .cond (x eq _.asInstanceOf[AnyRef])

  /** A pickler the handles instances of classes that have an empty constructor.
   *  It represents than as `$new ( <name of class> )`.
   *  When unpickling, a new instance of the class is created using the empty
   *  constructor of the class via `Class.forName(<name of class>).newInstance()`.
   */
  def javaInstancePickler[T <: AnyRef]: Pickler[T] =
    (stringPickler labelled "$new")
      .wrapped { name => Class.forName(name).newInstance().asInstanceOf[T] } { _.getClass.getName }

  /** A picklers that handles iterators. It pickles all values
   *  returned by an iterator separated by commas.
   *  When unpickling, it always returns an `UnpickleSuccess` containing an iterator.
   *  This iterator returns 0 or more values that are obtained by unpickling
   *  until a closing parenthesis, bracket or brace or the end of input is encountered.
   *
   *  This means that iterator picklers should not be directly followed by `~`
   *  because the pickler would also read any values belonging to the second
   *  part of the `~`-pair.
   *
   *  What's usually done instead is that the iterator pickler is wrapped and labelled
   *  to handle other kinds of sequences.
   */
  implicit def iterPickler[T: Pickler]: Pickler[Iterator[T]] = new Pickler[Iterator[T]] {
    lazy val p = pkl[T]
    def pickle(wr: Writer, xs: Iterator[T]) {
      var first = true
      for (x <- xs) {
        if (first) first = false else wr.write(',')
        p.pickle(wr, x)
      }
    }
    def unpickle(rd: Lexer): Unpickled[Iterator[T]] = UnpickleSuccess(new Iterator[T] {
      var first = true
      def hasNext = {
        val t = rd.token
        t != EOF && t != RParen && t != RBrace && t != RBracket
      }
      def next(): T = {
        if (first) first = false else rd.accept(',')
        p.unpickle(rd).requireSuccess.result
      }
    })
  }

  /** A pickler that handles values that can be represented as a single token.
   *  @param   kind   the kind of token representing the value, used in error messages
   *                  for unpickling.
   *  @param  matcher A partial function from tokens to handled values. Unpickling
   *                  succeeds if the matcher function is defined on the current token.
   */
  private def tokenPickler[T](kind: String)(matcher: PartialFunction[Token, T]) = new Pickler[T] {
    def pickle(wr: Writer, x: T) = wr.write(x.toString)
    def unpickle(rd: Lexer) =
      if (matcher isDefinedAt rd.token) nextSuccess(rd, matcher(rd.token))
      else errorExpected(rd, kind)
  }

  /** A pickler for values of type `Long`, represented as integer literals */
  implicit val longPickler: Pickler[Long] =
    tokenPickler("integer literal") { case IntLit(s) => s.toLong }

  /** A pickler for values of type `Int`, represented as integer literals */
  implicit val intPickler: Pickler[Int] = longPickler.wrapped { _.toInt } { _.toLong }

  /** A conditional pickler for the boolean value `true` */
  private val truePickler =
    tokenPickler("boolean literal") { case TrueLit => true } cond { _ == true }

  /** A conditional pickler for the boolean value `false` */
  private val falsePickler =
    tokenPickler("boolean literal") { case FalseLit => false } cond { _ == false }

  /** A pickler for values of type `Boolean`, represented as the literals `true` or `false`. */
  implicit def booleanPickler: Pickler[Boolean] = truePickler | falsePickler

  /** A pickler for values of type `Unit`, represented by the empty character string */
  implicit val unitPickler: Pickler[Unit] = new Pickler[Unit] {
    def pickle(wr: Writer, x: Unit) {}
    def unpickle(rd: Lexer): Unpickled[Unit] = UnpickleSuccess(())
  }

  /** A pickler for values of type `String`, represented as string literals */
  implicit val stringPickler: Pickler[String] = new Pickler[String] {
    def pickle(wr: Writer, x: String) = wr.write(if (x == null) "null" else quoted(x))
    def unpickle(rd: Lexer) = rd.token match {
      case StringLit(s) => nextSuccess(rd, s)
      case NullLit => nextSuccess(rd, null)
      case _ => errorExpected(rd, "string literal")
    }
  }

  /** A pickler for pairs, represented as `~`-pairs */
  implicit def tuple2Pickler[T1: Pickler, T2: Pickler]: Pickler[(T1, T2)] =
    (pkl[T1] ~ pkl[T2])
      .wrapped { case x1 ~ x2 => (x1, x2) } { case (x1, x2) => x1 ~ x2 }
      .labelled ("tuple2")

  /** A pickler for 3-tuples, represented as `~`-tuples */
  implicit def tuple3Pickler[T1, T2, T3](implicit p1: Pickler[T1], p2: Pickler[T2], p3: Pickler[T3]): Pickler[(T1, T2, T3)] =
    (p1 ~ p2 ~ p3)
      .wrapped { case x1 ~ x2 ~ x3 => (x1, x2, x3) } { case (x1, x2, x3) => x1 ~ x2 ~ x3 }
      .labelled ("tuple3")

  /** A pickler for list values */
  implicit def listPickler[T: Pickler]: Pickler[List[T]] =
    iterPickler[T] .wrapped { _.toList } { _.iterator } .labelled ("scala.List")
}

/** A subclass of Pickler can indicate whether a particular value can be pickled by instances
 *  of this class.
 *  @param canPickle   The predicate that indicates whether a given value
 *                     can be pickled by instances of this class.
 */
abstract class CondPickler[T](val canPickle: Any => Boolean) extends Pickler[T] {
  import Pickler._

  /** Pickles given value `x` if possible, as indicated by `canPickle(x)`.
   */
  def tryPickle(wr: Writer, x: Any): Boolean = {
    val result = canPickle(x)
    if (result) pickle(wr, x.asInstanceOf[T])
    result
  }

  /** A pickler obtained from this pickler and an alternative pickler.
   *  To pickle a value, this pickler is tried first. If it cannot handle
   *  the object (as indicated by its `canPickle` test), then the
   *  alternative pickler is tried.
   *  To unpickle a value, this unpickler is tried first. If it cannot read
   *  the input (as indicated by a `UnpickleFailure` result), then the
   *  alternative pickler is tried.
   *  @tparam V    The handled type of the returned pickler.
   *  @tparam U    The handled type of the alternative pickler.
   *  @param that The alternative pickler.
   */
  def | [V >: T, U <: V] (that: => CondPickler[U]): CondPickler[V] =
    eitherPickler[V, T, U](this, that)
}

