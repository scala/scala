// A SAM method with a primitive result that overrides a generic one needs a bridge that boxes the result.
// LambdaMetaFactory boxes as Scala does, so these lambdas use it. It cannot bridge a `void` result or box a
// value class, so those lambdas are expanded to anonymous classes.

import java.io._

trait Get[A] { def get(): A }
trait GetInt extends Get[Int] with Serializable { def get(): Int }
trait GetChar extends Get[Char] { def get(): Char }
trait GetBoolean extends Get[Boolean] { def get(): Boolean }
trait GetDouble extends Get[Double] { def get(): Double }
trait GetUnit extends Get[Unit] { def get(): Unit }
class V(val x: Int) extends AnyVal
trait GetV extends Get[V] { def get(): V }

class Holder { // not Serializable
  def int: GetInt = () => 42
  def char: GetChar = () => 'c'
  def boolean: GetBoolean = () => true
  def double: GetDouble = () => 1.5
  def unit: GetUnit = () => ()
  def v: GetV = () => new V(7)
}

object Test {
  def roundTrip[T](o: T): T = {
    val bo = new ByteArrayOutputStream
    val oo = new ObjectOutputStream(bo)
    oo.writeObject(o)
    oo.close()
    new ObjectInputStream(new ByteArrayInputStream(bo.toByteArray)).readObject().asInstanceOf[T]
  }

  def check(name: String, f: Get[_]): Unit =
    println(s"$name: ${f.get()}, LMF: ${f.getClass.isSynthetic}")

  def main(args: Array[String]): Unit = {
    val h = new Holder
    check("int", h.int)
    check("char", h.char)
    check("boolean", h.boolean)
    check("double", h.double)
    check("unit", h.unit)
    check("v", h.v)
    println(s"int serialized: ${roundTrip(h.int).get()}")
  }
}
