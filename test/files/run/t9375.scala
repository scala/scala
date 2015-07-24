import java.io._

trait SE extends Serializable {
  println("konstruktor: " + this.getClass)
}

class A extends SE {
  object O extends SE
  private[this] object Op extends SE
  def P: Object = Op

  object N extends SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }

  class A extends SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }

  trait T extends SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }
  class C extends T

  def u: Object = {
    object O extends SE
    O
  }

  val v: Object = {
    object O extends SE
    O
  }

  val f: () => Object = () => {
    object O extends SE
    O
  }

  trait GetObj { def O: Object; def P: Object }
  val a: GetObj = new GetObj with SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }
}

trait T extends SE {
  object O extends SE
  private[this] object Op extends SE
  def P: Object = Op

  object N extends SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }

  class A extends SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }

  trait T extends SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }
  class C extends T

  def u: Object = {
    object O extends SE
    O
  }

  val v: Object = {
    object O extends SE
    O
  }

  val f: () => Object = () => {
    object O extends SE
    O
  }

  trait GetObj { def O: Object; def P: Object }
  val a: GetObj = new GetObj with SE {
    object O extends SE
    private[this] object Op extends SE
    def P: Object = Op
  }
}

class C extends T

object Test extends App {
  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }

  val a = new A
  val aa = new a.A
  val ac = new a.C

  val c = new C
  val ca = new c.A
  val cc = new c.C

  val os: List[(Object, Object)] = List(
    a.O   -> a,
    a.P   -> a,
    a.N.O -> a.N,
    a.N.P -> a.N,
    aa.O  -> aa,
    aa.P  -> aa,
    ac.O  -> ac,
    ac.P  -> ac,
    a.u   -> a,
    a.v   -> a,
    a.f() -> a,
    a.a.O -> a.a,
    a.a.P -> a.a,

    c.O   -> c,
    c.P   -> c,
    c.N.O -> c.N,
    c.N.P -> c.N,
    ca.O  -> ca,
    ca.P  -> ca,
    cc.O  -> cc,
    cc.P  -> cc,
    c.u   -> c,
    c.v   -> c,
    c.f() -> c,
    c.a.O -> c.a,
    c.a.P -> c.a
  )

  println("fertig konstrukting")

  for ((obj, outer) <- os) {
    serializeDeserialize(obj);
    serializeDeserialize(outer)
  }
}
