import java.io._

object SerDes {
  def serialize(obj: AnyRef): Array[Byte] = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    buffer.toByteArray
  }

  def deserialize(a: Array[Byte]): AnyRef = {
    val in = new ObjectInputStream(new ByteArrayInputStream(a))
    in.readObject
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = deserialize(serialize(obj)).asInstanceOf[T]
}

import SerDes._

// tests to make sure that de-serializing an object does not run its constructor

trait S extends Serializable {
  println("  konstruktor: " + this.getClass)
}

trait SE extends S {
  def outer: Object
}

class A extends S {
  object O extends SE { def outer = A.this }
  private[this] object Op extends SE { def outer = A.this }
  def P: SE = Op

  object N extends S {
    object O extends SE { def outer = N }
    private[this] object Op extends SE { def outer = N }
    def P: SE = Op
  }

  class A extends S {
    object O extends SE { def outer = A.this }
    private[this] object Op extends SE { def outer = A.this }
    def P: SE = Op
  }

  trait T extends S {
    object O extends SE { def outer = T.this }
    private[this] object Op extends SE { def outer = T.this }
    def P: SE = Op
  }
  class C extends T

  def u: SE = {
    object O extends SE { def outer = A.this }
    O
  }

  val v: SE = {
    object O extends SE { def outer = A.this }
    O
  }

  val f: () => SE = () => {
    object O extends SE { def outer = A.this }
    O
  }

  trait GetObj { def O: SE; def P: SE }
  val a: GetObj = new GetObj with S {
    def anonThis = this
    object O extends SE { def outer = anonThis }
    private[this] object Op extends SE { def outer = anonThis }
    def P: SE = Op
  }
}

trait T extends S {
  object O extends SE { def outer = T.this }
  private[this] object Op extends SE { def outer = T.this }
  def P: SE = Op

  object N extends S {
    object O extends SE { def outer = N }
    private[this] object Op extends SE { def outer = N }
    def P: SE = Op
  }

  class A extends S {
    object O extends SE { def outer = A.this }
    private[this] object Op extends SE { def outer = A.this }
    def P: SE = Op
  }

  trait T extends S {
    object O extends SE { def outer = T.this }
    private[this] object Op extends SE { def outer = T.this }
    def P: SE = Op
  }
  class C extends T

  def u: SE = {
    object O extends SE { def outer = T.this }
    O
  }

  val v: SE = {
    object O extends SE { def outer = T.this }
    O
  }

  val f: () => SE = () => {
    object O extends SE { def outer = T.this }
    O
  }

  trait GetObj { def O: SE; def P: SE }
  val a: GetObj = new GetObj with S {
    def anonThis = this
    object O extends SE { def outer = anonThis }
    private[this] object Op extends SE { def outer = anonThis }
    def P: SE = Op
  }
}

class C extends T

object DeserializeModuleNoConstructor {
  def t(): Unit = {
    val a = new A
    val aa = new a.A
    val ac = new a.C

    val c = new C
    val ca = new c.A
    val cc = new c.C

    val outers: List[Object] = List(
      a, a.N, aa, ac, a.a,
      c, c.N, ca, cc, c.a
    )

    println("serializing outer objects should not initialize any nested objects")

    val serANotInit = serialize(a)
    outers foreach serializeDeserialize

    println("now initializing nested objects")

    val os: List[(SE, Object)] = List(
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

    println("no object konstruktors called when serializing / deserializing objects (starting at the outer or the object itself)")

    for ((obj, outer) <- os) {
      assert(obj.outer eq outer, s"${obj.outer} of $obj -- $outer")
      serializeDeserialize(obj)
      serializeDeserialize(outer)
    }

    println("deserializing outer objects with non-initialized inners again")
    val aNotInit = deserialize(serANotInit).asInstanceOf[A]

    println("accessing modules triggers initialization")
    aNotInit.O
    aNotInit.P
    aNotInit.N.O
    aNotInit.N.P

    println("deserializing creates a new object graph, including new scala 'object' instances, no matter where serialization starts")
    val deserializedAs: List[A] = List(
      serializeDeserialize(a),
      serializeDeserialize(a.O).outer.asInstanceOf[A],
      serializeDeserialize(a.P).outer.asInstanceOf[A],
      serializeDeserialize(a.v).outer.asInstanceOf[A]
    )
    for (aSD <- deserializedAs) {
      assert(aSD ne a)
      assert(aSD.O ne a.O)
      assert(aSD.P ne a.P)
      assert(aSD.N ne a.N)
      assert(aSD.N.O ne a.N.O)
      assert(aSD.N.P ne a.N.P)
      assert(aSD.v ne a.v)
      assert(aSD.a.O ne a.a.O)
      assert(aSD.a.P ne a.a.P)
    }
  }
}

// tests for serializing / deserializing static modules

object M extends S {
  object O extends S

  def u: S = {
    object O extends S
    O
  }

  val v: S = {
    object O extends S
    O
  }

  lazy val w: S = {
    object O extends S
    O
  }

  val f: () => S = () => {
    object O extends S
    O
  }
}

object SerializingStaticModules {
  def t(): Unit = {
    println("init static module M and field v")
    M

    println("serDeser does not initialize nested static modules")
    assert(serializeDeserialize(M) eq M)

    println("init M.O")
    M.O

    println("serDeser nested static module")
    assert(serializeDeserialize(M.O) eq M.O)

    println("objects declared in field decls are not static modules, so they deserialize to new instances")
    assert(serializeDeserialize(M.v) ne M.v)

    println("init lazy val M.w")

    println("objects declared in lazy val are not static modules either")
    assert(serializeDeserialize(M.w) ne M.w)

    println("object declared in a function: new instance created on each invocation")
    assert(M.f() ne M.f())
  }
}


object Test extends App {
  DeserializeModuleNoConstructor.t()
  SerializingStaticModules.t()
}
