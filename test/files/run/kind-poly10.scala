import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{ GenMap, GenTraversable }

/** Draft implmementation of Polykinded Safe Type Representation & Cast */
object Test extends App {

  /** Representation of the type of a value
    * T is the type constructor of the type of the value (For example Int => T = Int, List[Int] => T = List)
    * TAbs (for T abstracted) is a well-formed monomorphic type derived from T (For example List => TAbs = List[Any])
    * To think about: variance could be taken into account there
    */
  trait TypeRepr[T <: AnyKind, TAbs] {
    // the value associated to this type representation
    def value: TAbs

    // cast local value to the monomorphic type TT
    def cast[TT](implicit typeable: Typeable[TT]): Option[typeable.TAbs] = typeable.repr(value).map(_.value)

    def describe: String
    override def toString = s"TypeRepr[$describe]"
  }

  /** Kind-Polymorphic Typeable typeclass
    */
  trait Typeable[T <: AnyKind] extends Serializable {
    type TAbs

    // returns an eventual type representation associated the given value
    def repr(t: Any): Option[TypeRepr[T, TAbs]]

    def describe: String
    override def toString = s"Typeable[$describe]"
  }

  object Typeable {
    import java.{ lang => jl }

    def apply[T <: AnyKind](implicit st: Typeable[T]): st.type = st
    
    case class ValueTypeable[T, B](cB: Class[B], describe: String) extends Typeable[T] {
      self =>
      type TAbs = T
      def repr(t: Any): Option[TypeRepr[T, T]] = {
        if(t != null && cB.isInstance(t))
          Some(new TypeRepr[T, T] {
            val value = t.asInstanceOf[T]
            val describe = self.describe
          })
        else None
      }
    }

    /** Typeable instance for `String`. */
    implicit val stringTypeable: Typeable[String] = ValueTypeable[String, String](classOf[String], "String")
    /** Typeable instance for `Byte`. */
    implicit val byteTypeable: Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte], "Byte")
    /** Typeable instance for `Short`. */
    implicit val shortTypeable: Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short], "Short")
    /** Typeable instance for `Char`. */
    implicit val charTypeable: Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character], "Char")
    /** Typeable instance for `Int`. */
    implicit val intTypeable: Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer], "Int")
    /** Typeable instance for `Long`. */
    implicit val longTypeable: Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long], "Long")
    /** Typeable instance for `Float`. */
    implicit val floatTypeable: Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float], "Float")
    /** Typeable instance for `Double`. */
    implicit val doubleTypeable: Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double], "Double")
    /** Typeable instance for `Boolean`. */
    implicit val booleanTypeable: Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean], "Boolean")
    /** Typeable instance for `Unit`. */
    implicit val unitTypeable: Typeable[Unit] = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit], "Unit")

    /** Typeable instance for `Any`. */
    implicit val anyTypeable: Typeable[Any] =
      new Typeable[Any] {
        type TAbs = Any
        def repr(t: Any): Option[TypeRepr[Any, Any]] = Some(new TypeRepr[Any, Any] {
          val value = t
          val describe = "Any"
        })
        val describe = "Any"
      }

    // We coulld take variance into to use Any or Nothing in TAbs
    implicit def typeable1A[F[_]](implicit mF: ClassTag[F[_]]) =
      new Typeable[F] {
        type TAbs = F[Any]
        def repr(t: Any): Option[TypeRepr[F, F[Any]]] =
          if(t == null) None
          else if(mF.runtimeClass isAssignableFrom t.getClass) {
            Some(new TypeRepr[F, F[Any]] {
              val value = t.asInstanceOf[F[Any]]
              val describe = s"${mF.runtimeClass.getSimpleName}"
            })
          } else None

        val describe = s"${mF.runtimeClass.getSimpleName}"
      }

    implicit def typeable2AB[F[_, _]](implicit mF: ClassTag[F[_, _]]) =
      new Typeable[F] {
        type TAbs = F[Any, Any]
        def repr(t: Any): Option[TypeRepr[F, F[Any, Any]]] =
          if(t == null) None
          else if(mF.runtimeClass isAssignableFrom t.getClass) {
            Some(new TypeRepr[F, F[Any, Any]] {
              val value = t.asInstanceOf[F[Any, Any]]
              val describe = s"${mF.runtimeClass.getSimpleName}"
            })
          } else None

        val describe = s"${mF.runtimeClass.getSimpleName}"
      }

    implicit def genTraversableTypeable[CC[X] <: GenTraversable[X], T]
      (implicit mCC: ClassTag[CC[_]], castT: Typeable[T]): Typeable[CC[T] with GenTraversable[T]] =
        new Typeable[CC[T]] {
          type TAbs = CC[T]
          def repr(t: Any): Option[TypeRepr[CC[T], CC[T]]] =
            if(t == null) None
            else if(mCC.runtimeClass isAssignableFrom t.getClass) {
              val cc = t.asInstanceOf[CC[Any]]
              if(cc.forall(x => castT.repr(x).isDefined)) Some(new TypeRepr[CC[T], CC[T]] {
                val value = t.asInstanceOf[CC[T]]
                val describe = s"${mCC.runtimeClass.getSimpleName}[${castT.describe}]"
              })
              else None
            } else None
          val describe = s"${mCC.runtimeClass.getSimpleName}[${castT.describe}]"
        }

    /** Typeable instance for `Map`. Note that the contents will be tested for conformance to the key/value types. */
    implicit def genMapTypeable[M[X, Y], K, V]
      (implicit ev: M[K, V] <:< GenMap[K, V], mM: ClassTag[M[_, _]], castK: Typeable[K], castV: Typeable[V]): Typeable[M[K, V]] =
      new Typeable[M[K, V]] {
        type TAbs = M[K, V]
        def repr(t: Any): Option[TypeRepr[M[K, V], M[K, V]]] =
          if(t == null) None
          else if(mM.runtimeClass isAssignableFrom t.getClass) {
            val m = t.asInstanceOf[GenMap[Any, Any]]
            if(m.forall(x => castK.repr(x._1).isDefined && castV.repr(x._2).isDefined)) Some(new TypeRepr[M[K, V], M[K, V]] {
              val value = t.asInstanceOf[M[K, V]]
              val describe = s"${mM.runtimeClass.getSimpleName}[${castK.describe}, ${castV.describe}]"
            })
            else None
          } else None
        val describe = s"${mM.runtimeClass.getSimpleName}[${castK.describe}, ${castV.describe}]"
      }

  }

  /** Builds eventual type representation of a value
    * For ex:
    * repr[Int](5) => Some(TypeRepr[Int])
    * repr[List](List(1, 2, 3) => Some(TypeRepr[List])
    * repr[String](5) => None    
    */
  def repr[T <: AnyKind](t: Any)(implicit typeable: Typeable[T]): Option[TypeRepr[T, typeable.TAbs]] = typeable.repr(t)
  
  /** Safe-casts a monomorphic-typed value using implicit typeable
    * cast[Int](5) => Some(5)
    * cast[String](5) => None
    */
  def cast[T](t: Any)(implicit typeable: Typeable[T]): Option[typeable.TAbs] = typeable.repr(t).flatMap(_.cast[T])

  assert(repr[Int](5).isDefined)
  assert(cast[Int](5) == Some(5))

  assert(repr[String](5) == None)
  assert(repr[String]("5").isDefined)
  assert(cast[String]("5") == Some("5"))

  // List
  assert(repr[List](List(1, 2, 3)).isDefined)
  assert(cast[List[Int]](List(1, 2, 3)) == Some(List(1, 2, 3)))
  assert(repr[List[String]](List(1, 2, 3)) == None)
  assert(repr[List](List(1, "tutu", 3L)).flatMap(_.cast[List[Any]]) == Some(List(1, "tutu", 3L)))
  assert(cast[List[Any]](List(1, "tutu", 3L)) == Some(List(1, "tutu", 3L)))

  // Map
  assert(repr[Map](Map(1 -> "toto", 2 -> "tata", 3 -> "tutu")).isDefined)  
  assert(repr[Map](Map(1 -> "toto", 2 -> "tata", 3 -> "tutu")).flatMap(_.cast[Map[Int, String]]) == Some(Map(1 -> "toto", 2 -> "tata", 3 -> "tutu")))
  assert(repr[List](Map(1 -> "toto", 2 -> "tata", 3 -> "tutu")) == None)
  assert(cast[Map[Int, String]](Map(1 -> "toto", 2 -> "tata", 3 -> "tutu")) == Some(Map(1 -> "toto", 2 -> "tata", 3 -> "tutu")))
}




