package tastytest

/** contains transitive erasure of opaque types: Entity -> Id -> Long */
object EntityDb {
  import Ids._, Entities._

  object Ids {
    opaque type Id = Long

    object Id {

      private var _nextId = 0L

      def nextId: Id = {
        val id = _nextId
        _nextId += 1
        id
      }

      final implicit class IdOps(val id: Id) extends AnyVal {
        def toLong: Long = id
      }
    }
  }

  object Entities {
    opaque type Entity[+T] = Id

    object Entity {
      def ofKind[T <: Singleton](id: Id)(kind: T): Entity[kind.type] = id

      final implicit class EntityOps[T](val entity: Entity[T]) extends AnyVal {
        def id: Id = entity
      }
    }
  }

  object Data {

    opaque type Person = Kind.OfPerson.type

    private enum Kind { case OfPerson }

    def newPerson(id: Id)(name: String, age: Int): Entity[Person] =
      personName(id.toLong) = name
      personAge(id.toLong)  = age
      Entity.ofKind(id)(Kind.OfPerson)

    final implicit class PersonOps(val person: Entity[Person]) extends AnyVal {
      def name: String = personName(person.id.toLong)
      def age: Int = personAge(person.id.toLong)
    }

    private val personName = collection.mutable.LongMap.empty[String]
    private val personAge = collection.mutable.LongMap.empty[Int]

  }

}
