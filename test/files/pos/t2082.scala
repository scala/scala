
trait Mapper[T <: Mapper[T]]

trait KeyedMapper[KeyType, T <: KeyedMapper[KeyType, T]] extends Mapper[T]


trait KeyedMetaMapper[KeyType, T <: KeyedMapper[KeyType, T]]

trait MappedForeignKey[KeyType, Owner <: Mapper[Owner], Other <: KeyedMapper[KeyType, Other]]

trait IdPK

class TestSubject extends KeyedMapper[Long, TestSubject] with IdPK

class TestRun extends KeyedMapper[Long, TestRun] with IdPK {
	object testSubject extends MappedForeignKey[Long, TestRun, TestSubject]
}

object TestRun extends TestRun with KeyedMetaMapper[Long, TestRun]

class MetaTestSubject extends TestSubject with KeyedMetaMapper[Long, TestSubject]
object TestSubject extends MetaTestSubject

object Main {

  def oneToOneJoin[PType <: KeyedMapper[Long, PType] with IdPK,
                   CType <: KeyedMapper[Long, CType] with IdPK,
                   CMetaType <: CType with KeyedMetaMapper[Long, CType],
                   FKType <: MappedForeignKey[Long, PType, CType]]
  (parents: List[PType], metaMapper: CMetaType, keyGetter: (PType) => FKType ):
  Map[Long, CType] = Map.empty

  def callIt {
    oneToOneJoin[TestRun, TestSubject, MetaTestSubject,
                 MappedForeignKey[Long, TestRun, TestSubject]](
    List(), TestSubject, (tr: TestRun) => tr.testSubject)
  }

}
