abstract class BulkSearch {
       type R   <: Row
       type Rel <: Relation [R]
       type Corr <: Correspondence[R]

       def searchFor(input: Rel): Mapping[Corr] = { println("BulkSearch.searchFor called.") ; null }
}

object BulkSearchInstance extends BulkSearch {
       type R   = UpRow
       type Rel = UpRelation
       type Corr = UpCorrespondence
}

class Row
class UpRow extends Row

class Relation [R <: Row]
class UpRelation extends Relation [UpRow]

class Correspondence [R <: Row]
class UpCorrespondence extends Correspondence [UpRow]

class Mapping[MC <: Correspondence[_]]
