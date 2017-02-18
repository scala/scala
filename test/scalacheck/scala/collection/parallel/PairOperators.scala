package scala.collection.parallel.ops


import scala.collection.parallel._


trait PairOperators[K, V] extends Operators[(K, V)] {
  def koperators: Operators[K]
  def voperators: Operators[V]

  private def zipPredicates(kps: List[K => Boolean], vps: List[V => Boolean]): List[((K, V)) => Boolean] = for {
    (kp, vp) <- koperators.countPredicates zip voperators.countPredicates
  } yield new Function1[(K, V), Boolean] {
    def apply(kv: (K, V)) = kp(kv._1) && vp(kv._2)
  }

  /* operators */

  def reduceOperators = for {
    (kop, vop) <- koperators.reduceOperators zip voperators.reduceOperators
  } yield new Function2[(K, V), (K, V), (K, V)] {
    def apply(kv1: (K, V), kv2: (K, V)) = (kop(kv1._1, kv2._1), vop(kv1._2, kv2._2))
  }

  def countPredicates = zipPredicates(koperators.countPredicates, voperators.countPredicates)

  def forallPredicates = zipPredicates(koperators.forallPredicates, voperators.forallPredicates)

  def existsPredicates = zipPredicates(koperators.existsPredicates, voperators.existsPredicates)

  def findPredicates = zipPredicates(koperators.findPredicates, voperators.findPredicates)

  def mapFunctions = for {
    (km, vm) <- koperators.mapFunctions zip voperators.mapFunctions
  } yield new Function1[(K, V), (K, V)] {
    def apply(kv: (K, V)) = (km(kv._1), vm(kv._2))
  }

  def partialMapFunctions = for {
    (kpm, vpm) <- koperators.partialMapFunctions zip voperators.partialMapFunctions
  } yield new PartialFunction[(K, V), (K, V)] {
    def isDefinedAt(kv: (K, V)) = kpm.isDefinedAt(kv._1) && vpm.isDefinedAt(kv._2)
    def apply(kv: (K, V)) = (kpm(kv._1), vpm(kv._2))
  }

  def flatMapFunctions = for {
    (kfm, vfm) <- koperators.flatMapFunctions zip voperators.flatMapFunctions
  } yield new Function1[(K, V), Traversable[(K, V)]] {
    def apply(kv: (K, V)) = kfm(kv._1).toIterable zip vfm(kv._2).toIterable
  }

  def filterPredicates = zipPredicates(koperators.filterPredicates, voperators.filterPredicates)

  def filterNotPredicates = filterPredicates

  def partitionPredicates = filterPredicates

  def takeWhilePredicates = zipPredicates(koperators.takeWhilePredicates, voperators.takeWhilePredicates)

  def dropWhilePredicates = takeWhilePredicates

  def spanPredicates = takeWhilePredicates

  def foldArguments = for {
    ((kinit, kop), (vinit, vop)) <- koperators.foldArguments zip voperators.foldArguments
  } yield ((kinit, vinit), new Function2[(K, V), (K, V), (K, V)] {
    def apply(kv1: (K, V), kv2: (K, V)) = (kop(kv1._1, kv2._1), vop(kv1._2, kv2._2))
  })

  def addAllTraversables = for {
    (kt, vt) <- koperators.addAllTraversables zip voperators.addAllTraversables
  } yield kt.toIterable zip vt.toIterable

  def newArray(sz: Int) = new Array[(K, V)](sz)

  def groupByFunctions = (koperators.groupByFunctions zip voperators.groupByFunctions) map {
    opt => { (p: (K, V)) => (opt._1(p._1), opt._2(p._2)) }
  }

}





















