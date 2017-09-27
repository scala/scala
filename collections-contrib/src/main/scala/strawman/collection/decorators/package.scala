package strawman.collection

import scala.language.implicitConversions

package object decorators {

  implicit def MapDecorator[K, V](map: Map[K, V]): MapDecorator[K, V] { val `this`: map.type } =
    new MapDecorator[K, V] { val `this`: map.type = map }

}
