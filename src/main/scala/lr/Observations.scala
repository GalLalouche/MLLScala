package lr

import cats.data.NonEmptyVector
import common.rich.collections.RichTraversableOnce._

case class Observations[T](os: NonEmptyVector[Observation[T]]) {
  require(os.toVector.hasSameValues(_.arity))
  val arity = os.head.arity
}
