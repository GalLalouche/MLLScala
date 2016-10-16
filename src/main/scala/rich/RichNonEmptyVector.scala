package rich

import cats.data.NonEmptyVector
import common.rich.RichT._

object RichNonEmptyVector {
  implicit class richNonEmptyVector[T]($: NonEmptyVector[T]) {
    // well, at least we get some dependent value type system going on here
    def zipUnsafe[S](other: NonEmptyVector[S]): NonEmptyVector[(T, S)] = {
      require(other.length == $.length)
      $.toVector.zip(other.toVector) |> NonEmptyVector.fromVectorUnsafe
    }
    def zipWithIndex: NonEmptyVector[(T, Int)] = $.toVector.zipWithIndex |> NonEmptyVector.fromVectorUnsafe
    def sortBy[S: Ordering](f: T => S): NonEmptyVector[T] = $.toVector.sortBy(f) |> NonEmptyVector.fromVectorUnsafe
  }
}
