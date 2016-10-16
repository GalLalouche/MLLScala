package lr

trait Hypothesis[T] extends (NonEmptyVektor => Double) {
  val arity: Int
}
