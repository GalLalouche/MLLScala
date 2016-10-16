package lr

case class Observation[@specialized(Double, Boolean) T](y: T, xn: NonEmptyVektor) {
  val arity = xn.length
}
