package lr

import cats.data.NonEmptyVector

case class LogisticHypothesis(h: PolynomialHypothesis) extends Hypothesis[Boolean] {
  override val arity: Int = h.arity
  override def apply(xn: NonEmptyVector[Double]): Double = 1 / (1 + Math.exp(-h(xn)))
  override def toString(): String = h.toString
}
