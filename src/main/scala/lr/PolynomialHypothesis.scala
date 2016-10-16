package lr

import cats.data.NonEmptyVector
import common.rich.primitives.RichDouble._
import rich.RichNonEmptyVector._

case class Polynome(poly: NonEmptyVector[(Int, Double)]) extends (Double => Double) {
  override def apply(v: Double): Double = poly.map { case (i, x) => x * v.**(i) }.toVector.sum
}
case class PolynomialHypothesis(x0: Double, xn: NonEmptyVector[Polynome]) extends Hypothesis[Double] {
  val arity = xn.length
  override def apply(os: NonEmptyVektor): Double =
    x0 + os.zipUnsafe(xn).map(e => e._2(e._1)).toVector.sum
  override def toString: String = {
    def aux(p: (Polynome, Int)): String =
      p._1.poly.sortBy(_._1).map(e => s"${e._2}*x${p._2}^${e._1}").toVector.mkString(" + ")
    s"$x0 + ${xn.zipWithIndex.map(aux).toVector.mkString("(", "), (", ")")}"
  }
}
