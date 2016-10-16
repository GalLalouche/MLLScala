package lr

import cats.Foldable.ToFoldableOps
import cats.instances.TupleInstances
import rich.RichNonEmptyVector._

case class LinearHypothesis(x0: Double, xn: NonEmptyVektor) extends Hypothesis[Double]
    with TupleInstances with ToFoldableOps {
  val arity: Int = xn.size.toInt
  override def apply(v: NonEmptyVektor): Double =
    x0 + xn.zipUnsafe(v).map(e => e._1 * e._2).toVector.sum
  override def toString: String =
    s"$x0 + ${xn.zipWithIndex.map(e => s"${e._1}x${e._2 + 1}").toVector.mkString(" + ")}"
}
