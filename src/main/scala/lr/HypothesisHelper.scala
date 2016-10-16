package lr

import cats.Functor.ToFunctorOps
import cats.data.NonEmptyVector
import cats.instances.VectorInstances
import common.rich.RichT._
import common.rich.collections.RichTraversableOnce._
import common.rich.primitives.RichDouble._
import rich.RichNonEmptyVector._

trait HypothesisHelper[O, H <: Hypothesis[O]] {
  def initial: H
  def gradient(o: Observations[O], learningRate: Double, oldT: H): H
  def cost(o: Observation[O], h: Hypothesis[O]): Double
}

object HypothesisHelper extends VectorInstances with ToFunctorOps {
  def BinaryLinear(arities: NonEmptyVector[NonEmptyVector[Int]]) = new HypothesisHelper[Boolean, LogisticHypothesis] {
    val helper = Polynomial(arities)
    override def cost(o: Observation[Boolean], h: Hypothesis[Boolean]): Double =
      h(o.xn).mapTo(v => -Math.log(if (o.y) v else 1 - v))
    override def initial: LogisticHypothesis = helper.initial |> LogisticHypothesis
    override def gradient(o: Observations[Boolean], learningRate: Double, oldT: LogisticHypothesis): LogisticHypothesis = {
      def toDouble(o: Observation[Boolean]): Observation[Double] = Observation(if (o.y) 1 else 0, o.xn)
      HypothesisHelper.gradient(o.os map toDouble mapTo Observations.apply, learningRate, oldT.h, oldT) |> LogisticHypothesis.apply
    }
  }

  def Linear(arity: Int) = new HypothesisHelper[Double, LinearHypothesis] {
    override def cost(o: Observation[Double], h: Hypothesis[Double]): Double = HypothesisHelper.cost(o, h)
    override val initial = LinearHypothesis(0, NonEmptyVector fromVectorUnsafe Vector.fill(arity)(0))
    override def gradient(o: Observations[Double], learningRate: Double, oldT: LinearHypothesis): LinearHypothesis =
      asPoly(oldT) |> (HypothesisHelper.gradient(o, learningRate, _, oldT)) |> fromPoly
  }

  def Polynomial(arities: NonEmptyVector[NonEmptyVector[Int]]) = new HypothesisHelper[Double, PolynomialHypothesis] {
    override def cost(o: Observation[Double], h: Hypothesis[Double]): Double = HypothesisHelper.cost(o, h)
    override val initial = PolynomialHypothesis(0, arities.map(_.fproduct(0.0.const) |> Polynome))
    override def gradient(o: Observations[Double], learningRate: Double, oldT: PolynomialHypothesis): PolynomialHypothesis =
      HypothesisHelper.gradient(o, learningRate, oldT, oldT)
  }

  /* Utils */
  private def asPoly(lh: LinearHypothesis): PolynomialHypothesis =
    PolynomialHypothesis(lh.x0, lh.xn.map(e => 1 -> e).map(_ |> (NonEmptyVector.of(_)) |> Polynome))
  private def fromPoly(ph: PolynomialHypothesis): LinearHypothesis =
    LinearHypothesis(ph.x0, ph.xn.map(_.poly.toVector.single.ensuring(_._1 == 1)._2))

  private def gradient(o: Observations[Double], learningRate: Double, existingHypo: PolynomialHypothesis, calc: NonEmptyVektor => Double): PolynomialHypothesis = {
    def hypoDiff(o: Observation[Double]): Double = o.y - calc(o.xn)
    val x0Gradient = o.os.map(hypoDiff).toVector.sum * learningRate
    def xnGradient(e: (Polynome, Int)): Polynome = {
      val (p, featureIndex) = e
      val gradients = p.poly.map { case (i, x) =>
        o.os.map(e => (e.xn.getUnsafe(featureIndex) ** i) * hypoDiff(e)).toVector.sum * learningRate
      }
      Polynome(p.poly.zipUnsafe(gradients).map { case ((i, x), gradient) => i -> (x + gradient) })
    }
    PolynomialHypothesis(existingHypo.x0 + x0Gradient, existingHypo.xn.zipWithIndex map xnGradient)
  }
  private def cost(o: Observation[Double], h: Hypothesis[Double]): Double = (o.y - h(o.xn)).sq
}
