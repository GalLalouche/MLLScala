package lr

import scala.annotation.tailrec

trait GradientDescentSolver[O] {
  private val maxTries = 10000
  def apply[H <: Hypothesis[O]](o: Observations[O])(implicit helper: HypothesisHelper[O, H]): H = {
    def cost(os: Observations[O], h: Hypothesis[O]): Double = os.os.map(helper.cost(_, h)).toVector.sum
    @tailrec
    def aux(alpha: Double, oldHypothesis: H, n: Int): H = {
      if (n > maxTries)
        return oldHypothesis
      val (nextAlpha, nextHypo) = {
        val newHypothesis = helper.gradient(o, alpha, oldHypothesis)
        if (cost(o, oldHypothesis) > cost(o, newHypothesis))
          alpha * 2 -> newHypothesis
        else
          alpha / 2 -> oldHypothesis
      }
      aux(nextAlpha, nextHypo, n + 1)
    }
    aux(1, helper.initial, 0)
  }
}
