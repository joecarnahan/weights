/**
 * Code for representing a set of weights.
 *
 * @author Joe Carnahan
 */

package weights

import scala.math.abs
import scala.math.abs

/**
 * Represents a single bar configuration.
 *
 * @param plates
 *          The set of weights on the bar
 */
private class Configuration(plates: List[Double]) {

  def bar: Double = 45.0
  def weight: Double = bar + (2.0 * plates.sum)
  def numPlates: Int = plates.length

  def add(plate: Double): Configuration = 
    new Configuration(plate :: plates)

  override def toString: String = weight.toString + " (" + 
    plates.toIndexedSeq.sorted.mkString(", ") + ")"

  // Bar equality is determined by weight
  override def hashCode: Int = weight.hashCode
  override def equals(that: Any): Boolean = 
    (that.isInstanceOf[Configuration] &&
     that.asInstanceOf[Configuration].weight == weight)
}

/**
 * Ordering for bar configurations, which sorts by weight and breaks dies by
 * the number of plates.  Note that this is conspicuously inconsistent with
 * hashCode() and equals().
 *
 * Also includes an apply() method for easier configuration construction.
 */
private object Configuration extends Ordering[Configuration] {
  def compare(first: Configuration, second: Configuration): Int =
    if (first.weight == second.weight)
      first.numPlates.compareTo(second.numPlates)
    else
      first.weight.compareTo(second.weight)

  def apply(plates: List[Double]) = new Configuration(plates)
}

/**
 * Constants for the Weights class.
 */
private object Weights {
  val minStep = 2.0
}

/**
 * Represents a set of weights.
 *
 * @param plates
 *          A list of all of the plate sizes that you have. We assume that you
 *          have two weights of each size. To indicate that you have four or six
 *          plates of a given size, you can list the same weight size twice or
 *          three times in the list.
 */
class Weights(plates: Traversable[String]) {

  private def plateWeights: Traversable[Double] = plates.map(_.toDouble)

  private def addConfiguration: 
    (Vector[Configuration], Double) => Vector[Configuration] =
    ((v: Vector[Configuration], d: Double) => v ++ v.map(_.add(d)))

  private def configurations: Traversable[Configuration] =
    plateWeights.foldLeft(Vector(Configuration(Nil)))(addConfiguration)

  private def sortedConfigurations: IndexedSeq[Configuration] =
    configurations.toIndexedSeq.sorted(Configuration)

  /**
   * Returns a set of unique weight configurations that have the minimal number
   * of plates and whose weights are all at least <code>Weights.minStep</code>
   * apart.
   */
  private def wellSpacedConfigurations: IndexedSeq[Configuration] = {

    /**
     * Represents an individual weight configuration as we examine the list of
     * configurations to remove unnecessary ones.
     */
    case class Spacing(config: Configuration,  // weight configuration
                       index: Int,             // index in original list
                       before: Double,         // Difference between this
                                               //   weight and the one after it
                       after: Double,          // Difference between this
                                               //   weight and the one after it
                       gap: Double,            // Gap that would exist if this
                                               //   weight were not included
                       included: Boolean,      // true iff still included
                       changed: Boolean)       // true iff this spacing was
                                               //   modified in the latest pass

    /**
     * Given a configuration and its index in the sequence of configurations,
     * builds a Spacing object with the default initial values.
     */
    def buildSpacing(configAndIndex: (Configuration, Int)): Spacing =
      Spacing(configAndIndex._1, configAndIndex._2, 0.0, 0.0, 0.0, true, false)

    /**
     * Given a sequence of weight configurations, builds the initial sequence
     * of Spacing objects.
     */
    def buildSpacings(configurations: IndexedSeq[Configuration]): 
        IndexedSeq[Spacing] = configurations.zipWithIndex.map(buildSpacing)

    /**
     * Given a function that determines which indices to examine, start from
     * the current weight and return the absolute difference between it and
     * the first included weight that is returned by the given function.
     * Note that if the given function returns None, then the distance is
     * considered to be infinite.
     */
    def updateSpacing(spacings: IndexedSeq[Spacing], 
                      thisSpacing: Spacing,
                      moveIndex: Int => Option[Int]): Double = {
      def updateSpacingRec(currIndex: Int): Double =
        moveIndex(currIndex) match {
          case None => Double.PositiveInfinity
          case Some(i) =>
            if (spacings(i).included)
              abs(spacings(i).config.weight - thisSpacing.config.weight)
            else
              updateSpacingRec(i)
        }
      updateSpacingRec(thisSpacing.index)
    }

    /**
     * Given a spacing and the sequence of spacings, return a new spacing with
     * the "before," "after", and "gap" fields correctly populated.
     * Non-included weights ignored when calculating spacings and are not
     * modified by this method.
     * <p>
     * This could probably be improved by using the fact that each included
     * weight's "before" is the same as the previous included weight's "after",
     * but it doesn't seem like it's worth the trouble right now.
     */
    def updateSpacings(spacings: IndexedSeq[Spacing], 
                       thisSpacing: Spacing): Spacing =
      if (thisSpacing.included) {
        val newBefore = updateSpacing(spacings, thisSpacing, 
            {(i: Int) => if (i == 0) None else Some(i - 1)})
        val newAfter = updateSpacing(spacings, thisSpacing, 
            {(i: Int) => if (i == (spacings.size - 1)) None else Some(i + 1)})
        Spacing(thisSpacing.config, thisSpacing.index,
                newBefore, newAfter, newBefore + newAfter,
                thisSpacing.included,
                thisSpacing.changed || 
                  (thisSpacing.before != newBefore) || 
                  (thisSpacing.after != newAfter))
      }
      else
        thisSpacing
              
    /**
     * Returns a new list of configurations that may or may not contain some
     * additional combinations marked as no longer included.
     * <p>
     * Specifically, this uses a greedy algorithm.  If any weights are less
     * than the minimum distance apart, this removes the weight that will
     * leave the smallest gap of any weight that can be removed.
     */
    def updateIncluded(spacings: IndexedSeq[Spacing]): IndexedSeq[Spacing] = {
      
      /**
       * Orders spacings by the sizes of the gaps they would leave.
       */
      object OrderByGap extends Ordering[Spacing] {
        def compare(first: Spacing, second: Spacing): Int =
          if (first.gap == second.gap)
            first.config.numPlates.compareTo(second.config.numPlates) * -1
          else
            first.gap.compare(second.gap)
      }

      if (spacings.filter(_.included).exists(_.before < Weights.minStep)) {
        val smallestGap = spacings.filter(_.included).min(OrderByGap)
        spacings.updated(smallestGap.index,
          Spacing(smallestGap.config, smallestGap.index,
                  smallestGap.before, smallestGap.after, smallestGap.gap,
                  false, true))
      }
      else
        spacings
    }
    
    /**
     * Returns a new list of configurations with all of the "changed" flags
     * reset to <code>false</code>.
     */
    def clearChanged(spacings: IndexedSeq[Spacing]): IndexedSeq[Spacing] =
      spacings.map((s: Spacing) =>
        Spacing(s.config, s.index, s.before, s.after, s.gap, s.included, false))
    
    /**
     * Repeatedly marks extra weight combinations as not included and updates
     * the inter-weight spacings until no unnecessary weight combinations
     * remain.
     */
    def removeExtraSpacings(spacings: IndexedSeq[Spacing]):
        IndexedSeq[Spacing] = {
      val updated = updateIncluded(spacings.map(updateSpacings(spacings, _)))
      if (updated.exists(_.changed))
        removeExtraSpacings(clearChanged(updated))
      else
        updated
    } 

    removeExtraSpacings(buildSpacings(sortedConfigurations)).filter(_.included).
        map(_.config)
  }

  def calculatePossibilities: String = 
    wellSpacedConfigurations.mkString(System.getProperty("line.separator"))

}
