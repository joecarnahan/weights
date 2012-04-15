/**
 * Code for representing a set of weights.
 *
 * @author Joe Carnahan
 */

package weights

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
    return (that.isInstanceOf[Configuration] &&
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
      return first.numPlates.compareTo(second.numPlates)
    else
      return first.weight.compareTo(second.weight)

  def apply(plates: List[Double]) = new Configuration(plates)
}

/**
 * Constants for the Weights class.
 */
private object Weights {
  val minStep = 2.0
  val maxStep = 4.0
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

  private def uniqueConfigurations: IndexedSeq[Configuration] =
    configurations.groupBy(_.weight).
      map(_._2.toIndexedSeq.sorted(Configuration).head).toIndexedSeq.
      sorted(Configuration)

  private def wellSpacedConfigurations: IndexedSeq[Configuration] = {

    /**
     * Represents an individual weight configuration as we examine the list of
     * configurations to remove unnecessary ones.
     */
    case class Spacing(config: Configuration,  // weight configuration
                       index: Int,             // index in original list
                       before: Double,         // differences between this
                       after: Double,          //   weight and the nearest
                                               //   included weights
                       included: Boolean,      // true iff still included
                       changed: Boolean)       // true iff spacings were
                                               //   modified in the latest pass

    /**
     * Given a configuration and its index in the sequence of configurations,
     * builds a Spacing object with the default initial values.
     */
    def buildSpacing(configAndIndex: (Configuration, Int)): Spacing =
      Spacing(configAndIndex._1, configAndIndex._2, 0.0, 0.0, true, false)

    /**
     * Given a sequence of weight configurations, builds the initial sequence
     * of Spacing objects.
     */
    def buildSpacings(configurations: IndexedSeq[Configuration]): 
        IndexedSeq[Spacing] = configurations.zipWithIndex.map(buildSpacing)

    /**
     * Returns a new list of configurations with all of the inter-weight
     * spacings correctly computed.  Any configurations that are not "included"
     * are skipped when determining inter-weight spacings.
     */
    def updateSpacings(spacings: IndexedSeq[Spacing]): IndexedSeq[Spacing] =
      sys.error("TODO: Implement this")

    /**
     * Returns a new list of configurations with all of the unnecessary weight
     * combinations marked as no longer included.
     */
    def updateIncluded(spacings: IndexedSeq[Spacing]): IndexedSeq[Spacing] =
      sys.error("TODO: Implement this")
    
    /**
     * Returns a new list of configurations with all of the "changed" flags
     * reset to <code>false</code>.
     */
    def clearChanged(spacings: IndexedSeq[Spacing]): IndexedSeq[Spacing] =
      spacings.map((s: Spacing) =>
        Spacing(s.config, s.index, s.before, s.after, s.included, false))
    
    /**
     * Repeatedly marks extra weight combinations as not included and updates
     * the inter-weight spacings until no unnecessary weight combinations
     * remain.
     */
    def removeExtraConfigs(spacings: IndexedSeq[Spacing]):
        IndexedSeq[Spacing] = {
      val updated = updateIncluded(updateSpacings(spacings))
      if (updated.exists(_.changed))
        removeExtraConfigs(clearChanged(updated))
      else
        updated
    } 

    removeExtraConfigs(buildSpacings(uniqueConfigurations)).filter(_.included).
        map(_.config)
  }

  def calculatePossibilities: String = 
    uniqueConfigurations.mkString(System.getProperty("line.separator"))

}
