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

  def add(plate: Double): Configuration = 
    new Configuration(plate :: plates)

  override def toString: String = weight.toString + " (" + 
    plates.toIndexedSeq.sorted.mkString(", ") + ")"

  // Bar equality is determined by 
  override def hashCode: Int = weight.hashCode
  override def equals(that: Any): Boolean = 
    return (that.isInstanceOf[Configuration] &&
            that.asInstanceOf[Configuration].weight == weight)
}

/**
 * Ordering for bar configurations, which sorts by weight.
 */
private object Configuration extends Ordering[Configuration] {
  def compare(first: Configuration, second: Configuration): Int =
    return first.weight.compareTo(second.weight)
    // TODO Break ties by number of plates
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

  private def combinations: IndexedSeq[Configuration] =
    (plateWeights.foldLeft(Vector(new Configuration(Nil)))(addConfiguration)
      .toSet.toIndexedSeq.sorted(Configuration))

  def calculatePossibilities: String = 
    combinations.mkString(System.getProperty("line.separator"))

}
