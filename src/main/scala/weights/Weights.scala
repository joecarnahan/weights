/**
 * Code for representing a set of weights.
 *
 * @author Joe Carnahan
 */

package weights

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

  def plateWeights: Traversable[Double] = plates.map(_.toDouble * 2)

  def bar: Double = 45.0

  def addCombination: (Vector[Vector[Double]], Double) => Vector[Vector[Double]] =
    ((v: Vector[Vector[Double]], d: Double) => v ++ v.map(_ :+ d))

  def combinations: Vector[Vector[Double]] = 
    plateWeights.foldLeft(Vector(Vector[Double]()))(addCombination)

  def possibleWeights: Seq[Double] = 
    combinations.map(_.sum + bar).toSet.toSeq.sorted

  def calculatePossibilities: String = 
    possibleWeights.mkString(System.getProperty("line.separator"))

}
