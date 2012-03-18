/**
 * Main program for computing what weights can be assembled from a given set
 * of plates.
 *
 * @author Joe Carnahan
 */

package weights

object Main {
  def main(args: Array[String]) = 
    println((new Weights(args)).calculatePossibilities)
}
