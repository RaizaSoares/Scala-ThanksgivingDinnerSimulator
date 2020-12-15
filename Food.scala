/*
@class - food
 */

package soares_raiza
import scala.collection.mutable
import scala.xml._


class Food (n : String="", w : Double= 1.0) extends RDP with CoR {
  private var name : String = n
  private var weight : Double = w

  override def add(): Unit = {
    name = scala.io.StdIn.readLine("\nName: ")
    weight = scala.io.StdIn.readLine("Weight: ").toDouble
  }
  def getWeight: Double = {return weight}

  /*
  Displays food output
   */
  override def displayString(): String =
    {
      return "-"*6 + s"Name: " +name+ " (" + f"$weight%.1f" +" lbs)\n"
    }
  /*
     @function: XML read reads in data from
     a given XML file.
      */
  override def xmlRead(node: Node): Unit =
    {
      val name1 = node.attribute("name")
      weight = node.text.toDouble
      name = name1.getOrElse("").toString
    }

  override def xmlWrite(): Elem = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", name))
    var w = Text(weight.toString)
    XMLHelper.makeNode("food", attr, w)
  }
/*
If food exists, return true, else false
 */
  override def findFood(n: String): Boolean =
    {
      if(name.toLowerCase() == n.toLowerCase())
        {
          return true
        }
        return false

    }

}

object Food{
  val TAG = "food"
  def createFood(n : String, w : Double) = new Food(n,w)
}