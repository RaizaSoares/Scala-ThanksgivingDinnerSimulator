/*
class - stay
 */

package soares_raiza
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node}
class Stay (trav : Boolean= false, d : Int= 1) extends RDP{
  private var travel : Boolean = trav
  private var inp: String = ""
  private var inpDouble: Double = 0.0
  private var inpInt: Int = 0
  private var days : Int = d
  def giveStay : Int = {return days}
  def yesorno : String =
    {
      if(travel)
        return "yes"
      else
        return "no"
    }
/*
Adds stay for each person
 */
  override def add(): Unit = {
    inp = scala.io.StdIn.readLine("\nAdd stay y/n?")
    if (inp.toLowerCase() == "y") {
      inp = scala.io.StdIn.readLine("\nUpdate traveling: ")
      if (inp.toLowerCase() == "yes") {
        travel = true
      }

      inpInt = scala.io.StdIn.readLine("Update days:").toInt
      days = inpInt
    }
  }
/*
String for stay result
 */
  override def displayString(): String =
    {
      var personvar2 = "-" * 4 + "Stay:\n" + "-" * 6 + "Traveling: "
      if (travel) {
        personvar2 = personvar2 + s"yes Days: $days\n"
      }
      else {
        personvar2 = personvar2 + s"no Days: $days\n"
      }
      return personvar2
    }
  /*
     @function: XML read reads in data from
     a given XML file.
      */
  override def xmlRead(node: Node): Unit = {
    if(node.attribute("traveling").getOrElse("no").toString.toLowerCase().equals("yes")) {
      travel = true
    }
    else {
      travel = false;
    }
      days = node.attribute("days").getOrElse("1").toString.toInt
  }

  override def xmlWrite(): Elem = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("days", days.toString), ("traveling", yesorno))

    XMLHelper.makeNode("stay", attr)
  }
}

object Stay {
  def createStay(trav : Boolean, d : Int) = new Stay(trav, d)
}
