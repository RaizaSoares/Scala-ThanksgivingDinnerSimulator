/*
@class Person - person class
 */

package soares_raiza
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml._

class Person (n : String="", hung : Int=1) extends RDP with CoR {
  private var name: String = n
  private var inp: String = ""
  private var inpDouble: Double = 0.0
  private var inpInt: Int = 0
  private var hunger: Int = hung
  private var ListFood = new ListBuffer[Food]()
  private var myStay: Stay = new Stay(false, 1)


  def getName: String = {
    return name
  }
/*
@function Adds stay and foods while the user gives foods to add
 */
  override def add(): Unit = {
    hunger = scala.io.StdIn.readLine("\nAdd hunger: ").toInt
    myStay.add()

    if(scala.io.StdIn.readLine("\nAdd food y/n?").toLowerCase() == "y") {
      var flag: Boolean = true
      while (flag) { //while the user still wants to add food
        val f= new Food()
        f.add()
        ListFood.append(f)
        if(scala.io.StdIn.readLine("\nAdd food y/n?").toLowerCase() != "y")
          {
            flag = false
          }
      }
    }
      return

  }
/*
Displays output
 */
  override def displayString(): String = {
    var personvar = "-" * 2 + s"Name: " + f"$name%-10s" + " Hunger: " + hunger + "\n"
    var personvar2 = myStay.displayString()

    if(ListFood.nonEmpty) {
      personvar2 = personvar2 + "-" * 4 + "Food: \n"
    }
    var new_ListFood = ListFood.map(x => x.displayString())
    return personvar + personvar2 + new_ListFood.mkString("\n")
  }

  /*
     @function: XML read reads in data from
     a given XML file.
      */
  override def xmlRead(node: Node): Unit = {
    val n = node.attribute("name")
    name= n.getOrElse("").toString
    val h = node.attribute("hunger")
    hunger = h.getOrElse("1").toString.toInt
      ListFood = ListBuffer[Food]()
      val children = node.child //grab all children
      for(child <- children) {
        val tag = child.label
        tag match {
          case "stay" =>
            myStay = new Stay ()
            myStay.xmlRead(child)
          case Food.TAG =>
            val food = new Food() //full functional would give the node to the constructor
            food.xmlRead(child)
            ListFood += food
          case _ => null
        }
      }
    }

  /*
       @function: XML Write writes in data to
       a given XML file.
        */
  override def xmlWrite(): Elem = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", name), ("hunger", hunger.toString))
    var stays = myStay.xmlWrite()
    var FoodXml = ListFood.map(x => x.xmlWrite())
    val children = stays ++ FoodXml //need ALL siblings at one time
    XMLHelper.makeNode("person", attr, children)
  }
/*
When food is found, prints the person who brought it and its weight
 */
  override def findFood(n: String): Boolean =
    {
      var f: Boolean = false
      for(x <- ListFood)
      {
        f= x.findFood(n) //GRADING: CHAIN
        if(f)
        {
          print(name + " brought " + n + " (" + f"${x.getWeight}%.1f" + " lbs).")
          return f
        }
      }
      return f
    }

/*
Calculates the sum of weights of all the food a person brought
 */
  def calcpersonFood() : Double =
    {
      var sum : Double = 0.0
      for(x <- ListFood)
      {
        sum= sum + x.getWeight
      }

      return sum
    }
/*
returns product of hunger and stay
 */
  def sendHungerandStay() : Int =
  {
    return hunger*myStay.giveStay
  }


}

object Person {
  val TAG = "person"
  def createPerson(n : String, hung : Int) = new Person(n,hung)
}