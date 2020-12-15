/*
@file: House - Contains the house class
 */

package soares_raiza
import scala.collection.mutable
import scala.xml._
import scala.collection.mutable.ListBuffer

class House (var ident : Int= 0) extends RDP with CoR {

  private var id : Int = ident
  private var ListPeople = new ListBuffer[Person]()
  def giveid() :Int = {return id}
  /*
   @function: If the person exists, calls add on that person,
   else makes a new person and returns
    */
  override def add(): Unit ={
    var name: String = scala.io.StdIn.readLine("\nWhat person: ")
    for(i<- ListPeople) {
      if (i.getName.toLowerCase() == name.toLowerCase()) {
        i.add()
        return

      }
    }

        ListPeople.append(new Person(name, 1))
        println("Added person")
        return
  }
/*
@function: Calls displayString on each of the people in the house
 */
  override def displayString(): String = {
    var housevar : String = s"\nHouse: $id \n"
    var new_ListPeople = ListPeople.map(x => x.displayString())
    return housevar + new_ListPeople.mkString("\n")
  }

  /*
   @function: XML read reads in data from
   a given XML file.
    */
  override def xmlRead(node: Node): Unit =
    {
      id = node.attribute("id").getOrElse("").toString.toInt
      ListPeople = ListBuffer[Person]()
      val children = node.child //grab all children
      for(child <- children) {
        val tag = child.label
        tag match {
          case Person.TAG =>
            val person = new Person() //full functional would give the node to the constructor
            person.xmlRead(child)
            ListPeople += person
          case _ => null
        }
      }
    }
  /*
     @function: XML write writes data to
     a given XML file.
      */
  override def xmlWrite(): Elem= {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("code", id.toString))
    var PeopleXml = ListPeople.map(x => x.xmlWrite())
    XMLHelper.makeNode("house", attr, PeopleXml)

  }
/*
Find food finds the first person with said food.
 */
  override def findFood(name: String): Boolean = {
    var f: Boolean = false
    for(x <- ListPeople)
      {
        f= x.findFood(name)  //GRADING: CHAIN
        if(f) //returns immediately if food is found
          {
            return f
          }
      }
    return f
  }
/*
@function: Calculates the total weight of all the food brought
by people in this house
 */
  def calculatePeopleWeight(): Double = {

    var left: Double = 0.0
    val Parlist  = ListPeople.map(x => x.calcpersonFood()).par
    left = Parlist.reduce(_+_)                //GRADING: LEFTOVERS
    return left
  }

  /*
  @function: Calculates the sum of the product of the
   hunger and stay of people
   */
  def calculatePeopleHungerandStay(): Int=
    {
      var Parlist = ListPeople.map(x => x.sendHungerandStay()).par
      return Parlist.reduce(_+_)                //GRADING: LEFTOVERS
    }
}

object House {
  val TAG= "house"
  def createHouse(ident : Int) = new House(ident)
}
