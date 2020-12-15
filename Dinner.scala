/*
@file: Contains the dinner class
 */

package soares_raiza

import scala.xml._
import scala.collection.mutable.ListBuffer

class Dinner extends RDP with CoR  {
  private var ListHouses = new ListBuffer[House]()
  private var h : Int = 0
/*
@function: add
Adds a new house if the house is not already in the list.
If the house is known, calls add on that house.
 */
  override def add(): Unit = {
    h = scala.io.StdIn.readLine("what house code: ").toInt
    for(i<- ListHouses) {
      if (i.giveid() == h) {
        i.add()
        return
      }
    }

    ListHouses.append(new House(h))
    print("added house code")

  }
/*
@function: Displays all the houses by calling displayString
on each of the houses.
 */

  override def displayString(): String =
    {
      var new_ListHouses = ListHouses.map(x => x.displayString())
      return new_ListHouses.mkString("\n")
    }

  /*
  @function: Removes a house if it exists, else
  outputs an error message and returns
   */
  def removeHouse(): Unit = {
    h = scala.io.StdIn.readLine("what house code: ").toInt
    for (i <- ListHouses) {
      if (i.giveid() == h) {
        print("removed " + i.giveid())
        ListHouses = ListHouses - i
        return
      }
    }
    print("House code not found")
  }
  /*
   @function: XML read reads in data from
   a given XML file.
    */
  override def xmlRead(node: Node): Unit = {
    val newHouseList = ListBuffer[House]()
    val children = node.child //grab all children
    for(child <- children) {
      val tag = child.label
      tag match {             //matches tags
        case House.TAG =>
            val house = new House() //if house tag is found, creates a new house
            house.xmlRead(child)  //calls XML read on the house to read in its children
            newHouseList += house  //appends new house to the list created
        case _ => null
      }
    }
    for (m<- newHouseList)
      {
        ListHouses.append(m) //appends all elements to the main house list
      }
  }
  /*
     @function: XML Write writes in data to
     a given XML file.
      */
  override def xmlWrite(): Elem = {

    var HouseXml = ListHouses.map(x => x.xmlWrite()) //calls XMLWrite on every house
    XMLHelper.makeNode(Dinner.TAG, null, HouseXml)
  }
  /*
     @function: Goes through each of the houses and
     finds the first person with the desired food.
      */
  override def findFood(name: String): Boolean =
    {
      var f :Boolean = false
      for(x<- ListHouses)
        {
          f= x.findFood(name) //GRADING: CHAIN
          if(f)  //returns immediately after the first match is found
          {
            return f
          }
        }
        return f
    }
  /*
     @function: Calculates leftovers using the parallel technique in house.
      */
  def calculateLeftovers(): Double = {
    h = scala.io.StdIn.readLine("what house code: ").toInt
    var left : Double = 0.0
      for (i <- ListHouses) {
        if (i.giveid() == h) {  //checks only the house that has been requested.
          left = i.calculatePeopleWeight() - i.calculatePeopleHungerandStay()
          return left
        }
      }
   return left
  }
}
object Dinner
{
  val TAG = "dinner"
  def createDinner() = new Dinner()
}
