/*
@Main header
@author: Raiza Soares

@description: The program simulates a thanksgiving dinner.
There are 7 options in a menu from which a user can select from
1) Add data
2) Display data
3) Remove house
4) Load XML
5) Write XML
6) Find food
7) Estimate leftovers
Add data lets the user enter in a house, person, food and their
corresponding inputs.
Display data displays the data so far.
Remove house removes a house from the simulation
Load XML reads data from an XML file
Write data writes data to an XML file
Find food finds the first food among all the houses
Estimate leftovers computes the leftovers based on hunger and stay,
and weight of food.

@To_do: none
@Bugs: none

 */

package soares_raiza
import java.text.DecimalFormat

import scala.io.StdIn
import scala.xml.XML



object MainStarterStudent extends App {
    var choice = -1

    val menu: String =
        """
          |1) Add data
          |2) Display data
          |3) Remove house
          |4) Load XML
          |5) Write XML
          |6) Find food
          |7) Estimate leftovers
          |0) Quit
          |Choice: """.stripMargin

    var temp = ""
    var d = new Dinner
    while (choice != 0) {
        try {
            print(menu)
            //something to strip out empty lines
            temp = StdIn.readLine()
            while(temp.isEmpty)
                temp = StdIn.readLine()
            choice = temp.toInt

            choice match {
                case 0 =>
                case 1 =>
                    //GRADING: ADD
                    d.add()
                case 2 => println(d.displayString())             //GRADING: PRINT
                case 3 => d.removeHouse()
                case 4 => {
                    print("File name: ")
                    temp = StdIn.readLine()
                    try{
                    val topNode = XML.loadFile(temp)
                    if(topNode.label != "dinner"){
                        println("invalid xml file. needs to be an dinner xml file")
                    }else {
                        //GRADING: READ
                        d.xmlRead(topNode)
                    }
                    }catch {
                       case e: Throwable=> print("Could not open file: " + temp +
                         " (the system cannot find the file specified)")
                    }
                }
                case 5 => {
                    println("File name: ")
                    val fileName = scala.io.StdIn.readLine()
                    //GRADING: WRITE
                    val xmlOut= d.xmlWrite()
                    XML.save(fileName, xmlOut, "UTF-8", true, null)
                }
                case 6 => {
                    var name: String = scala.io.StdIn.readLine("Food: ")
                    var flag: Boolean = false
                    //GRADING: FIND
                    flag =d.findFood(name)
                    if(!flag)
                        {
                            println(name + " not found")
                        }
                }
                case 7 =>
                    {   var lefts : Double =0.0
                        lefts = d.calculateLeftovers()
                        print("Total leftovers: " + f"$lefts%.1f" + " lbs")
                    }
                case _ => println("Invalid option")
            }
        } catch {
            case e: Throwable => print(e)
        }
    }
}

