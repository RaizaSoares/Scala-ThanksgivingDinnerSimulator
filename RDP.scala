package soares_raiza
import scala.xml._
abstract class RDP {
  def add(): Unit
  def displayString() : String
  def xmlRead(node: Node): Unit
  def xmlWrite(): Elem

}
