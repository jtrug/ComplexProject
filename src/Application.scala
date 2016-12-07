/**
  * Created by chrx on 12/6/16.
  */
object Application {
  def main(args: Array[String]): Unit = {
    load()
    println("School Database")
    println("Please select a menu to enter\n1:Student\n2: Class\n3:Teacher")
    var choice = scala.io.StdIn.readInt()
    choice match {
      case 1 =>

      case 2 =>
      case 3 =>
      case _ =>
        println("Invalid choice exiting")
    }
  }
  def save(student: Array[Student],teacher:Array[Teacher],classes:Array[Classes]):Unit={

  }
  def load():Unit={

  }
}
