/**
  * Nick Trfz
  * Jared Bartrug
  * Created by chrx on 12/6/16.
  */
object Application {
  def main(args: Array[String]): Unit = {
    load()
    MainMenu()
  }

  def save(students: Array[Student], teachers: Array[Teacher], classes: Array[Classes]): Unit = {

  }

  def load(): Unit = {

  }
 def MainMenu(): Unit = {
   println("School Database")
   println("Please select a menu to enter\n1:Student\n2: Class\n3:Teacher")
   val choice = readInt()
   choice match {
     case 1 => // Student
       StudentMainMenu()

     case 2 => //Class
       ClassMainMenu()

     case 3 => //Teacher


     case _ =>
       println("Invalid choice exiting")
   }
 }
  def StudentMainMenu(): Unit = {
    var firstChoice = 0
    var secondChoice = 0
    var invalidChoice = false

    while (!invalidChoice) {
      println("\n1: Search Students by name\n2: Search students by ID\n3: See all Students")
      firstChoice = readInt()

      firstChoice match {
        case 0 =>
          //back to main menu
          MainMenu()
          sys.exit()
        case 1 =>
          //Search by name
          invalidChoice = false
          println("Search for a Name: ")
          var name = readLine()

        case 2 =>
        // Search by ID
          invalidChoice = false

        case 3 =>
        // Show all students
          invalidChoice = false

        case _ =>
          println("Invalid choice.")
          invalidChoice = true
      }
    }
    println("-1: Exit Program \n0: Return to main menu")
      /*
    Print the results
     */

      print("Select an option or student to view")
      secondChoice = secondChoice

      if (secondChoice == 0) {
        //return to main menu
        MainMenu()
        sys.exit()
      }
      else if (secondChoice == -1) {
        // exit program
        sys.exit()
      }
      else {
        //ShowStudent(secondChoice, arrayOfStudents)
      }

  }

  def ShowStudent(index: Int, array: Array[Student]): Unit = {
    println("Name: " + array(index).name)
    println("Student ID: " + array(index).id)
    println("Classes: " + array(index).classes)
    println("Grades: " + array(index).grades)
  }
  def ClassMainMenu(): Unit = {
    println("\nList of Classes:")
    // print a list of all classes
  }
}