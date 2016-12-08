/**
  * Nick Trefz
  * Jared Bartrug
  *
  */

import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.io.Source

object Application {
  def main(args: Array[String]): Unit = {
    val (students, teachers, classes) = load()
    MainMenu(students, teachers, classes)
  }

  def save(student: Array[Student], teacher: Array[Teacher], classes: Array[Classes]): Unit = {
    val writerS = new PrintWriter(new File("Students.txt"))
    val writerT = new PrintWriter(new File("Teachers.txt"))
    val writerC = new PrintWriter(new File("Classes.txt"))
    for (s <- student) {
      val tempStr = s.name + " " + s.id.toString + " " + s.classes.mkString(",") + " " + s.grades.toList.mkString(", ") + "\n"
      writerS.write(tempStr)
    }
    writerS.close()
    for (c <- classes) {
      val tempStr = c.name + " " + c.teacher + " " + c.students.mkString(",") + " " + c.grades.toList.mkString(", ") + "\n"
      writerC.write(tempStr)
    }
    writerC.close()
    for (t <- teacher) {
      val tempStr = t.name + " " + t.classes.mkString(",") + "\n"
      writerT.write(tempStr)
    }
    writerT.close()

  }

  def load() = {
    // Loading function loads all the students teachers and classes from file
    //Opening up the respective files
    val studentInfo = Source.fromFile("Students.txt").getLines().toList
    val students = ArrayBuffer[Student]()

    val teacherInfo = Source.fromFile("Teachers.txt").getLines().toList
    val teachers = ArrayBuffer[Teacher]()

    val classInfo = Source.fromFile("Classes.txt").getLines().toList
    val classes = ArrayBuffer[Classes]()

    for (i <- studentInfo) {
      val info = i.split(" ")
      val values = info(3).split(", ")
      val m = scala.collection.mutable.Map[String, Int]()
      for (i <- values) {
        val key = i.substring(1, i.length - 1).split(",")
        m(key(0)) = key(1).toInt
      }
      students += new Student(info(0), info(1).toInt, info(2).split(","), m)
    }

    for (i <- teacherInfo) {
      val info = i.split(" ")
      val name = info(0)
      val classes = info(1).split(",")
      teachers += Teacher(name, classes)
    }


    for (i <- classInfo) {
      val info = i.split(" ")
      val values = info(3).split(", ")
      val m = scala.collection.mutable.Map[String, Int]()
      for (i <- values) {

        val key = i.substring(1, i.length - 1).split(",")
        m(key(0)) = key(1).toInt
      }
      classes += new Classes(info(0), info(1), info(2).split(","), m)
    }
    (students.toArray, teachers.toArray, classes.toArray)
  }

  def MainMenu(students: Array[Student], teachers: Array[Teacher], classes: Array[Classes]): Unit = {
    println("School Database")
    println("Please select a menu to enter\n0: Exit Program\n1:Student\n2: Class\n3:Teacher")
    val choice = StdIn.readInt()
    choice match {
      case 0 =>
        save(students, teachers, classes)
        sys.exit()
      case 1 => // Student
        StudentMainMenu(students, teachers, classes)

      case 2 => //Class
        ClassMainMenu(students, teachers, classes)

      case 3 => //Teacher
        TeacherMainMenu(students, teachers, classes)

      case _ =>
        println("Invalid choice exiting")
    }
  }

  def StudentMainMenu(students: Array[Student], teachers: Array[Teacher], classes: Array[Classes]): Unit = {
    var firstChoice = 0
    var secondChoice = 0
    var getOutOfWhileLoop = false

    while (!getOutOfWhileLoop) {
      println("0: Back to main menu\n1: Search Students by name\n2: Search students by ID\n3: See all Students")
      firstChoice = StdIn.readInt()

      firstChoice match {
        case 0 =>
          //back to main menu
          MainMenu(students, teachers, classes)
          sys.exit()
        case 1 =>
          //Search by name
          getOutOfWhileLoop = true
          println("Search for a Name: ")
          var name = StdIn.readLine()

        case 2 =>
          // Search by ID
          getOutOfWhileLoop = true

        case 3 =>
          // Show all students
          getOutOfWhileLoop = true

        case _ =>
          println("Invalid choice.")
          getOutOfWhileLoop = false
      }
    }
    println("-1: Exit Program \n0: Return to main menu")
    /** Print the results */


    print("Select an option or student to view")
    secondChoice = StdIn.readInt()

    if (secondChoice == 0) {
      //return to main menu
      MainMenu(students, teachers, classes)
      sys.exit()
    }
    else if (secondChoice == -1) {
      // exit program
      save(students, teachers, classes)
      sys.exit()
    }
    else {
      PrintStudent(students(secondChoice))

    }

  }

  def PrintStudent(stud: Student): Unit = {
    println("Name: " + stud.name)
    println("Student ID: " + stud.id)
    println("Classes: " + stud.classes.mkString(", "))
    println("Grades: " + stud.grades)
    println()
  }

  def PrintClass(clasz: Classes): Unit = {
    println(clasz.name)
    println(clasz.teacher)
    println(clasz.students.mkString(", "))
    println(clasz.grades)
  }

  def ClassMainMenu(students: Array[Student], teachers: Array[Teacher], classes: Array[Classes]): Unit = {
    println("Select an option: ")
    println("0: Main Menu \n1: Search for a class \n2: View All Classes")
    val option = StdIn.readInt()
    var doWhileLoop = true

    while (doWhileLoop) option match {
      case 0 =>

        /** Main Menu */
        MainMenu(students, teachers, classes)
        sys.exit()

      case 1 =>

        /** Search */
        doWhileLoop = false
        print("Enter the name of the class to search for: ")
        val search = StdIn.readLine()
        var resultArray = classes.filter(_.name == search)
        resultArray.foreach(PrintClass(_))

      case 2 =>

        /** Show all classes */
        doWhileLoop = false
        classes.foreach(PrintClass(_))

      case _ =>
        println("Invalid Selection. Try again.")
        doWhileLoop = true
    } // end of while loop
  }

  def TeacherMainMenu(students: Array[Student], teachers: Array[Teacher], classes: Array[Classes]): Unit = {
    println("Select an option: ")
    println("0: Return to Main Menu \n1: Search for Teacher by name \n2: View all Teachers \n3: Add a teacher")
    val selection = StdIn.readInt()

    selection match {
      case 0 =>
        MainMenu(students, teachers, classes)

      case 1 =>
        println("Enter the name of the teacher to search for")
        val search = StdIn.readLine()
        var searchResults = teachers.filter(_.name == search)
        searchResults.foreach(PrintTeacher(_))

      case 2 =>
        teachers.foreach(PrintTeacher(_))

      case 3 =>
        println("Enter the teacher's name: ")
        val tName = StdIn.readLine()
        println("Enter the class the teacher's class: ")
    val tClass = StdIn.readLine()
        val tClassArr = Array(tClass)

        val newTeach = new Teacher(tName, tClassArr)
        val newTeachArray = Array(newTeach)
        teachers.copyToArray(newTeachArray)

        save(students, newTeachArray, classes)
        MainMenu(students, newTeachArray, classes)
    }
  }

    def PrintTeacher(teach: Teacher): Unit = {
      println(teach.name)
      teach.classes.foreach(println(_))
    }

}