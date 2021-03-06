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

  def MainMenu(students: ArrayBuffer[Student], teachers: ArrayBuffer[Teacher], classes: ArrayBuffer[Classes]): Unit = {
    var exit = false
    while (!exit) {
      println("---School Database---")
      println("Please select a menu to enter\n0: Exit Program\n1:Student\n2:Class\n3:Teacher")
      val choice = StdIn.readInt()
      println()

      choice match {
        case 0 =>
          save(students, teachers, classes)
          sys.exit()
        case 1 => // Student
          StudentMainMenu(students, teachers, classes)
          exit = true
        case 2 => //Class
          ClassMainMenu(students, teachers, classes)
          exit = true
        case 3 => //Teacher
          TeacherMainMenu(students, teachers, classes)
          exit = true
        case _ =>
          println("Invalid Choice.\n")
          exit = false
      }
    }
  }

  def StudentMainMenu(students: ArrayBuffer[Student], teachers: ArrayBuffer[Teacher], classes: ArrayBuffer[Classes]): Unit = {
    var firstChoice = 0
    var secondChoice = 0
    var getOutOfWhileLoop = false

    while (!getOutOfWhileLoop) {
      println("---Student Database Main Menu---")
      println("0:Return to Main Menu\n1:Search Students by Name\n2:Search Students by ID\n3:See All Students\n4:Add a Student\n5:Remove a Student")
      firstChoice = StdIn.readInt()
      println()

      firstChoice match {
        case 0 =>
          //back to main menu
          MainMenu(students, teachers, classes)
        case 1 =>
          //Search by name
          getOutOfWhileLoop = true
          println("Search by a Name: ")
          val search = StdIn.readLine()
          println()
          val results = students.filter(_.name == search)
          results.foreach(PrintStudent(_))

        case 2 =>
          // Search by ID
          getOutOfWhileLoop = true
          println("Search by an ID: ")
          val search = StdIn.readInt()
          println()
          val results = students.filter(_.id == search)
          results.foreach(PrintStudent(_))

        case 3 =>
          // Show all students
          getOutOfWhileLoop = true
          students.sortBy(_.name.toUpperCase).foreach(PrintStudent(_))

        case 4 =>
          // add student
          getOutOfWhileLoop = true
          println("Please enter the students name")
          val name = StdIn.readLine()
          println("Please enter the students id")
          val id = StdIn.readInt()
          println("Please enter the students classes seperated by commas")
          val classes = StdIn.readLine().trim.split(" *, *")
          val m = scala.collection.mutable.Map[String, Int]()
          classes.foreach {
            x => print("Please enter the grade for " + x)
              m(x) = StdIn.readInt()
          }
          students += new Student(name, id, classes, m)

        case 5 =>  //remove a student
          getOutOfWhileLoop = true
          println("Remove by a Name: ")
          val search = StdIn.readLine()
          students.filter(_.name == search).foreach(students -= _)

        case _ =>
          println("Invalid choice.")
          getOutOfWhileLoop = false
      }
    }
    println("Select an option")
    println("0:Return to main menu\n1:Return to Student Menu")
    secondChoice = StdIn.readInt()
    println()

    if (secondChoice == 0) {
      //return to main menu
      MainMenu(students, teachers, classes)
      sys.exit()
    }
    else {
      StudentMainMenu(students, teachers, classes)
    }

  }

  def PrintStudent(stud: Student): Unit = {
    println("Name: " + stud.name)
    println("Student ID: " + stud.id)
    println("Classes: " + stud.classes.mkString(", "))
    println("Grades: " + stud.grades.mkString(", "))
    println()
  }

  def PrintClass(clasz: Classes): Unit = {
    println("Class: " + clasz.name)
    println("Teacher: " + clasz.teacher)
    println("Students: " + clasz.students.mkString(", "))
    println("Grades: " + clasz.grades.mkString(", "))
    println()
  }

  def ClassMainMenu(students: ArrayBuffer[Student], teachers: ArrayBuffer[Teacher], classes: ArrayBuffer[Classes]): Unit = {
    println("---Class Database Main Menu---")
    println("0:Return to Main Menu \n1:Search for a Class \n2:View All Classes\n3:Add a Class\n4:Remove a Class")
    val option = StdIn.readInt()
    var doWhileLoop = true

    while (doWhileLoop) option match {
      case 0 =>

        /** Main Menu */
        MainMenu(students, teachers, classes)
      case 1 =>

        /** Search */
        doWhileLoop = false
        print("Enter the name of the class to search for: ")
        val search = StdIn.readLine()
        println()
        val resultArray = classes.filter(_.name == search)
        resultArray.foreach(PrintClass(_))

      case 2 =>

        /** Show all classes */
        doWhileLoop = false
        classes.sortBy(_.name.toUpperCase).foreach(PrintClass(_))
        println()

      case 3 =>
        // add student
        doWhileLoop = false
        println("Please enter the class name")
        val name = StdIn.readLine()
        println("Please enter the teacher name")
        val teach = StdIn.readLine()
        println("Please enter the students names separated by commas")
        val studs = StdIn.readLine().trim.split(" *, *")
        val m = scala.collection.mutable.Map[String, Int]()
        studs.foreach {
          x => print("Please enter the grade for " + x)
            m(x) = StdIn.readInt()
        }
        classes += new Classes(name, teach, studs, m)
        println()


      case 4 => //remove a student
        doWhileLoop = false
        println("Remove Class by a Name: ")
        val search = StdIn.readLine()
        classes.filter(_.name == search).foreach(classes -= _)
      case _ =>
        println("Invalid Selection. Try again.\n")
        doWhileLoop = true
    }
    ClassMainMenu(students, teachers, classes)
  }

  def TeacherMainMenu(students: ArrayBuffer[Student], teachers: ArrayBuffer[Teacher], classes: ArrayBuffer[Classes]): Unit = {
    println("---Teacher Database Main Menu---")
    println("0:Return to Main Menu \n1:Search for Teacher by name \n2:View all Teachers \n3:Add a teacher\n4:Remove a Teacher")
    val selection = StdIn.readInt()
    println()

    selection match {
      case 0 =>
        MainMenu(students, teachers, classes)

        //search for a student
      case 1 =>
        println("Enter the name of the teacher to search for")
        val search = StdIn.readLine()
        println()
        val searchResults = teachers.filter(_.name == search)
        searchResults.foreach(PrintTeacher(_))

        //print teachers
      case 2 =>
        teachers.sortBy(_.name.toUpperCase).foreach(PrintTeacher(_))

        //add teacher
      case 3 =>
        println("Enter the teacher's name: ")
        val tName = StdIn.readLine()
        println("Enter the teacher's classes separated by commas: ")
        val tClass = StdIn.readLine()
        println()

        teachers += Teacher(tName, tClass.split(" *, *"))

        save(students, teachers, classes)


      case 4 => //remove teacher
        println("Remove Teacher by a Name: ")
        val search = StdIn.readLine()
        teachers.filter(_.name == search).foreach(teachers -= _)
      case _ =>
        println("Invalid choice please try again.\n")
    }
    TeacherMainMenu(students, teachers, classes)
  }

  def PrintTeacher(teach: Teacher): Unit = {
    print("Teacher: " + teach.name + "   ")
    println("Classes: " + teach.classes.mkString(" , "))
    println()
  }

  //save the objects to file
  def save(student: ArrayBuffer[Student], teacher: ArrayBuffer[Teacher], classes: ArrayBuffer[Classes]): Unit = {
    //opening up the files
    val writerS = new PrintWriter(new File("Students.txt"))
    val writerT = new PrintWriter(new File("Teachers.txt"))
    val writerC = new PrintWriter(new File("Classes.txt"))
    //converting objects to string
    for (s <- student.distinct) {
      var tempStr = s.name + ":" + s.id.toString + ":" + s.classes.mkString(",") + ":" + s.grades.toList.mkString(";")
      if (s != student.last) tempStr += "\n"
      writerS.write(tempStr)
    }
    writerS.close()
    for (c <- classes.distinct) {
      var tempStr = c.name + ":" + c.teacher + ":" + c.students.mkString(",") + ":" + c.grades.toList.mkString(";")
      if (c != classes.last) tempStr += "\n"
      writerC.write(tempStr)
    }
    writerC.close()
    for (t <- teacher.distinct) {
      var tempStr = t.name + ":" + t.classes.mkString(",")
      if (t != teacher.last) tempStr += "\n"
      writerT.write(tempStr)
    }
    writerT.close()

  }
  //loading the objects
  def load() = {
    // Loading function loads all the students teachers and classes from file
    //Opening up the respective files
    val studentInfo = Source.fromFile("Students.txt").getLines().toList
    val students = ArrayBuffer[Student]()

    val teacherInfo = Source.fromFile("Teachers.txt").getLines().toList
    val teachers = ArrayBuffer[Teacher]()

    val classInfo = Source.fromFile("Classes.txt").getLines().toList
    val classes = ArrayBuffer[Classes]()

    //converting string to object
    for (i <- studentInfo) {
      val info = i.split(":")
      val values = info(3).split(";")
      val m = scala.collection.mutable.Map[String, Int]()
      for (i <- values) {
        val key = i.substring(1, i.length - 1).split(",")
        m(key(0)) = key(1).toInt
      }
      students += new Student(info(0), info(1).toInt, info(2).split(","), m)
    }

    for (i <- teacherInfo) {
      val info = i.split(":")
      val name = info(0)
      val classes = info(1).split(",")
      teachers += Teacher(name, classes)
    }


    for (i <- classInfo) {
      val info = i.split(":")
      val values = info(3).split(";")
      val m = scala.collection.mutable.Map[String, Int]()
      for (i <- values) {

        val key = i.substring(1, i.length - 1).split(",")
        m(key(0)) = key(1).toInt
      }
      classes += new Classes(info(0), info(1), info(2).split(","), m)
    }
    (students, teachers, classes)
  }
}