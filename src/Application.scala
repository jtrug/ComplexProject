import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.io.Source

/**
  * Nick Trefz
  * Jared Bartrug
  *
  */
object Application {
  def main(args: Array[String]): Unit = {
    val (students,teachers,classes) = load()
    MainMenu(students,teachers,classes)
  }

  def MainMenu(students:ArrayBuffer[Student],teachers:ArrayBuffer[Teacher],classes:ArrayBuffer[Classes]): Unit = {
    println("School Database")
    println("Please select a menu to enter\n1:Student\n2: Class\n3:Teacher")
    val choice = StdIn.readInt()
    choice match {
      case 1 => // Student
        StudentMainMenu(students,teachers,classes)

      case 2 => //Class
        ClassMainMenu(students,teachers,classes)

      case 3 => //Teacher


      case _ =>
        println("Invalid choice exiting")
    }
  }
  def StudentMainMenu(students:ArrayBuffer[Student],teachers:ArrayBuffer[Teacher],classes:ArrayBuffer[Classes]): Unit = {
    var firstChoice = 0
    var secondChoice = 0
    var invalidChoice = false

    while (!invalidChoice) {
      println("0: Back to main menu\n1: Search Students by name\n2: Search students by ID\n3: See all Students\n4: Add a student")
      firstChoice = StdIn.readInt()

      firstChoice match {
        case 0 =>
          //back to main menu
          MainMenu(students,teachers,classes)
          sys.exit()
        case 1 =>
          //Search by name
          invalidChoice = false
          println("Search by a Name: ")
          var search = StdIn.readLine()
          var results = students.filter(_.name==search)
          results.foreach(PrintStudent(_))
        case 2 =>
          // Search by ID
          invalidChoice = false
          println("Search by an ID: ")
          var search = StdIn.readInt()
          var results = students.filter(_.id==search)
          results.foreach(PrintStudent(_))
        case 3 =>
          // Show all students
          invalidChoice = false
          students.foreach(PrintStudent(_))
        case 4 =>
          // add student
          invalidChoice = false
          println("Please enter the students name")
          val name = StdIn.readLine()
          println("Please enter the students id")
          val id = StdIn.readInt()
          println("Please enter the students classes seperated by commas")
          val classes = StdIn.readLine().trim.split(" *, *")
          val m = scala.collection.mutable.Map[String, Int]()
          classes.foreach{
            x=>print("Please enter the grade for "+x)
              m(x) = StdIn.readInt()
          }
          val temp = new Student(name,id,classes,m)
          students += temp
          PrintStudent(temp)
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
    secondChoice = StdIn.readInt()

    if (secondChoice == 0) {
      //return to main menu
      MainMenu(students,teachers,classes)
      sys.exit()
    }
    else if (secondChoice == -1) {
      // exit program
      save(students,teachers,classes)
      sys.exit()
    }
    else {
      //PrintStudent(secondChoice, arrayOfStudents)
    }

  }

  def PrintStudent( stud: Student): Unit = {
    println("Name: " + stud.name)
    println("Student ID: " + stud.id)
    println("Classes: " + stud.classes.mkString(","))
    println("Grades: " + stud.grades)
  }
  def ClassMainMenu(students:ArrayBuffer[Student],teachers:ArrayBuffer[Teacher],classes:ArrayBuffer[Classes]): Unit = {
    println("\nList of Classes:")
    // print a list of all classes
  }

  def save(student: ArrayBuffer[Student], teacher: ArrayBuffer[Teacher], classes: ArrayBuffer[Classes]): Unit = {
    //opening up the files
    val writerS = new PrintWriter(new File("Students.txt"))
    val writerT = new PrintWriter(new File("Teachers.txt"))
    val writerC = new PrintWriter(new File("Classes.txt"))
    //converting objects to string
    for (s <- student.distinct) {
      var tempStr = s.name + " " + s.id.toString + " " + s.classes.mkString(",") + " " + s.grades.toList.mkString(";")
      if(s != student.last)tempStr+="\n"
      writerS.write(tempStr)
    }
    writerS.close()
    for (c <- classes.distinct) {
      var tempStr = c.name + " " + c.teacher + " " + c.students.mkString(",") + " " + c.grades.toList.mkString(";")
      if(c != classes.last)tempStr+="\n"
      writerC.write(tempStr)
    }
    writerC.close()
    for (t <- teacher.distinct) {
      var tempStr = t.name + " " + t.classes.mkString(",")
      if(t != teacher.last)tempStr+="\n"
      writerT.write(tempStr)
    }
    writerT.close()

  }

  def load() = {
    // Loading function loads all the students teachers and classes from file
    //Opeing up the respective files
    val studentInfo = Source.fromFile("Students.txt").getLines().toList
    val students = ArrayBuffer[Student]()

    val teacherInfo = Source.fromFile("Teachers.txt").getLines().toList
    val teachers = ArrayBuffer[Teacher]()

    val classInfo = Source.fromFile("Classes.txt").getLines().toList
    val classes = ArrayBuffer[Classes]()

    //converting string to object
    for (i <- studentInfo) {
      val info = i.split(" ")
      var values = info(3).split(";")
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