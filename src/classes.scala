/**
  * Created by chrx on 12/6/16.
  */
class Classes(teach:String,coursename:String, student:Array[String], marks:scala.collection.mutable.Map[String,Int]){
  val name = coursename
  val teacher = teach
  var grades = marks
  var students = student
}
class Student(fullname:String,ID:Int,schedule:Array[String], marks:scala.collection.mutable.Map[String,Int]){
  val name = fullname
  val id = ID
  var classes = schedule
  var grades = marks
}
case class Teacher(name:String,classes:Array[String])
