/**
  * Created by chrx on 12/6/16.
  */
class Classes(teach:Teacher,coursename:String, student:Array[String], marks:Map[String,Int]){
  val name = coursename
  val teacher = teach
  var grades = marks
  var students = student
}
class Student(fullname:String,ID:Int,schedule:Array[String], marks:Map[String,Int]){
  val name = fullname
  val id = ID
  var classes = schedule
  var grades = marks
}
case class Teacher(name:String,classes:Array[Classes])
