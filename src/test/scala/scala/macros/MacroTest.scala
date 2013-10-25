package scala.macros


object MacroTest extends App{

  Assert.printf("sasas  %d fsafka %s",1,"dsfsd")

  println("")

  {
    val a = 1
    val b = "dsfsd"
    print("sasas ")
    print(a)
    print(" fsafka ")
    print(b)
  }

  println("")
  println(LocationMacro.currentLocation)


  import scala.language.experimental.macros
  val foo = StructuralMacros.makeInstance
  println(foo.z)

  case class User(first: String, last: String, age: Int) extends HListable
  val user = User("Foo", "McBar", 25)
  println(user.members)

  case class Person(name:String,age:Int) extends  Model

  println(Person("name",20).asMap)


}
