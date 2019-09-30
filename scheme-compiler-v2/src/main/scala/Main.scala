import scala.io.StdIn

object repl extends App {

  def commandLoop(expr: String): Unit = {
    expr match {
      case _ => println(expr)
    }
    try {
      print("> ")
      val line = StdIn.readLine()
      println(line) // TODO(john-b-yang): Replace with parse method call
    } catch {
      case e: Exception => println("Error: ", e) // TODO(john-b-yang): Handle this
    }
  }

  val greet = "(display \"mtscheme v0.1 \")"
  commandLoop()
}