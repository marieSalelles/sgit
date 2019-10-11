package sgit.io

object ConsolePrinter {
  /**
   * Print in the console
   * @param message the message to print
   */
  def display(message: String) :Unit = {
    println(message)
  }

  /**
   * Print a list of String in the console.
   * @param list : list of string
   */
  def displayList(list :Seq[String]): Unit = {
    if(list.nonEmpty) {
      println(list.head)
      displayList(list.tail)
    }
  }
}
