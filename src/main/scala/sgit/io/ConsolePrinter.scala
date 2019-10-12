package sgit.io

import better.files.File

import scala.annotation.tailrec

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
  @tailrec
  def displayList(list :Seq[String]): Unit = {
    if(list.nonEmpty) {
      println(list.head)
      displayList(list.tail)
    }
  }

  /**
   * Print the status states
   * @param state : state : Changes to be committed ou not stage for commit
   * @param subState : sub state : added, modified, deleted, untracked
   * @param files : file path list to print
   */
  def printStatus(state: String, subState: String, files: Seq[String]): Unit ={
    ConsolePrinter.display(state)
    ConsolePrinter.display(subState)
    ConsolePrinter.displayList(files)
  }
}
