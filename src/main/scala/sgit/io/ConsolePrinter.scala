package sgit.io

import java.util.Date
import scala.annotation.tailrec

object ConsolePrinter {
  /**
   * Print in the console
   * @param message: the message to print
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
      display(list.head)
      displayList(list.tail)
    }
  }

  /**
   * Print a string in red in the console.
   * @param word : a string
   */
  def displayRed (index: String, word :String): Unit = {
      println("\u001B[31m"+ "- " + index + " " + word + "\u001B[0m")
  }

  /**
   * Print a string in green in the console.
   * @param word : a string
   */
  def displayGreen (index:String, word :String): Unit = {
    println("\u001B[32m"+ "+ " + index + " " + word + "\u001B[0m")
  }

  /**
   * Print the current branch in green in the console.
   * @param branch: branch name
   */
  def displayCurrentBranch(branch: String): Unit = {
    println("\u001B[32m"+ "* "+ branch + "\u001B[0m")
  }

  /**
   * Print the status states.
   * @param state : state (Changes to be committed or Not stage for commit)
   * @param subState : sub state (added, modified, deleted, untracked)
   * @param files : file path list to print
   */
  def printStatus(state: String, subState: String, files: Seq[String]): Unit ={
    display(state)
    display(subState)
    displayList(files)
  }

  /**
   * Print the commit info.
   * @param name : commit name
   * @param date : commit date
   * @param message : commit description
   */
  def printCommit(name: String, date: Date, message: String): Unit = {
    display("\u001B[35m" + "Commit name: "+ "\u001B[0m" + name)
    display("\u001B[35m" + "Date: "+"\u001B[0m" + date)
    display("\u001B[35m" + "Description: "+"\u001B[0m" + message + "\n")
  }
}
