package sgit.app

import scopt.OParser
import sgit.app.commandlineParser._
import scala.util.matching.Regex
import sgit.createRepo._

object Main extends App {

  /** Check if the user write a command.
    */
  OParser.parse(Parser.parser1, args, Config()) match {
    case Some(config) => determineMode(config.command, config.option, config.files)
    case None => println("")
  }

  /**
   * Dispatch according to the command written by the user
   * @param command : written command
   * @param option : command option
   * @param files : files argument
   */
  def determineMode(command: String, option: String, files: Seq[String]): Unit = {
    command match {
      case "init" => InitCommand.createTreeView()
      case "status" => println("status")
      case "diff" => println("diff")
      case "add" => addTypeArg (files)
      case _=> println("Error, write a good command 2")
    }
  }

  /*def determineOpt(opt: String, arg: String){

  }*/

  /**
   * Check the argument type of add command
   * @param files : sequence of string which can be a dot, a regex or filenames
   */
  def addTypeArg(files :Seq[String]): Unit = {
      if(files.nonEmpty) {
        if (files.head == ".") println("add all docs.")
        else {
          val regexFilename: Regex = "^[a-zA-Z0-9_]+\\.[A-Za-z]*$".r
          regexFilename.matches(files.head) match {
            case true => println("file.")
            case false => println("regex")
          }
        }
      } else println("enter an argument after the add command.")

  }
}
