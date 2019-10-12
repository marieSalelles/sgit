package sgit.app

import scopt.OParser
import sgit.app.commandlineParser._

import sgit.createRepo._
import sgit.localChangeRepo._

object Main extends App {

  /** Check if the user write a command.
    */
  OParser.parse(Parser.parser1, args, Config()) match {
    case Some(config) => determineMode(config.command, config.option, config.files, config.message)
    case None => println("")
  }

  /**
   * Dispatch according to the command written by the user
   * @param command : written command
   * @param option : command option
   * @param files : files argument
   */
  def determineMode(command: String, option: String, files: Seq[String], message: String): Unit = {
    command match {
      case "init" => InitCommand.createTreeView()
      case "status" => StatusCommand.statusFile()
      case "diff" => println("diff")
      case "add" => AddCommand.addAccordingTypeArg(files)
      case "commit" => CommitCommand.commit(message)
      case "log" => determineOpt(option, "log")
      case _=> println("Error, write a good command 2")
    }
  }

  def determineOpt(opt: String, command: String){
    if (command == "log") {
      opt match {
        case ""=> println("log")
        case stat => println("log stat")
        case p => println("log p")
      }
    }
  }


}
