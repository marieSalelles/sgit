package sgit.app

import scopt.OParser
import sgit.app.commandlineParser._

import sgit.createRepo._
import sgit.localChangeRepo._

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
      case "add" => AddCommand.addAccordingTypeArg(files)
      case "commit" => CommitCommand.commit()
      case _=> println("Error, write a good command 2")
    }
  }

  /*def determineOpt(opt: String, arg: String){

  }*/


}
