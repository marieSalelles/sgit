package sgit.app

import scopt.OParser
import sgit.app.commandlineParser._
import sgit.refsOperation._
import sgit.commitHistory.LogCommand
import sgit.createRepo._
import sgit.localChangeRepo._

object Main extends App {

  /** Check if the user write a command.
    */
  OParser.parse(Parser.parser1, args, Config()) match {
    case Some(config) => determineMode(config.command, config.option, config.files, config.message, config.branch, config.tag, config.checkout, config.merge, config.rebase)
    case None => println("")
  }

  /**
   * Dispatch according to the command written by the user
   * @param command : written command
   * @param option : command option
   * @param files : files argument given with the add command
   * @param message : commit argument
   * @param branch : branch argument
   * @param tag : tag argument
   * @param checkout : checkout argument
   * @param merge : merge argument
   * @param rebase : rebase argument
   */
  def determineMode(command: String, option: String, files: Seq[String], message: String, branch: String, tag: String, checkout: String, merge: String, rebase: String): Unit = {
    command match {
      case "init" => InitCommand.createTreeView()
      case "status" => StatusCommand.statusFile()
      case "diff" => DiffCommand.diffBetweenCommitWD()
      case "add" => AddCommand.addAccordingTypeArg(files)
      case "commit" => CommitCommand.commit(message)
      case "log" => determineOpt(option, "log", None)
      case "branch" => determineOpt(option, "branch", Some(branch))
      case "tag" => TagCommand.newTag(tag)
      case "checkout" => CheckoutCommand.checkout(checkout)
      case "merge" => println("Unimplemented functionality.")
      case "rebase" => println("Unimplemented functionality.")
      case _=> println("Error, write a valid command!")
    }
  }

  /**
   * Dispatch according to the option given after the command.
   * @param opt: option
   * @param command: command
   * @param arg: argument
   */
  def determineOpt(opt: String, command: String, arg: Option[String]){
    if (command == "log") {
      opt match {
        case ""=> LogCommand.log()
        case "stat" => println("Unimplemented functionality.")
        case "p" => println("Unimplemented functionality.")
      }
    }
    else if (command =="branch") {
      opt match {
        case "v" => BranchCommand.VerboseBranch()
        case "" => BranchCommand.newBranch(arg.get)
      }
    }
  }


}
