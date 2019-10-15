package sgit.app

import scopt.OParser
import sgit.app.commandlineParser._
import sgit.branch.{BranchCommand, TagCommand}
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
   * @param files : files argument
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
      case "checkout" => println("checkout")
      case "merge" => println("merge")
      case "rebase" => println("rebase")
      case _=> println("Error, write a good command!")
    }
  }

  def determineOpt(opt: String, command: String, arg: Option[String]){
    if (command == "log") {
      opt match {
        case ""=> LogCommand.log()
        case "stat" => println("log stat")
        case "p" => println("log p")
      }
    }
    else if (command =="branch") {
      opt match {
        case "" => BranchCommand.newBranch(arg.get)
        case "av" => BranchCommand.VerboseBranch()
      }
    }
  }


}
