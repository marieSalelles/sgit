package sgit.commitHistory

import java.util.Date

import sgit.io.{ConsolePrinter, ReadFile, SearchingTools}
import sgit.objects.Commit

import scala.annotation.tailrec

object LogCommand {
  /**
   * Write the commit log in the console.
   */
  def log(): Unit = {
    if(SearchingTools.searchSgitFolder()) {
      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {
        ConsolePrinter.display("The current branch is :" + ReadFile.readHEAD().split("/").toList.last)
        showCommitProperties(lastCommit, Seq())
      }
      else ConsolePrinter.display("No commit at the moment.")
    } else ConsolePrinter.display("Do a sgit init before a sgit log.")
  }

  /**
   * Search the commit properties.
   * @param name: commit name
   * @param commits : commits list of the displayed commit names
   * @return the list of all commit names
   */
    @tailrec
  def showCommitProperties(name: Option[String], commits: Seq[String]):Seq[String] = {
      //retrieve the commit properties
    val commit: Option[Commit] = ReadFile.readCommitProperties(name)

    if (commit.isDefined) {
      ConsolePrinter.printCommit(commit.get.sha, Date.from(commit.get.timestamp), commit.get.message)
      if (commit.get.parents.nonEmpty) showCommitProperties(Some(commit.get.parents.head),commits :+ commit.get.sha)
      else commits :+ commit.get.sha
      //TODO: handle 2 parents when the merge will be implemented
      //if (commit.get.parents.length == 1)
      //else if (commit.get.parents.length == 2) {}
    } else commits
  }
}
