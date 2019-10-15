package sgit.branch

import sgit.io
import sgit.io.{ConsolePrinter, CreateFile, ReadFile, SearchingTools, WriteFile}
import sgit.objects.{Commit, StagedLine}

object BranchCommand {

  /**
   * Create a new branch.
   * @param name : branch name
   * @return the printed string into the console
   */
  def newBranch(name: String): String = {
    if(SearchingTools.searchSgitFolder()) {
      if(name == "") {
        val allBranches: Seq[(String, String)] = SearchingTools.searchAllBranches()
        ConsolePrinter.displayList(allBranches.map(branch => branch._1))
        "Display all the branches."
        //search if the branch already exists
      } else if (!SearchingTools.searchBranch(name)) {
        //retrieve the lastCommit
        val commit: Option[String] = SearchingTools.findLastCommit()
        //create a file in heads folder
        if (commit.isDefined) {
          CreateFile.writeHeadsFile("refs/heads/"+ name, commit.get)
          //change the current branch
          WriteFile.writeHead(name)
          ConsolePrinter.display("Success, the branch is created.")
          "Success, the branch is created."
        }
        else {
          ConsolePrinter.display("Do a commit before create a new branch.")
          "Do a commit before create a new branch."
        }
      } else {
        ConsolePrinter.display("The branch already exists.")
        "The branch already exists."
      }
    } else {
      ConsolePrinter.display("Do an sgit init.")
      "Do an sgit init."
    }
  }

  /**
   * Display the list of all branch with their last commmit (commit sha and message)
   * @return the list of the branches with their commit infos
   */
  def VerboseBranch(): Option[Seq[String]] = {
    if(SearchingTools.searchSgitFolder()) {
      val allBranches: Seq[(String, String)] = SearchingTools.searchAllBranches()

      val branchesList: Seq[String] = allBranches.map(branch => {
        //retrieve commit content
        val commitContent: Option[Commit] = ReadFile.readCommitProperties(Some(branch._2))
        val commitMessage = commitContent.get.message
        ConsolePrinter.display( branch._1 + " " + branch._2 + " " + commitMessage)
        branch._1 + " " + branch._2 + " " + commitMessage
      })
      Some(branchesList)
    } else None
  }

}
