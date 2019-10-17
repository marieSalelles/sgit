package sgit.refsOperation

import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import better.files._
import sgit.io.{ReadFile, SearchingTools}
import sgit.localChangeRepo.{AddCommand, CommitCommand}

class BranchCommandTest extends FunSpec with BeforeAndAfter {
  before {
    InitCommand.createTreeView()
    val repo :File = ".sgit/".toFile.parent
    val _: File=  (repo + "/" + "READMEBIS.md")
      .toFile
      .createIfNotExists()
  }
  after {
    if("READMEBIS.md".toFile.exists) ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.delete()
    if(".sgit/".toFile.exists) ".sgit/".toFile.delete()
  }
  describe("If the user write the command sgit branch in the sgit repository.") {
    it("should create a file in the heads folder and put the last commit in it.") {
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      val commit= CommitCommand.commit("First commit")
      BranchCommand.newBranch("test")

      assert(".sgit/refs/heads/test".toFile.exists)
      val referencedCommit: String = ".sgit/refs/heads/test".toFile.contentAsString
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      assert(lastCommit.get == referencedCommit)
    }
    it("should not create a branch if it already exists.") {
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      CommitCommand.commit("First commit")
      BranchCommand.newBranch("test")

      val sameNewBranch = BranchCommand.newBranch("test")
      assert(sameNewBranch == "The branch already exists.")
    }
    it("should display all the branches if the user doesn't put an argument.") {
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      CommitCommand.commit("First commit")

      BranchCommand.newBranch("test")

      val displayBranches: String = BranchCommand.newBranch("")
      assert(displayBranches == "Display all the branches.")
    }
    it("should do nothing if there is no commit.") {
      val newBranch = BranchCommand.newBranch("test")

      assert(newBranch == "Do a commit before create a new branch.")
    }
    it("should do list all the branches and their commit infos if the user put the -av option after the branch command.") {
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      val commitKey: Option[String] = CommitCommand.commit("First commit")

      val listBranches: Option[Seq[String]] = BranchCommand.VerboseBranch()

      assert(listBranches.get.head == ("master" + " " +commitKey.get + " " + "First commit"))
    }
  }

}
