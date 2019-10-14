package sgit.branch

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
}

}
