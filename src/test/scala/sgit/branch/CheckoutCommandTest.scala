package sgit.branch

import better.files._
import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import sgit.io.ReadFile
import sgit.localChangeRepo.{AddCommand, CommitCommand}

class CheckoutCommandTest extends FunSpec with BeforeAndAfter {
  before {
    InitCommand.createTreeView()
    val repo :File = ".sgit/".toFile.parent
    val folder: File = ("test/")
      .toFile
      .createIfNotExists(true,false)
    val _: File=  ("test/READMES.md")
      .toFile
      .createIfNotExists(false, true)
    val _: File=  ("test/READMEBIS.md")
      .toFile
      .createIfNotExists(false, true)
        .append("A line.")
    AddCommand.addAccordingTypeArg(Seq("test/READMEBIS.md","test/READMES.md"))
  }
  after{
    if(".sgit/".toFile.exists) ".sgit".toFile.delete()
    if("test/".toFile.exists) "test".toFile.delete()
  }

  describe("If the user write the command sgit checkout in the sgit repository.") {
    it("should change the current working directory with the version store in the branch name given in argument."){
      //commit the added files
      CommitCommand.commit("My first commit.")
      //create a branch
      BranchCommand.newBranch("test")

      CheckoutCommand.checkout("test")

      assert(ReadFile.readHEAD() == "refs/heads/test")

      //delete a file
      "test/READMEBIS.md".toFile.delete()
      CheckoutCommand.checkout("master")

      assert("test/READMEBIS.md".toFile.exists)
      assert(ReadFile.readHEAD() == "refs/heads/master")
    }
    it("should change the current working directory with the version store by the tag name given in argument."){
      //commit the added files
      CommitCommand.commit("My first commit.")
      //create a tag
      TagCommand.newTag("tagTest")
      //modified a file
      "test/READMEBIS.md".toFile.appendLine("It is a test line.")

      AddCommand.addAccordingTypeArg(Seq("test/READMEBIS.md"))
      CommitCommand.commit("commit modified file.")

      CheckoutCommand.checkout("tagTest")

      assert("test/READMEBIS.md".toFile.contentAsString.contains ("A line."))
      assert(!"test/READMEBIS.md".toFile.contentAsString.contains ("It is a test line."))
    }
    it("should change the current working directory with the version of the commit given in argument."){
      //commit the added files
      val firstCommit: Option[String] = CommitCommand.commit("My first commit.")

      //modified a file
      "test/READMEBIS.md".toFile.appendLine("It is a test line.")

      AddCommand.addAccordingTypeArg(Seq("test/READMEBIS.md"))
      CommitCommand.commit("commit modified file.")

      CheckoutCommand.checkout(firstCommit.get)

      assert("test/READMEBIS.md".toFile.contentAsString.contains ("A line."))
      assert(!"test/READMEBIS.md".toFile.contentAsString.contains ("It is a test line."))
    }
  }
}
