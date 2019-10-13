package sgit.localChangeRepo

import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import better.files._
import sgit.io.ReadFile
import sgit.objects.Commit

class CommitCommandTest extends FunSpec with BeforeAndAfter {
  before {
    if(".sgit/".toFile.exists) ".sgit".toFile.delete()
    InitCommand.createTreeView()
  }
  after {
    if(".sgit/".toFile.exists) ".sgit".toFile.delete()
    if("READMES.md".toFile.exists) "READMES.md".toFile.delete()
    if("READMEBIS.md".toFile.exists) "READMEBIS.md".toFile.delete()
  }
  describe("If the user write the command sgit commit in the sgit repository."){
    it("should do nothing if the staged file is empty") {
      assert(CommitCommand.commit("First commit.").isEmpty)
    }
    it("should create the first commit if the heads folder is empty."){
      //create a file in the repo
      val repo :File = ".sgit/".toFile.parent
      val _: File = (repo + "/" + "READMES.md")
        .toFile
        .createIfNotExists()
      // add the file to the staged file
      AddCommand.addAccordingTypeArg(Seq("READMES.md"))

      val commit = CommitCommand.commit("First commit")
      assert(commit.isDefined)
      assert(".sgit/refs/heads/master".toFile.exists)

      val commitInHeads: String = ".sgit/refs/heads/master".toFile.contentAsString
      assert(commitInHeads == commit.get)
      assert(".sgit/staged".toFile.contentAsString.isEmpty)
    }
    it("should crete a commit and modify the file in heads folder"){
      val repo :File = ".sgit/".toFile.parent
      val _: File = (repo + "/" + "READMES.md")
        .toFile
        .createIfNotExists()
      val _: File = (repo + "/" + "READMEBIS.md")
        .toFile
        .createIfNotExists()
      // add the file to the staged file
      AddCommand.addAccordingTypeArg(Seq("READMES.md"))
      // create the first commit
      val firstCommit = CommitCommand.commit("First commit")
      // add another file
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      // create the second commit
      val commit = CommitCommand.commit("Second commit")

      assert(commit.isDefined)
      // retrieve the value of the last created commit on the master branch
      val commitInHeads: String = ".sgit/refs/heads/master".toFile.contentAsString
      //retrieve the info of the second commit
      val secondCommitInfos: Option[Commit] = ReadFile.readCommitProperties(commit)

      assert(secondCommitInfos.get.parents.head == firstCommit.get)

      assert(commitInHeads == commit.get)
      assert(".sgit/staged".toFile.contentAsString.isEmpty)
    }
  }

}
