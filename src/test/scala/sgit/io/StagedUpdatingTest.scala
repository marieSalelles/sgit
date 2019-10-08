package sgit.io

import java.nio.file.{Files, Paths}

import better.files._
import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import sgit.localChangeRepo.AddCommand
import sgit.objects.StagedLine

class StagedUpdatingTest extends FunSpec with BeforeAndAfter{
  before {
    InitCommand.createTreeView()
    val repo :File = ".sgit/".toFile.parent
    val _: File=  (repo + "/" + "READMEBIS.md")
      .toFile
      .createIfNotExists()
    val _: File=  (repo + "/" + "READMES.md")
      .toFile
      .createIfNotExists()
      .appendLine("* Test")
  }
  after {
    ".sgit/".toFile.delete()
    ".sgit/".toFile.parent + "/" + "READMES.md".toFile.delete()
    if (Files.exists(Paths.get("READMEBIS.md"))) {
      ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.delete()
    }
  }

  describe("If the user have already added file and try to re add file without commit (the staged file have a content)") {
    describe("The file that the user would like to add are compare to the files in the staged.") {
      it(" If there is the same file it is not rewrite on the staged file.") {
        val file = "READMEBIS.md"
        AddCommand.addAccordingTypeArg(List(file))
        //build the expected value
        val expectedFileStaged :StagedLine = StagedLine(file.toFile.sha1, ".sgit/".toFile.parent.relativize(file.toFile).toString)
        //add the same file
        AddCommand.addAccordingTypeArg(List(file))
        //read the staged file
        val stagedContent = ReadFile.readStaged()
        val stagedFile = stagedContent.get.map(f => f)
        assert(stagedFile.head == expectedFileStaged)
      }
      it("If a new version of a file is added the older one is delete of the staged file and the new is added to this.") {
        val file = "READMEBIS.md"
        AddCommand.addAccordingTypeArg(List(file))
        ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.appendLine("#It is a test.")
        //add the new version of the file
        AddCommand.addAccordingTypeArg(List(file))
        //compute the expected value
        val expectedFileStaged :StagedLine = StagedLine(file.toFile.sha1, ".sgit/".toFile.parent.relativize(file.toFile).toString)
        val stagedContent = ReadFile.readStaged()
        val stagedFile = stagedContent.get.map(f => f)
        assert(stagedFile.head == expectedFileStaged)
      }
      it("If a file in the staged file is deleted by the user in his repository, the line which refers to this file in the staged file is deleted"){
        val file = "READMEBIS.md"
        val file2 = "READMES.md"
        AddCommand.addAccordingTypeArg(List(file))
        ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.delete()
        //add another file
        AddCommand.addAccordingTypeArg(List(file2))
        //compute the expected value
        val expectedFileStaged :StagedLine = StagedLine(file2.toFile.sha1, ".sgit/".toFile.parent.relativize(file2.toFile).toString)
        //read the current staged file
        val stagedContent = ReadFile.readStaged()
        val stagedFile = stagedContent.get.map(f => f)
        assert(stagedFile.head == expectedFileStaged)
      }
    }
  }

}
