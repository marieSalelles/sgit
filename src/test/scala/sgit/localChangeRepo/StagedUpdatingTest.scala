package sgit.localChangeRepo

import better.files._
import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import sgit.io.ReadFile
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
    if("READMES.md".toFile.exists) ".sgit/".toFile.parent + "/" + "READMES.md".toFile.delete()
    if("READMEBIS.md".toFile.exists) {
      ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.delete()
    }
    if(".sgit/".toFile.exists) ".sgit/".toFile.delete()
  }

  describe("If the user has already added files and try to re add files without commit (the staged file has a content)") {
    describe("The files that the user would like to add are compared to the files in the staged file.") {
      it("should not add a file that is already in the staged file.") {
        val file = "READMEBIS.md"
        AddCommand.addAccordingTypeArg(List(file))
        //build the expected value
        val expectedFileStaged :StagedLine = StagedLine(file.toFile.sha1, ".sgit/".toFile.parent.relativize(file.toFile).toString)
        //add the same file
        AddCommand.addAccordingTypeArg(List(file))
        //read the staged file
        val stagedContent: Option[Seq[StagedLine]] = ReadFile.readStaged()

        assert(stagedContent.get.head == expectedFileStaged)
      }
      it("should delete the old version of a file in the staged file.") {
        val file = "READMEBIS.md"
        AddCommand.addAccordingTypeArg(List(file))
        //modify the READMEBIS.md
        ".sgit/".toFile.parent + "/" + file
          .toFile
          .appendLine("#It is a test.")
        //add the new version of the file
        AddCommand.addAccordingTypeArg(List(file))
        //compute the expected value
        val expectedFileStaged :StagedLine = StagedLine(file.toFile.sha1, ".sgit/".toFile.parent.relativize(file.toFile).toString)
        val stagedContent = ReadFile.readStaged()
        val stagedFile = stagedContent.get.map(f => f)

        assert(stagedFile.head == expectedFileStaged)
      }
      it("should delete a file that is not in the working directory but in the staged file."){
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
