package sgit.localChangeRepo

import better.files._
import org.scalatest._
import sgit.createRepo.InitCommand
import sgit.io._

class AddCommandTest extends FunSpec with BeforeAndAfter {
  before {
    InitCommand.createTreeView()
    val repo :File = ".sgit/".toFile.parent
    val folder: File = ("test/")
      .toFile
      .createIfNotExists(true,false)
    val _: File=  (repo + "/" + "test/READMES.c")
      .toFile
      .createIfNotExists()
    val _: File=  (repo + "/" + "test/READMEBIS.c")
      .toFile
      .createIfNotExists()
  }
  after {
    if(".sgit/".toFile.exists) ".sgit".toFile.delete()
    if("test/".toFile.exists) "test".toFile.delete()
    if("READMES.c".toFile.exists) ".sgit/".toFile.parent + "/" + "READMES.c".toFile.delete()
    if("READMEBIS.c".toFile.exists) ".sgit/".toFile.parent + "/" + "READMEBIS.c".toFile.delete()
  }

  describe("If the user write the command sgit add <args> in the sgit repository."){
    it("should add files which correspond to the regex in the staged file.") {
        val regexarg = List("test/*.c")
        AddCommand.addAccordingTypeArg(regexarg)
        //search the file corresponding to the regex in the user repo
        val fileInUserRepo = RepoSearching.searchDirectoryFile("test/*.c")
        val files = fileInUserRepo.filterNot(f => f.isDirectory)
        //generate sha key to files
        val shakeysAndPath = files.map(f => (f.sha1 + " " + ".sgit/".toFile.parent.relativize(f).toString))
        //retrieve the sha key and path in the staged file
        val contentFile: List[String] = ".sgit/staged".toFile.contentAsString
          .replace("\r", "")
          .split("\n").toList

        assert(fileInUserRepo.map(f => f.name) == List("READMEBIS.c", "READMES.c"))
        assert(contentFile == shakeysAndPath)

    }
    it("should add files which correspond to the name(s) given in by the user in the staged file."){
      val files :Seq[String] = Seq("test/READMES.c", "test/READMEBIS.c")
      AddCommand.addAccordingTypeArg(files)

      //search the file corresponding to the names in the user repo
      val foundFile: Seq[File] = RepoSearching.searchDirectoryFile(files)
      val files2: Seq[File] = foundFile.filterNot(f => f.isDirectory)
      //generate sha key to files
      val shakeysAndPath: Seq[String] = files2.map(f => (f.sha1 +" "+ ".sgit/".toFile.parent.relativize(f).toString))
      //retrieve the sha key and path in the staged file
      val contentFile: List[String] = ".sgit/staged".toFile.contentAsString
        .replace("\r", "")
        .split("\n").toList

      assert(foundFile.map(f => f.name) == List("READMES.c", "READMEBIS.c"))
      assert(contentFile == shakeysAndPath)
    }
    it("should add all the files contain in the working directory in the staged file.") {
      val args :Seq[String] = Seq(".")
      AddCommand.addAccordingTypeArg(args)

      val files = RepoSearching.searchAllDirectoryFile(".sgit/")
      val files2: Seq[File] = files.filterNot(f => f.isDirectory)
      //generate sha key to files
      val shakeysAndPath: Seq[String] = files2.map(f => (f.sha1 +" "+ ".sgit/".toFile.parent.relativize(f).toString))
      //retrieve the sha key and path in the staged file
      val contentFile = ".sgit/staged".toFile.contentAsString
        .replace("\r", "")
        .split("\n").toList

      assert(contentFile == shakeysAndPath)
    }
  }
}
