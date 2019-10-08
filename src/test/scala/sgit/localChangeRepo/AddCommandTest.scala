package sgit.localChangeRepo

import better.files._
import org.scalatest._
import sgit.createRepo.InitCommand
import sgit.io._
import sgit.localChangeRepo.AddCommand

class AddCommandTest extends FunSpec with BeforeAndAfter {
  before {
    InitCommand.createTreeView()
    val repo :File = ".sgit/".toFile.parent
    val _: File=  (repo + "/" + "READMES.md")
      .toFile
      .createIfNotExists()
    val _: File=  (repo + "/" + "READMEBIS.md")
      .toFile
      .createIfNotExists()
  }
  after {
    ".sgit/".toFile.delete()
    ".sgit/".toFile.parent + "/" + "READMES.md".toFile.delete()
    ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.delete()
  }

  describe("If the user write the command sgit add <args> in the sgir repository."){
    it("The file(s) corresponding to the regex is/are added to the staged file.") {
      val regexarg = List("[A-Za-z]*R[a-zA-Z]*.[a-z]*")

      AddCommand.addAccordingTypeArg(regexarg)
      //search the file corresponding to the regex in the user repo
      val fileInUserRepo = RepoSearching.searchDirectoryFile("[A-Za-z]*R[a-zA-Z]*.[a-z]*".r)
      val files = fileInUserRepo.filterNot(f => f.isDirectory)
      //generate sha key to files
      val shakeys = files.map(f => f.sha1)
      //retrieve the sha key in the file staged
      val contentFile = ".sgit/staged".toFile.contentAsString.split("\r\n").toList
      assert(contentFile == shakeys)

    }
    it("The file(s) corresponding the the name(s) given in by the user is/are added into the staged file."){
      val files :Seq[String] = Seq("README.md", "build.sbt")
      AddCommand.addAccordingTypeArg(files)

      //search the file corresponding to the regex in the user repo
      val foundFile = RepoSearching.searchDirectoryFile(files)
      //generate sha key to files
      val files2 = foundFile.filterNot(f => f.isDirectory)
      val shakeys = files2.map(f => f.sha1)
      //retrieve the sha key in the file staged
      val contentFile = ".sgit/staged".toFile.contentAsString.split("\r\n").toList
      assert(contentFile == shakeys)
    }
    it("All the files in the repository are added the the staged file.") {
      val args :Seq[String] = Seq(".")
      AddCommand.addAccordingTypeArg(args)

      val files = RepoSearching.searchAllDirectoryFile()
      //generate sha key to files
      val files2 = files.filterNot(f => f.isDirectory)
      val shakeys = files2.map(f => f.sha1)
      //retrieve the sha key in the file staged
      val contentFile = ".sgit/staged".toFile.contentAsString.split("\r\n").toList
      assert(contentFile == shakeys)
    }
  }
}
