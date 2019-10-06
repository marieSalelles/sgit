package sgit.createRepo

import org.scalatest._
import java.nio.file.{Files, Paths}
import better.files._
import sgit.localChangeRepo.AddCommand

import sgit.io._

class AddCommandTest extends FunSpec {

  describe("If the user write the command sgit add <args> in the sgir repository."){
    it("The file(s) corresponding to the regex is/are added to the staged file.") {
      val regexarg = List("[A-Za-z]*R[a-zA-Z]*.[a-z]*")
      AddCommand.addAccordingTypeArg(regexarg)

      val fileInUserRepo = RepoSearching.searchDirectoryFile("[A-Za-z]*R[a-zA-Z]*.[a-z]*".r)
      val shakeys = fileInUserRepo.map(f => f.sha1)
      val contentFile = ".sgit/staged".toFile.contentAsString.split(" ").toSeq
      assert(contentFile == shakeys)
    }
  }
}
