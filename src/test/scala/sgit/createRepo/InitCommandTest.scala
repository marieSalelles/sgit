package sgit.createRepo

import org.scalatest._
import java.nio.file.{Files, Paths}
import better.files._

import sgit.io.CreateRepository

class InitCommandTest extends FunSpec with BeforeAndAfter {

  after {
    if(Files.exists(Paths.get(".sgit"))) ".sgit".toFile.delete()
  }
  describe("If the user writes the command sgit init in a directory") {
    it(" should  create a folder with a tree view in the directory.") {
      InitCommand.createTreeView()
      assert(Files.exists(Paths.get(".sgit")))
      assert(Files.exists(Paths.get(".sgit/objects")))
      assert(Files.exists(Paths.get(".sgit/HEAD")))
      assert(Files.exists(Paths.get((".sgit/staged"))))
      assert(Files.exists(Paths.get(".sgit/refs")))
      assert(Files.exists(Paths.get((".sgit/refs/heads"))))
      assert(Files.exists(Paths.get(".sgit/refs/tags")))
    }
    it("sould do nothing if the directory already exist.") {
      CreateRepository.initialisation()
      assert (!CreateRepository.initialisation())
    }
  }


}
