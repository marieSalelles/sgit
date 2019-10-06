package sgit.createRepo

import org.scalatest._
import java.nio.file.{Files, Paths}
import better.files._

import sgit.io.CreateRepository

class InitCommandTest extends FunSpec {

  describe("If the user write the command sgit init in a directory") {
    it("A folder with a tree view should be created in the directory.") {
      InitCommand.createTreeView()
      assert(Files.exists(Paths.get(".sgit")))
      assert(Files.exists(Paths.get(".sgit/objects")))
      assert(Files.exists(Paths.get(".sgit/HEAD")))
      assert(Files.exists(Paths.get((".sgit/staged"))))
      assert(Files.exists(Paths.get(".sgit/refs")))
      assert(Files.exists(Paths.get((".sgit/refs/heads"))))
      assert(Files.exists(Paths.get(".sgit/refs/tags")))
      if (Files.exists(Paths.get(".sgit")))  ".sgit".toFile.delete()
    }
    it("Or if the directory already exist, nothing is done.") {
      CreateRepository.initialisation()
      assert (!CreateRepository.initialisation())
      ".sgit".toFile.delete()
    }
  }


}
