package sgit.branch

import better.files.File
import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import better.files._
import sgit.io.SearchingTools
import sgit.localChangeRepo.{AddCommand, CommitCommand}

class TagCommandTest extends FunSpec with BeforeAndAfter{
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
  describe("If the user write the command sgit tag in the sgit repository.") {
    it("should create a file in the tags folder and put the last commit in it.") {
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      CommitCommand.commit("First commit")
      TagCommand.newTag("test")

      assert(".sgit/refs/tags/test".toFile.exists)
      val referencedCommit: String = ".sgit/refs/tags/test".toFile.contentAsString
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      assert(lastCommit.get == referencedCommit)
    }
    it("should not create a tag if it already exists.") {
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      CommitCommand.commit("First commit")
      TagCommand.newTag("test")

      val sameNewTag = TagCommand.newTag("test")
      assert(sameNewTag == "The tag already exists.")
    }
    it("should display all the tags if the user doesn't put an argument."){
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      CommitCommand.commit("First commit")

      val displayEmptyListTags: String = TagCommand.newTag("")
      assert(displayEmptyListTags == "There is no tags.")
      //create a tag.
      TagCommand.newTag("test")

      val displayTags: String = TagCommand.newTag("")
      assert(displayTags == "Display all the tags.")
    }
  }
}
