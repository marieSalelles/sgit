package sgit.localChangeRepo

import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import better.files._
import sgit.io.{ReadFile, RepoSearching, SearchingTools}
import sgit.objects.{Blob, StagedLine}

class DiffCommandTest extends FunSpec with BeforeAndAfter {
  before {
    if(".sgit/".toFile.exists) ".sgit".toFile.delete()
    InitCommand.createTreeView()
    val _: File=  ("READMES.md")
      .toFile
      .createIfNotExists()
      .appendLine("I am a test file.")

    val _: File=  ("READMEBIS.md")
      .toFile
      .createIfNotExists()
      .appendLine("Test file.")
      .appendLine("Second line.")
      .appendLine("Third line.")
      .appendLine("Fourth line.")
  }
    after{
      if(".sgit/".toFile.exists) ".sgit".toFile.delete()
      if("READMES.md".toFile.exists) "READMES.md".toFile.delete()
      if("READMEBIS.md".toFile.exists) "READMEBIS.md".toFile.delete()
    }
  describe("If the user write the command sgit diff in the sgit repository."){
    it("should do nothing if there is no commit."){
      assert(!DiffCommand.diffBetweenCommitWD())
    }
    it("should do nothing if there is no difference between the current files version and the commit files version."){
      AddCommand.addAccordingTypeArg(Seq("READMES.md"))
      CommitCommand.commit("commit first version")
      assert(!DiffCommand.diffBetweenCommitWD())
    }
    it("should show the differences in the files when the user commit a version and modify the same file in his working directory by adding a line."){
      //add the first versionof READMES.md file
      AddCommand.addAccordingTypeArg(Seq("READMES.md", "READMEBIS.md"))
      CommitCommand.commit("commit first version")
      val _: File=  ("READMES.md")
        .toFile
        .appendLine("Test the new version.")

      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {
        val root = ".sgit/".toFile.parent
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrive the differents files
        val differentFile: Option[Seq[(Blob,Blob)]] = SearchingTools.searchDifferentFileBetweenWDCommit(workingDirectory, commitContent)

        assert(differentFile.get.length == 1)
        assert(differentFile.get.head._1.path == root.relativize("READMES.md".toFile).toString )

        val differentLine: (List[String], List[String]) = DiffCommand.showDifferences(differentFile.get.head._1.content.split("\n"), differentFile.get.head._2.content.split("\n"))

        assert(differentLine._1.isEmpty)
        assert(differentLine._2.length == 1)
        assert(differentLine._2.toList.head.contains("Test the new version."))
      }
    }
    it("should show the differences in the files when the user commit a version and modify the same file in his working directory by deleting a line."){
      //add the first versionof READMES.md file
      AddCommand.addAccordingTypeArg(Seq("READMES.md", "READMEBIS.md"))
      CommitCommand.commit("commit first version")
      val _: File=  ("READMES.md")
        .toFile
        .overwrite("")

      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {
        val root = ".sgit/".toFile.parent
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrive the differents files
        val differentFile: Option[Seq[(Blob,Blob)]] = SearchingTools.searchDifferentFileBetweenWDCommit(workingDirectory, commitContent)

        assert(differentFile.get.length == 1)
        assert(differentFile.get.head._1.path == root.relativize("READMES.md".toFile).toString )

        val differentLine: (List[String], List[String]) = DiffCommand.showDifferences(differentFile.get.head._1.content.split("\n"), differentFile.get.head._2.content.split("\n"))

        assert(differentLine._1.length == 1)
        assert(differentLine._2.isEmpty)
        assert(differentLine._1.toList.head.contains("I am a test file."))
      }
    }
    it("should show the differences in the files when the user commit a version and modify the same file in his working directory by deleting and adding a line.") {
      //add the first version of READMES.md file
      AddCommand.addAccordingTypeArg(Seq("READMES.md", "READMEBIS.md"))
      CommitCommand.commit("commit first version")
      val _: File = ("READMEBIS.md")
        .toFile
        .overwrite("")
        .appendLine("Test file.")
        .appendLine("Second line changes.")
        .appendLine("Third line.")
        .appendLine("Fourth line changes.")

      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {
        val root = ".sgit/".toFile.parent
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrive the differents files
        val differentFile: Option[Seq[(Blob, Blob)]] = SearchingTools.searchDifferentFileBetweenWDCommit(workingDirectory, commitContent)

        assert(differentFile.get.length == 1)
        assert(differentFile.get.head._1.path == root.relativize("READMEBIS.md".toFile).toString)

        val differentLine: (List[String], List[String]) = DiffCommand.showDifferences(differentFile.get.head._1.content.split("\n"), differentFile.get.head._2.content.split("\n"))

        assert(differentLine._1.length == 2)
        assert(differentLine._2.length == 2)
        assert(differentLine._1.toList.head.contains("Second line."))
        assert(differentLine._2.toList.head.contains("Second line changes."))
      }
    }
    it("should show the differences between the added files and the working directory files when the user commits a version and modifies the same files and adds it and re modifies it.") {
      //add the first version of files
      AddCommand.addAccordingTypeArg(Seq("READMES.md", "READMEBIS.md"))
      CommitCommand.commit("commit first version")
      val _: File = ("READMEBIS.md")
        .toFile
        .overwrite("")
        .appendLine("Test file.")
        .appendLine("Second line changes.")
        .appendLine("Third line.")
        .appendLine("Fourth line changes.")

      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))

      val _: File = ("READMEBIS.md")
        .toFile
        .overwrite("")
        .appendLine("Test file.")
        .appendLine("Second line changes.")
        .appendLine("Third line.")

      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {
        val root = ".sgit/".toFile.parent
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrive the differents files
        val differentFile: Option[Seq[(Blob, Blob)]] = SearchingTools.searchDifferentFileBetweenWDCommit(workingDirectory, commitContent)

        assert(differentFile.get.length == 1)
        assert(differentFile.get.head._1.path == root.relativize("READMEBIS.md".toFile).toString)

        val differentLine: (List[String], List[String]) = DiffCommand.showDifferences(differentFile.get.head._1.content.split("\n"), differentFile.get.head._2.content.split("\n"))

        assert(differentLine._1.length == 1)
        assert(differentLine._1.toList.head.contains("Fourth line changes."))
      }
    }
    it("should show the differences between the commit files and the working directory files when the user commits a version and modifies one of the files and adds it and change the other file in working directory.") {
      //add the first version of READMES.md file
      AddCommand.addAccordingTypeArg(Seq("READMES.md", "READMEBIS.md"))
      CommitCommand.commit("commit first version")
      val _: File = ("READMEBIS.md")
        .toFile
        .overwrite("")
        .appendLine("Test file.")
        .appendLine("Second line changes.")
        .appendLine("Third line.")
        .appendLine("Fourth line changes.")

      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))

      val _: File = ("READMES.md")
        .toFile
        .overwrite("")
        .appendLine("I am a test file.")
        .appendLine("Test to add line.")

      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {
        val root = ".sgit/".toFile.parent
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrive the differents files
        val differentFile: Option[Seq[(Blob, Blob)]] = SearchingTools.searchDifferentFileBetweenWDCommit(workingDirectory, commitContent)

        assert(differentFile.get.length == 2)
        assert(differentFile.get.head._1.path == root.relativize("READMEBIS.md".toFile).toString)
        assert(differentFile.get.last._1.path == root.relativize("READMES.md".toFile).toString)

        val differentLine: (List[String], List[String]) = DiffCommand.showDifferences(differentFile.get.last._1.content.split("\n"), differentFile.get.last._2.content.split("\n"))

        assert(differentLine._2.length == 1)
        assert(differentLine._2.toList.head.contains("Test to add line."))
      }
    }
  }
}
