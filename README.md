
# sgit
Development of git in scala. Realization of the following functionalities: 

## Install

To install Sgit application, you have to create a JAR. To do this, follow these instructions, if you are on Windows download the ubuntu shell to carry out the installation
* Download the source code by cloning the git repository
* Launch the sbt console into the cloning repository. There are some requirements to launch sbt, so make sure that you have at least the 1.8 Java version and you have already installed sbt.
* Create a JAR with the command sbt assembly. This command creates a JAR file:` /target/scala-2.13/sgit-assembly-xx.jar`, the `xx` depending of your version of assembly.
* Create an alias with this command: `alias sgit='java -jar path/to/jar/sgit-assembly-xx.jar'`, where `path/to/jar` is the absolute path to the jar file.
* To keep the alias functional you have to add it to the .bashrc file. So, do `nano .bachrc` and write at the end of the file `alias sgit='java -jar path/to/jar/sgit-assembly-xx.jar'`.
* Reload changes with `source .bashrc`. 

## Functionalities
* sgit init
* sgit status 
* sgit diff
* sgit add
* sgit commit
* sgit log 
* sgit branch
* sgit tag 
* sgit checkout