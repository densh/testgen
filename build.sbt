sourceGenerators in Compile += Def.task {
  testgen.TestGen.gen().map {
    case (name, contents) =>
      val file = (sourceManaged in Compile).value / name
      IO.write(file, contents)
      file
  }
}

scalaVersion := "2.12.7"
