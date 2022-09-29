lazy val fps = project
  .in(file("fps"))
  .settings( name           := "fps"
    , version              := "0.1.0"
    , scalaVersion         := "3.2.0"
    , libraryDependencies ++=
      Seq("org.scalacheck" %% "scalacheck" % "1.17.0" % "test")
  )

lazy val aoc = project
  .in(file("aoc"))
  .settings( name           := "aoc"
    , version              := "0.1.0"
    , scalaVersion         := "3.2.0"
    , libraryDependencies ++=
      Seq("org.scalacheck" %% "scalacheck" % "1.17.0" % "test")
  )
