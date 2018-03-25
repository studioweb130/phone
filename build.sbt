lazy val phoneCompany = (project in file(".")).settings(
  Seq(
    name := "disco-test-phone-company",
    version := "1.0",
    scalaVersion := "2.12.3"
  )
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3"
libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
