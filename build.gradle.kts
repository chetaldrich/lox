group = "template"
version = "0.1.0-SNAPSHOT"

plugins {
    `java-library`
    scala
    java
}

repositories {
    jcenter()
}

dependencies {
    implementation(group = "org.scala-lang", name = "scala-library", version = "2.12.4")
}

tasks.withType<Wrapper> {
    gradleVersion = "5.0"
}

task<JavaExec>("run") {
   classpath = sourceSets["main"].runtimeClasspath
   main = "org.name.Main"
   args = emptyList()
}
