//
// Taranos Cloud Sonification Framework: Service Core
// Copyright (C) 2018 David Hinson, Netrogen Blue LLC (dhinson@netrogenblue.com)
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

name := "taranos-core"
version := "0.2"

scalaVersion := "2.11.8"

// Akka
libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4"

// Play Framework's JSON library
libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.3.10"

// ScalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

scalacOptions += "-feature"

