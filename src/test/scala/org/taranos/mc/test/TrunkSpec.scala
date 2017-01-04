//
// Taranos Cloud Sonification Framework: Service Core
// Copyright (C) 2017 David Hinson, Netrogen Blue LLC (dhinson@netrogenblue.com)
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

package org.taranos.mc.test

import org.scalatest.fixture
import org.taranos.mc.trunk.intraprocess.Patch
import play.api.libs.json._
import org.taranos.common.ServiceCall
import org.taranos.mc.Cell.ResponseMessages.ServiceResult
import org.taranos.mc.{Cell, CellDirector}


class TrunkSpec extends fixture.FlatSpec
{
    case class FixtureParam(cellDirector: CellDirectorFixture)

    class CellDirectorFixture
    {
        // Create an actor system for the fixture:
        val _actorSystem = akka.actor.ActorSystem("TestActorSystem")

        // Create a mailbox for the fixture:
        val _inbox = akka.actor.Inbox.create(_actorSystem)

        // Create a cell director actor:
        val _cellDirectorRef = _actorSystem.actorOf(CellDirector.MakeProps, "TestCellDirector")


        _cellDirectorRef ! CellDirector.RequestMessages.Start

        def CallService(
            serviceName: String,
            serviceArgs: AnyRef*): org.taranos.common.ServiceResult =
        {
            def ArgsHelper(args: Seq[AnyRef]): Vector[String] =
            {
                var result = Vector.empty[String]
                for (arg <- args)
                {
                    result :+= (arg match
                    {
                        case jsValue: JsValue => Json.stringify(jsValue)
                        case value: String => value
                        case _ => "?"
                    })
                }
                result
            }

            _inbox.send(
                _cellDirectorRef,
                CellDirector.RequestMessages.ServiceCall(ServiceCall(serviceName, ArgsHelper(serviceArgs))))

            import scala.concurrent.duration._
            val message = _inbox.receive(10.minutes)
            message match
            {
                case serviceResult: ServiceResult =>
                    serviceResult._serviceResult

                case _ =>
                    assert(false); null
            }
        }
    }

    def withFixture(test: OneArgTest) =
    {
        val fixtureParam = FixtureParam(new CellDirectorFixture)

        try
        {
            withFixture(test.toNoArgTest(fixtureParam)) // "loan" the fixture to the test
        }
        finally
        {}
    }

    def CountTrunkElements (
        f: FixtureParam,
        trunkKey: String,
        specificElements: String*): Int =
    {
        val elements =
            if (specificElements.isEmpty)
                Vector(
                    "t",
                    "si",
                    "sp",
                    "ss",
                    "sk",
                    "sl",
                    "st",
                    "smi",
                    "smb",
                    "smo",
                    "smpe",
                    "smpo")
            else
                specificElements
        val report = ReportTrunkPlants(f, trunkKey)
        var sum: Int = 0
        for (element <- elements)
            sum += (report \ trunkKey \ element \ "ec").as[String].toInt
        sum
    }

    def ReportTrunkPlants (
        f: FixtureParam,
        trunkKey: String): JsObject =
    {
        val serviceResult = f.cellDirector.CallService("@ReportTrunkModel", trunkKey, Json.obj("s" -> Json.arr("c")))
        (Json.parse(serviceResult._results.head) \ "rmt" \ "rp").as[JsObject]
    }

    val expectedField1Key = "f1~f"
    val expectedTrunk1Key = "t1~t"
    val field1Tag  = "!f1"
    val field1Name = "f1"
    val trunk1Tag = "!t1"
    val trunk1Name = "t1"

    //
    // Trunks:
    //
    "CellDirector" must "pass basic trunk tests" in
        { f =>
            // Attempt to destroy trunk (should fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm trunk state:
            var trunkReports = (Json.parse(serviceResult._results.head) \ "rt").as[Seq[JsObject]]
            var trunkReport = trunkReports.head.as[JsObject]
            assert((trunkReport \ "m" \ "k").as[String] == expectedTrunk1Key)
            assert((trunkReport \ "m" \ "t").as[String] == trunk1Tag)

            // Update trunk:
            val trunk1Renamed = trunk1Name + "_renamed"
            val trunkUpdate = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedTrunk1Key,
                    "n" -> trunk1Renamed))
            serviceResult = f.cellDirector.CallService("UpdateTrunks", trunkUpdate)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get trunk report:
            var query = Json.obj(
                "k" -> Json.arr(expectedTrunk1Key))
            serviceResult = f.cellDirector.CallService("ReportTrunks", query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm trunk state:
            trunkReports = (Json.parse(serviceResult._results.head) \ "rt").as[Seq[JsObject]]
            trunkReport = trunkReports.head.as[JsObject]
            assert((trunkReport \ "m" \ "k").as[String] == expectedTrunk1Key)
            assert((trunkReport \ "m" \ "n").as[String] == trunk1Renamed)

            // Destroy trunk:
            assert(CountTrunkElements(f, expectedTrunk1Key, "t") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key, "si") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 2)
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "t") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key, "si") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 0)

            // Get trunk report (should be none reporting):
            serviceResult = f.cellDirector.CallService("ReportTrunks", query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            trunkReports = (Json.parse(serviceResult._results.head) \ "rt").as[Seq[JsObject]]
            assert(trunkReports.isEmpty)
        }

    //
    // Signal Interfaces:
    //
    it must "pass basic signal interface tests" in
        { f =>
            val expectedInterface1Key = "si1~si"
            val expectedInterface2Key = "si2~si"
            val interface1Name = "si1"
            val interface2Name = "si2"
            val interface1Tag = "!si1"
            val interface2Tag = "!si2"

            //
            // Test entity creation:
            //

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create interfaces:
            val interface1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> interface1Tag))
            val interface2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> interface2Tag))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalInterfaces",
                expectedTrunk1Key,
                interface1Constructor,
                interface2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm interface states:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedInterface1Key,
                    expectedInterface2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalInterfaces",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            var interfaceReports = (Json.parse(serviceResult._results.head) \ "rsi").as[Seq[JsObject]]
            assert(interfaceReports.size == 2)
            for (report <- interfaceReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedInterface1Key` =>
                        assert((report \ "m" \ "t").as[String] == interface1Tag)
                    case `expectedInterface2Key` =>
                        assert((report \ "m" \ "t").as[String] == interface2Tag)
                    case _ => assert(false)
                }

            //
            // Test entity updates:
            //

            // Update interfaces:
            val interface1Renamed = interface1Name + "_renamed"
            var interface1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInterface1Key,
                    "n" -> interface1Renamed))
            val interface2Renamed = interface2Name + "_renamed"
            var interface2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInterface2Key,
                    "n" -> interface2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalInterfaces",
                expectedTrunk1Key,
                interface1Update,
                interface2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm interface states:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedInterface1Key,
                    expectedInterface2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalInterfaces",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            interfaceReports = (Json.parse(serviceResult._results.head) \ "rsi").as[Seq[JsObject]]
            assert(interfaceReports.size == 2)
            for (report <- interfaceReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedInterface1Key` =>
                        assert((report \ "m" \ "n").as[String] == interface1Renamed)
                    case `expectedInterface2Key` =>
                        assert((report \ "m" \ "n").as[String] == interface2Renamed)
                    case _ => assert(false)
                }

            //
            // Test entity destruction:
            //

            // Destroy interfaces:
            assert(CountTrunkElements(f, expectedTrunk1Key, "si") == 3)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 4)
            val interface1Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInterface1Key))
            val interface2Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInterface2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalInterfaces",
                expectedTrunk1Key,
                interface1Destructor,
                interface2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "si") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 2)

            // Confirm interface states (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedInterface1Key,
                    expectedInterface2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalInterfaces",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            interfaceReports = (Json.parse(serviceResult._results.head) \ "rsi").as[Seq[JsObject]]
            assert(interfaceReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }

    //
    // Signal Ports:
    //
    it must "pass basic signal port tests" in
        { f =>
            val expectedInterface1Key = "si1~si"
            val expectedPort1Key = "sp1~sp"
            val expectedPort2Key = "sp2~sp"
            val expectedSink1Key = "sk1~sk"
            val interface1Tag = "!si1"
            val port1Tag = "!sp1"
            val port2Tag = "!sp2"
            val sink1Tag = "!sk1"

            //
            // Test entity creation:
            //

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create interface:
            val interface1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> interface1Tag))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalInterfaces",
                expectedTrunk1Key,
                interface1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create destination sinks:
            val sinkCConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> sink1Tag,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalSinks",
                expectedTrunk1Key,
                sinkCConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create ports:
            val port1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> port1Tag,
                    "m" -> "c"),
                "r" -> Json.obj(
                    "si" -> expectedInterface1Key))
            val port2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> port2Tag,
                    "m" -> "c"),
                "r" -> Json.obj(
                    "si" -> expectedInterface1Key))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalPorts",
                expectedTrunk1Key,
                expectedInterface1Key,
                port1Constructor,
                port2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm port states:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedPort1Key,
                    expectedPort2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalPorts",
                expectedTrunk1Key,
                expectedInterface1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            var portReports = (Json.parse(serviceResult._results.head) \ "rsp").as[Seq[JsObject]]
            assert(portReports.size == 2)
            for (report <- portReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedPort1Key` =>
                        assert((report \ "m" \ "t").as[String] == port1Tag)
                    case `expectedPort2Key` =>
                        assert((report \ "m" \ "t").as[String] == port2Tag)
                    case _ => assert(false)
                }

            //
            // Test entity updates:
            //

            // Update ports:
            val port1Renamed = port1Tag + "_renamed"
            var port1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedPort1Key,
                    "n" -> port1Renamed))
            val port2Renamed = port2Tag + "_renamed"
            var port2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedPort2Key,
                    "n" -> port2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalPorts",
                expectedTrunk1Key,
                port1Update,
                port2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm port states:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedPort1Key,
                    expectedPort2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalPorts",
                expectedTrunk1Key,
                expectedInterface1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            portReports = (Json.parse(serviceResult._results.head) \ "rsp").as[Seq[JsObject]]
            assert(portReports.size == 2)
            for (report <- portReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedPort1Key` =>
                        assert((report \ "m" \ "n").as[String] == port1Renamed)
                    case `expectedPort2Key` =>
                        assert((report \ "m" \ "n").as[String] == port2Renamed)
                    case _ => assert(false)
                }

            //
            // Test entity destruction:
            //

            // Destroy ports:
            assert(CountTrunkElements(f, expectedTrunk1Key, "sp") == 2)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 12)
            val port1Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedPort1Key))
            val port2Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedPort2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalPorts",
                expectedTrunk1Key,
                port1Destructor,
                port2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sp") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 4)

            // Confirm port states (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedPort1Key,
                    expectedPort2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalPorts",
                expectedTrunk1Key,
                expectedInterface1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            portReports = (Json.parse(serviceResult._results.head) \ "rsp").as[Seq[JsObject]]
            assert(portReports.isEmpty)

            // Destroy interface:
            val interface1Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInterface1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalInterfaces",
                expectedTrunk1Key,
                interface1Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            CountTrunkElements(f, expectedTrunk1Key)

            // Destroy trunk:
            CountTrunkElements(f, expectedTrunk1Key)
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }

    //
    // Signal Sources:
    //
    it must "pass basic signal source tests" in
        { f =>
            val expectedSource1Key = "ss1~ss"
            val source1Name = "ss1"
            val source1Tag = "!ss1"

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create sources:
            val sourceCConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> source1Tag,
                    "n" -> source1Name))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalSources",
                expectedTrunk1Key,
                sourceCConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm source states:
            var sourceReports = (Json.parse(serviceResult._results.head) \ "rss").as[Seq[JsObject]]
            assert(sourceReports.size == 1)
            for (report <- sourceReports)
            {
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSource1Key` =>
                    case _ => assert(false)
                }
                (report \ "m" \ "n").as[String] match
                {
                    case `source1Name` =>
                    case _ => assert(false)
                }
            }

            // Update sources:
            val sourceCRenamed = source1Name + "_renamed"
            val sourceCUpdate = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSource1Key,
                    "n" -> sourceCRenamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalSources",
                expectedTrunk1Key,
                sourceCUpdate)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get source reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedSource1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalSources",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm source states:
            sourceReports = (Json.parse(serviceResult._results.head) \ "rss").as[Seq[JsObject]]
            assert(sourceReports.size == 1)
            for (report <- sourceReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSource1Key` =>
                        assert((report \ "m" \ "n").as[String] == sourceCRenamed)
                    case _ => assert(false)
                }

            // Destroy sources:
            assert(CountTrunkElements(f, expectedTrunk1Key, "ss") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 3)
            val sourceCDestructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSource1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalSources",
                expectedTrunk1Key,
                sourceCDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "ss") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 2)

            // Get source reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSource1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalSources",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm source states (should be none reporting):
            sourceReports = (Json.parse(serviceResult._results.head) \ "rss").as[Seq[JsObject]]
            assert(sourceReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }

    //
    // Signal Sinks:
    //
    it must "pass basic signal sink tests" in
        { f =>
            val expectedSink1Key = "sk1~sk"
            val sink1Name = "sk1"
            val sink1Tag = "!sk1"

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create sinks:
            val sinkCConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> sink1Tag,
                    "n" -> sink1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalSinks",
                expectedTrunk1Key,
                sinkCConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm sink states:
            var sinkReports = (Json.parse(serviceResult._results.head) \ "rsk").as[Seq[JsObject]]
            assert(sinkReports.size == 1)
            for (report <- sinkReports)
            {
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSink1Key` =>
                    case _ => assert(false)
                }
                (report \ "m" \ "n").as[String] match
                {
                    case `sink1Name` =>
                    case _ => assert(false)
                }
                (report \ "m" \ "m").as[String] match
                {
                    case "c" =>
                    case "d" =>
                    case _ => assert(false)
                }
            }

            // Update sinks:
            val sinkCRenamed = sink1Name + "_renamed"
            val sinkCUpdate = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSink1Key,
                    "n" -> sinkCRenamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalSinks",
                expectedTrunk1Key,
                sinkCUpdate)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get sink reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedSink1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalSinks",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm sink states:
            sinkReports = (Json.parse(serviceResult._results.head) \ "rsk").as[Seq[JsObject]]
            assert(sinkReports.size == 1)
            for (report <- sinkReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSink1Key` =>
                        assert((report \ "m" \ "n").as[String] == sinkCRenamed)
                    case _ => assert(false)
                }

            // Destroy sinks:
            assert(CountTrunkElements(f, expectedTrunk1Key, "sk") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 3)
            val sinkCDestructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSink1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalSinks",
                expectedTrunk1Key,
                sinkCDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sk") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 2)

            // Get sink reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSink1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalSinks",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm sink states (should be none reporting):
            sinkReports = (Json.parse(serviceResult._results.head) \ "rsk").as[Seq[JsObject]]
            assert(sinkReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }

    //
    // Signal Links:
    //
    it must "pass basic signal link tests" in
        { f =>
            val expectedLink1Key = "sl1~sl"
            val expectedSink1Key = "sk1~sk"
            val expectedSource1Key = "ss1~ss"
            val link1Name = "sl1"
            val link1Tag = "!sl1"
            val sink1Name = "sk1"
            val sink1Tag = "!sk1"
            val source1Name = "ss1"
            val source1Tag = "!ss1"

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create sources:
            val sourceCConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> source1Tag,
                    "n" -> source1Name))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalSources",
                expectedTrunk1Key,
                sourceCConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create sinks:
            val sinkCConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> sink1Tag,
                    "n" -> sink1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalSinks",
                expectedTrunk1Key,
                sinkCConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create links:
            val linkCConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> link1Tag,
                    "n" -> link1Name,
                    "m" -> "c"),
                "r" -> Json.obj(
                    "ss" -> expectedSource1Key,
                    "sk" -> expectedSink1Key))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalLinks",
                expectedTrunk1Key,
                linkCConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm link states:
            var linkReports = (Json.parse(serviceResult._results.head) \ "rsl").as[Seq[JsObject]]
            assert(linkReports.size == 1)
            for (linkReport <- linkReports)
            {
                (linkReport \ "m" \ "k").as[String] match
                {
                    case `expectedLink1Key` =>
                    case _ => assert(false)
                }
                (linkReport \ "m" \ "n").as[String] match
                {
                    case `link1Name` =>
                    case _ => assert(false)
                }
                var sourceReportsOpt = (linkReport \ "rss").asOpt[Seq[JsObject]]
                if (sourceReportsOpt.isDefined)
                {
                    for (sourceReport <- sourceReportsOpt.get)
                    {
                        (sourceReport \ "m" \ "k").as[String] match
                        {
                            case `expectedSource1Key` =>
                            case _ => assert(false)
                        }
                    }
                }
                val sinkReports = (linkReport \ "rsk").as[Seq[JsObject]]
                assert(sinkReports.size == 1)
                for (sinkReport <- sinkReports)
                {
                    (sinkReport \ "m" \ "k").as[String] match
                    {
                        case `expectedSink1Key` =>
                        case _ => assert(false)
                    }
                }
                (linkReport \ "m" \ "m").as[String] match
                {
                    case "c" =>
                    case "d" =>
                    case _ => assert(false)
                }
            }

            // Update links:
            val linkCRenamed = link1Name + "_renamed"
            val linkCUpdate = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedLink1Key,
                    "n" -> linkCRenamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalLinks",
                expectedTrunk1Key,
                linkCUpdate)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get link reports:
            var query = Json.obj(
                "k" -> Json.arr(
                expectedLink1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalLinks",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm link states:
            linkReports = (Json.parse(serviceResult._results.head) \ "rsl").as[Seq[JsObject]]
            assert(linkReports.size == 1)
            for (report <- linkReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedLink1Key` =>
                        assert((report \ "m" \ "n").as[String] == linkCRenamed)
                    case _ => assert(false)
                }

            // Destroy links:
            assert(CountTrunkElements(f, expectedTrunk1Key, "ss") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sl") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sk") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 5)
            val linkCDestructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedLink1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalLinks",
                expectedTrunk1Key,
                linkCDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "ss") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sl") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sk") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 3)

            // Get link reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedLink1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalLinks",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm link states (should be none reporting):
            linkReports = (Json.parse(serviceResult._results.head) \ "rsl").as[Seq[JsObject]]
            assert(linkReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }

    //
    // Signal Taps:
    //
    it must "pass basic signal tap tests" in
        { f =>
            val expectedLink1Key = "sl1~sl"
            val expectedSink1Key = "sk1~sk"
            val expectedTap1Key = "st1~st"
            val expectedTap1SinkKey = "st1.sk~sk"
            val expectedTap1SourceKey = "st1.ss~ss"
            val link1Name = "sl1"
            val link1Tag = "!sl1"
            val tap1Name = "st1"
            val tap1Tag = "!st1"
            val sink1Name = "sk1"
            val sink1Tag = "!sk1"

            //
            // Test entity creation:
            //

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create tap:
            val tap1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> tap1Tag,
                    "n" -> tap1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalTaps",
                expectedTrunk1Key,
                tap1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm tap states:
            var tapReports = (Json.parse(serviceResult._results.head) \ "rst").as[Seq[JsObject]]
            assert(tapReports.size == 1)
            for (tapReport <- tapReports)
            {
                (tapReport \ "m" \ "k").as[String] match
                {
                    case `expectedTap1Key` =>
                    case _ => assert(false)
                }
                (tapReport \ "m" \ "n").as[String] match
                {
                    case `tap1Name` =>
                    case _ => assert(false)
                }
                var sourceReportsOpt = (tapReport \ "rss").asOpt[Seq[JsObject]]
                if (sourceReportsOpt.isDefined)
                {
                    for (sourceReport <- sourceReportsOpt.get)
                    {
                        (sourceReport \ "m" \ "k").as[String] match
                        {
                            case `expectedTap1SourceKey` =>
                            case _ => assert(false)
                        }
                    }
                }
                val sinkReports = (tapReport \ "rsk").as[Seq[JsObject]]
                assert(sinkReports.size == 1)
                for (sinkReport <- sinkReports)
                {
                    (sinkReport \ "m" \ "k").as[String] match
                    {
                        case `expectedTap1SinkKey` =>
                        case _ => assert(false)
                    }
                }
                (tapReport \ "m" \ "m").as[String] match
                {
                    case "c" =>
                    case "d" =>
                    case _ => assert(false)
                }
            }

            // Create destination sink:
            val sink1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> sink1Tag,
                    "n" -> sink1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalSinks",
                expectedTrunk1Key,
                sink1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create link between tap source and destination sink:
            val link1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> link1Tag,
                    "n" -> link1Name,
                    "m" -> "c"),
                "r" -> Json.obj(
                    "ss" -> expectedTap1SourceKey,
                    "sk" -> expectedSink1Key))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalLinks",
                expectedTrunk1Key,
                link1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            //
            // Test entity updates:
            //

            // Update tap:
            val tap1Renamed = tap1Name + "_renamed"
            var tap1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedTap1Key,
                    "n" -> tap1Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalTaps",
                expectedTrunk1Key,
                tap1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm tap states:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedTap1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalTaps",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            tapReports = (Json.parse(serviceResult._results.head) \ "rst").as[Seq[JsObject]]
            assert(tapReports.size == 1)
            for (report <- tapReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedTap1Key` =>
                        assert((report \ "m" \ "n").as[String] == tap1Renamed)
                    case _ => assert(false)
                }

            //
            // Test signal propagation:
            //

            // Initialize tap sink signal:
            var sink1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedTap1SinkKey),
                "s" -> Json.obj(
                    "s" -> "0.0"))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalSinks",
                expectedTrunk1Key,
                sink1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm tap sink state:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedTap1Key),
                "s" -> Json.arr("cs"))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalTaps",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            tapReports = (Json.parse(serviceResult._results.head) \ "rst").as[Seq[JsObject]]
            assert(tapReports.size == 1)
            for (report <- tapReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedTap1Key` =>
                        val sinkReports = (report \ "rsk").as[Seq[JsObject]]
                        assert(sinkReports.size == 1)
                        for (sinkReport <- sinkReports)
                        {
                            val trapReport = (sinkReport \ "s" \ "t").as[JsObject]
                            (trapReport \ expectedTap1SinkKey \ "v").as[String] match
                            {
                                case "0.000" =>
                                case _ => assert(false)
                            }
                        }
                    case _ => assert(false)
                }

            // Confirm destination sink states:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSink1Key),
                "s" -> Json.arr("s"))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalSinks",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            var sinkReports = (Json.parse(serviceResult._results.head) \ "rsk").as[Seq[JsObject]]
            assert(sinkReports.size == 1)
            for (sinkReport <- sinkReports)
                (sinkReport \ "m" \ "k").as[String] match
                {
                    case `expectedSink1Key` =>
                        val trapReport = (sinkReport \ "s" \ "t").as[JsObject]
                        (trapReport \ expectedLink1Key \ "v").as[String] match
                        {
                            case "0.000" =>
                            case _ => assert(false)
                        }
                    case _ => assert(false)
                }

            // Update tap sink signal:
            sink1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedTap1SinkKey),
                "s" -> Json.obj(
                    "s" -> 1.0.toString))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalSinks",
                expectedTrunk1Key,
                sink1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm destination sink state:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSink1Key),
                "s" -> Json.arr("s"))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalSinks",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            sinkReports = (Json.parse(serviceResult._results.head) \ "rsk").as[Seq[JsObject]]
            assert(sinkReports.size == 1)
            for (sinkReport <- sinkReports)
                (sinkReport \ "m" \ "k").as[String] match
                {
                    case `expectedSink1Key` =>
                        val trapReport = (sinkReport \ "s" \ "t").as[JsObject]
                        (trapReport \ expectedLink1Key \ "v").as[String] match
                        {
                            case "1.000" =>
                            case _ => assert(false)
                        }
                    case _ => assert(false)
                }

            //
            // Test entity destruction:
            //

            // Destroy tap:
            assert(CountTrunkElements(f, expectedTrunk1Key, "st") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sk") == 2)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sl") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key, "ss") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 7)
            val tap2Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedTap1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalTaps",
                expectedTrunk1Key,
                tap2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "st") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sk") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key, "sl") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key, "ss") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 2)

            // Get tap report:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedTap1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalTaps",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm tap state (should be none reporting):
            tapReports = (Json.parse(serviceResult._results.head) \ "rst").as[Seq[JsObject]]
            assert(tapReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }


    //
    // Signal Inputs:
    //
    it must "pass basic signal input tests" in
        { f =>
            val expectedInput1Key = "smi1~smi"
            val expectedTap1Key = "smi1.sp.st~st"
            val input1Name = "smi1"
            val input1Tag = "!smi1"

            //
            // Test entity creation:
            //

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create input:
            val inputConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> input1Tag,
                    "n" -> input1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalInputs",
                expectedTrunk1Key,
                inputConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm input state:
            var inputReports = (Json.parse(serviceResult._results.head) \ "rsmi").as[Seq[JsObject]]
            var inputReport = inputReports.head
            (inputReport \ "m" \ "k").as[String] match
            {
                case `expectedInput1Key` =>
                case _ => assert(false)
            }
            (inputReport \ "m" \ "n").as[String] match
            {
                case `input1Name` =>
                case _ => assert(false)
            }
            var tapReports = (inputReport \ "rst").as[Seq[JsObject]]
            var tapReport = tapReports.head
            (tapReport \ "m" \ "k").as[String] match
            {
                case `expectedTap1Key` =>
                case _ => assert(false)
            }
            (inputReport \ "m" \ "m").as[String] match
            {
                case "c" =>
                case _ => assert(false)
            }

            //
            // Test entity updates:
            //

            // Update input:
            val input1Renamed = input1Name + "_renamed"
            var input1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInput1Key,
                    "n" -> input1Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalInputs",
                expectedTrunk1Key,
                input1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm input state:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedInput1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalInputs",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            inputReports = (Json.parse(serviceResult._results.head) \ "rsmi").as[Seq[JsObject]]
            inputReport = inputReports.head
            (inputReport \ "m" \ "k").as[String] match
            {
                case `expectedInput1Key` =>
                    assert((inputReport \ "m" \ "n").as[String] == input1Renamed)
                case _ => assert(false)
            }
            
            //
            // Test entity destruction:
            //

            // Destroy inputs:
            assert(CountTrunkElements(f, expectedTrunk1Key, "smi") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 7)
            val inputDestructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInput1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalInputs",
                expectedTrunk1Key,
                inputDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "smi") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 6)

            // Get input report:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedInput1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalInputs",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm input state (should be none reporting):
            inputReports = (Json.parse(serviceResult._results.head) \ "rsmi").as[Seq[JsObject]]
            assert(inputReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 0)
        }


    //
    // Signal Bridges:
    //
    it must "pass basic signal bridge tests" in
        { f =>
            val expectedBridge1Key = "smb1~smb"
            val expectedInput1Key = "smi1~smi"
            val expectedTap1Key = "smb1.st~st"
            val bridge1Name = "smb1"
            val bridge1Tag = "!smb1"
            val input1Name = "smi1"
            val input1Tag = "!smi1"

            //
            // Test entity creation:
            //

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create input:
            val inputConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> input1Tag,
                    "n" -> input1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalInputs",
                expectedTrunk1Key,
                inputConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm input state:
            var inputReports = (Json.parse(serviceResult._results.head) \ "rsmi").as[Seq[JsObject]]
            var inputReport = inputReports.head
            (inputReport \ "m" \ "k").as[String] match
            {
                case `expectedInput1Key` =>
                case _ => assert(false)
            }

            // Create bridge:
            val bridgeConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> bridge1Tag,
                    "n" -> bridge1Name,
                    "m" -> "c"),
                "r" -> Json.obj(
                    "sm" -> expectedInput1Key))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalBridges",
                expectedTrunk1Key,
                bridgeConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm bridge state:
            var bridgeReports = (Json.parse(serviceResult._results.head) \ "rsmb").as[Seq[JsObject]]
            var bridgeReport = bridgeReports.head
            (bridgeReport \ "m" \ "k").as[String] match
            {
                case `expectedBridge1Key` =>
                case _ => assert(false)
            }
            (bridgeReport \ "m" \ "n").as[String] match
            {
                case `bridge1Name` =>
                case _ => assert(false)
            }
            var tapReports = (bridgeReport \ "rst").as[Seq[JsObject]]
            var tapReport = tapReports.head
            (tapReport \ "m" \ "k").as[String] match
            {
                case `expectedTap1Key` =>
                case _ => assert(false)
            }
            (bridgeReport \ "m" \ "m").as[String] match
            {
                case "c" =>
                case _ => assert(false)
            }

            //
            // Test entity updates:
            //

            // Update bridge:
            val bridge1Renamed = bridge1Name + "_renamed"
            var bridge1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedBridge1Key,
                    "n" -> bridge1Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalBridges",
                expectedTrunk1Key,
                bridge1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm bridge state:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedBridge1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalBridges",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            bridgeReports = (Json.parse(serviceResult._results.head) \ "rsmb").as[Seq[JsObject]]
            bridgeReport = bridgeReports.head
            (bridgeReport \ "m" \ "k").as[String] match
            {
                case `expectedBridge1Key` =>
                    assert((bridgeReport \ "m" \ "n").as[String] == bridge1Renamed)
                case _ => assert(false)
            }

            //
            // Test signal propagation:
            //

            // Put input's signal:
            val input1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInput1Key),
                "s" -> Json.obj(
                    "s" -> "1.0"))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalInputs",
                expectedTrunk1Key,
                input1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm bridge state:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedBridge1Key),
                "s" -> Json.arr(
                    "cs"))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalBridges",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            bridgeReports = (Json.parse(serviceResult._results.head) \ "rsmb").as[Seq[JsObject]]
            bridgeReport = bridgeReports.head
            (bridgeReport \ "s" \ "s" \ "v").as[String] match
            {
                case "1.000" =>
                case _ => assert(false)
            }

            //
            // Test entity destruction:
            //

            // Destroy bridges:
            assert(CountTrunkElements(f, expectedTrunk1Key, "smb") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 12)
            val bridgeDestructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedBridge1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalBridges",
                expectedTrunk1Key,
                bridgeDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key, "smb") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 9)

            // Get bridge report:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedBridge1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalBridges",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm bridge state (should be none reporting):
            bridgeReports = (Json.parse(serviceResult._results.head) \ "rsmb").as[Seq[JsObject]]
            assert(bridgeReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 0)
        }

    //
    // Signal Outputs:
    //
    it must "pass basic signal output tests" in
        { f =>
            val expectedOutput1Key = "smo1~smo"
            val expectedInput1Key = "smi1~smi"
            val expectedTap1Key = "smo1.st~st"
            val output1Name = "smo1"
            val output1Tag = "!smo1"
            val input1Name = "smi1"
            val input1Tag = "!smi1"

            //
            // Test entity creation:
            //

            // Attempt to destroy any prior trunk (might fail):
            val trunkDestructor = Json.obj("m" -> Json.obj(
                "k" -> expectedTrunk1Key))
            var serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.TrunkUnknown)

            // Create trunk:
            val trunkConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> trunk1Tag,
                    "n" -> trunk1Name))
            serviceResult = f.cellDirector.CallService("CreateTrunks", trunkConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create input:
            val inputConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> input1Tag,
                    "n" -> input1Name,
                    "m" -> "c"))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalInputs",
                expectedTrunk1Key,
                inputConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm input state:
            var inputReports = (Json.parse(serviceResult._results.head) \ "rsmi").as[Seq[JsObject]]
            var inputReport = inputReports.head
            (inputReport \ "m" \ "k").as[String] match
            {
                case `expectedInput1Key` =>
                case _ => assert(false)
            }

            // Create output:
            val outputConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> output1Tag,
                    "n" -> output1Name,
                    "m" -> "c"),
                "r" -> Json.obj(
                    "sm" -> expectedInput1Key))
            serviceResult = f.cellDirector.CallService(
                "CreateSignalOutputs",
                expectedTrunk1Key,
                outputConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm output state:
            var outputReports = (Json.parse(serviceResult._results.head) \ "rsmo").as[Seq[JsObject]]
            var outputReport = outputReports.head
            (outputReport \ "m" \ "k").as[String] match
            {
                case `expectedOutput1Key` =>
                case _ => assert(false)
            }
            (outputReport \ "m" \ "n").as[String] match
            {
                case `output1Name` =>
                case _ => assert(false)
            }
            var tapReports = (outputReport \ "rst").as[Seq[JsObject]]
            var tapReport = tapReports.head
            (tapReport \ "m" \ "k").as[String] match
            {
                case `expectedTap1Key` =>
                case _ => assert(false)
            }
            (outputReport \ "m" \ "m").as[String] match
            {
                case "c" =>
                case _ => assert(false)
            }
        
            //
            // Test entity updates:
            //

            // Update output:
            val output1Renamed = output1Name + "_renamed"
            var output1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedOutput1Key,
                    "n" -> output1Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalOutputs",
                expectedTrunk1Key,
                output1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm output state:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedOutput1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalOutputs",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            outputReports = (Json.parse(serviceResult._results.head) \ "rsmo").as[Seq[JsObject]]
            outputReport = outputReports.head
            (outputReport \ "m" \ "k").as[String] match
            {
                case `expectedOutput1Key` =>
                    assert((outputReport \ "m" \ "n").as[String] == output1Renamed)
                case _ => assert(false)
            }

            //
            // Test signal propagation:
            //

            // Put input's signal:
            val input1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedInput1Key),
                "s" -> Json.obj(
                    "s" -> "1.0"))
            serviceResult = f.cellDirector.CallService(
                "UpdateSignalInputs",
                expectedTrunk1Key,
                input1Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm output state:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedOutput1Key),
                "s" -> Json.arr(
                    "cs"))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalOutputs",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            outputReports = (Json.parse(serviceResult._results.head) \ "rsmo").as[Seq[JsObject]]
            outputReport = outputReports.head
            (outputReport \ "s" \ "s" \ "v").as[String] match
            {
                case "1.000" =>
                case _ => assert(false)
            }

            //
            // Test entity destruction:
            //

            // Destroy outputs:
//var r = ReportTrunkPlants(f, expectedTrunk1Key)
            assert(CountTrunkElements(f, expectedTrunk1Key, "smo") == 1)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 12)
            val outputDestructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedOutput1Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySignalOutputs",
                expectedTrunk1Key,
                outputDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
//r = ReportTrunkPlants(f, expectedTrunk1Key)
            assert(CountTrunkElements(f, expectedTrunk1Key, "smo") == 0)
            assert(CountTrunkElements(f, expectedTrunk1Key) == 9)

            // Get output report:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedOutput1Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSignalOutputs",
                expectedTrunk1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm output state (should be none reporting):
            outputReports = (Json.parse(serviceResult._results.head) \ "rsmo").as[Seq[JsObject]]
            assert(outputReports.isEmpty)

            // Destroy trunk:
            serviceResult = f.cellDirector.CallService("DestroyTrunks", trunkDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
        }

    //
    // Fields:
    //
    it must "pass basic field tests" in
        { f =>
            // Attempt to destroy field (should fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field state:
            var fieldReports = (Json.parse(serviceResult._results.head) \ "rf").as[Seq[JsObject]]
            var fieldReport = fieldReports.head.as[JsObject]
            assert((fieldReport \ "m" \ "k").as[String] == expectedField1Key)
            assert((fieldReport \ "m" \ "t").as[String] == field1Tag)

            // Update field:
            val field1Renamed = field1Tag + "_renamed"
            val fieldUpdate = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedField1Key,
                    "n" -> field1Renamed))
            serviceResult = f.cellDirector.CallService("UpdateFields", fieldUpdate)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field report:
            var query = Json.obj(
                "k" -> Json.arr(expectedField1Key))
            serviceResult = f.cellDirector.CallService("ReportFields", query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field state:
            fieldReports = (Json.parse(serviceResult._results.head) \ "rf").as[Seq[JsObject]]
            fieldReport = fieldReports.head.as[JsObject]
            assert((fieldReport \ "m" \ "k").as[String] == expectedField1Key)
            assert((fieldReport \ "m" \ "n").as[String] == field1Renamed)

            // Destroy field:
            serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field report (should be none reporting):
            serviceResult = f.cellDirector.CallService("ReportFields", query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            fieldReports = (Json.parse(serviceResult._results.head) \ "rf").as[Seq[JsObject]]
            assert(fieldReports.isEmpty)
        }

    //
    // Field Emitters:
    //
    it must "pass basic field emitter tests" in
        { f =>
            val expectedFieldEmitter1Key = "fe1~fe"
            val expectedFieldEmitter2Key = "fe2~fe"
            val fieldEmitter1Tag = "!fe1"
            val fieldEmitter1Name = "fe1"
            val fieldEmitter2Tag = "!fe2"
            val fieldEmitter2Name = "fe2"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create field emitters:
            val fieldEmitter1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> fieldEmitter1Tag,
                    "n" -> fieldEmitter1Name))
            val fieldEmitter2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> fieldEmitter2Tag,
                    "n" -> fieldEmitter2Name))
            serviceResult = f.cellDirector.CallService(
                "CreateFieldEmitters",
                expectedField1Key,
                fieldEmitter1Constructor,
                fieldEmitter2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field emitter states:
            var fieldEmitterReports = (Json.parse(serviceResult._results.head) \ "rfe").as[Seq[JsObject]]
            assert(fieldEmitterReports.size == 2)
            for (report <- fieldEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedFieldEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldEmitter1Name)
                    case `expectedFieldEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldEmitter2Name)
                    case _ => assert(false)
                }

            // Update field emitters:
            val fieldEmitter1Renamed = fieldEmitter1Name + "_renamed"
            val fieldEmitter2Renamed = fieldEmitter2Name + "_renamed"
            val fieldEmitter1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedFieldEmitter1Key,
                    "n" -> fieldEmitter1Renamed))
            val fieldEmitter2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedFieldEmitter2Key,
                    "n" -> fieldEmitter2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateFieldEmitters",
                expectedField1Key,
                fieldEmitter1Update,
                fieldEmitter2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field emitter reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedFieldEmitter1Key,
                    expectedFieldEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportFieldEmitters",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field emitter states:
            fieldEmitterReports = (Json.parse(serviceResult._results.head) \ "rfe").as[Seq[JsObject]]
            assert(fieldEmitterReports.size == 2)
            for (report <- fieldEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedFieldEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldEmitter1Renamed)
                    case `expectedFieldEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldEmitter2Renamed)
                    case _ => assert(false)
                }

            // Call field emitters:
            val fieldEmitter1Call = Json.obj(
                "k" -> expectedFieldEmitter1Key,
                "m" -> Json.obj(
                    "set_channel_ceiling" -> Json.arr("_", "0.75")))
            val fieldEmitter2Call = Json.obj(
                "k" -> expectedFieldEmitter1Key,
                "m" -> Json.obj(
                    "create_channel" -> Json.arr("1")))
            val fieldEmitter3Call = Json.obj(
                "k" -> expectedFieldEmitter1Key,
                "m" -> Json.obj(
                    "destroy_channel" -> Json.arr("1")))
            val fieldEmitter4Call = Json.obj(
                "k" -> expectedFieldEmitter1Key,
                "m" -> Json.obj(
                    "set_channel_poles" -> Json.arr("_", "3")))
//            val fieldEmitter5Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_loudness_envelope" -> Json.arr("a000000a")))
//            val fieldEmitter6Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_loudness_envelope" -> Json.arr("a112233a")))
//            val fieldEmitter7Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_shape_envelope" -> Json.arr("5aa50")))
//            val fieldEmitter8Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_shape_envelope" -> Json.arr("a")))
//            val fieldEmitter9Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_shape_envelope" -> Json.arr("000000a")))
//            val fieldEmitter10Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_shape_envelope" -> Json.arr("0000000a")))
//            val fieldEmitter11Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_shape_envelope" -> Json.arr("a000000a")))
//            val fieldEmitter12Call = Json.obj(
//                "k" -> expectedFieldEmitter1Key,
//                "m" -> Json.obj(
//                    "set_shape_envelope" -> Json.arr("a112233a")))
            serviceResult = f.cellDirector.CallService(
                "CallFieldEmitters",
                expectedField1Key,
                fieldEmitter1Call,
                fieldEmitter2Call,
                fieldEmitter3Call,
                fieldEmitter4Call
//                fieldEmitter5Call,
//                fieldEmitter6Call,
//                fieldEmitter7Call,
//                fieldEmitter8Call,
//                fieldEmitter9Call,
//                fieldEmitter10Call,
//                fieldEmitter11Call,
//                fieldEmitter12Call)
            )
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field emitter reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedFieldEmitter1Key,
                    expectedFieldEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportFieldEmitters",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field emitter states:
            fieldEmitterReports = (Json.parse(serviceResult._results.head) \ "rfe").as[Seq[JsObject]]
            assert(fieldEmitterReports.size == 2)
            for (report <- fieldEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedFieldEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldEmitter1Renamed)
                    case `expectedFieldEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldEmitter2Renamed)
                    case _ => assert(false)
                }

            // Destroy field emitters:
            val fieldEmitter1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedFieldEmitter1Key))
            val fieldEmitter2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedFieldEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroyFieldEmitters",
                expectedField1Key,
                fieldEmitter1Destructor,
                fieldEmitter2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field emitter reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedFieldEmitter1Key,
                    expectedFieldEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportFieldEmitters",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            fieldEmitterReports = (Json.parse(serviceResult._results.head) \ "rfe").as[Seq[JsObject]]
            assert(fieldEmitterReports.isEmpty)
        }

    //
    // Field Oscillators:
    //
    it must "pass basic field oscillator tests" in
        { f =>
            val expectedFieldEmitter1Key = "fe1~fe"
            val expectedFieldOscillator1Key = "fo1~fo"
            val expectedFieldOscillator2Key = "fo2~fo"
            val fieldEmitter1Tag = "!fe1"
            val fieldEmitter1Name = "fe1"
            val fieldOscillator1Tag = "!fo1"
            val fieldOscillator1Name = "fo1"
            val fieldOscillator2Tag = "!fo2"
            val fieldOscillator2Name = "fo2"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create field emitter:
            val fieldEmitterConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> fieldEmitter1Tag,
                    "n" -> fieldEmitter1Name))
            serviceResult = f.cellDirector.CallService(
                "CreateFieldEmitters",
                expectedField1Key,
                fieldEmitterConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create field oscillators:
            val fieldOscillator1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> fieldOscillator1Tag,
                    "n" -> fieldOscillator1Name),
                "a" -> Json.obj(
                    "dc" -> Json.obj(
                        "tc" -> "0",
                        "dpo" -> Json.obj(
                            "w" -> "default.tws"))))
            val fieldOscillator2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> fieldOscillator2Tag,
                    "n" -> fieldOscillator2Name),
                "a" -> Json.obj(
                    "dc" -> Json.obj(
                        "tc" -> "1",
                        "dpo" -> Json.obj(
                            "w" -> "default.tws"))))
            serviceResult = f.cellDirector.CallService(
                "CreateFieldOscillators",
                expectedField1Key,
                expectedFieldEmitter1Key,
                fieldOscillator1Constructor,
                fieldOscillator2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field oscillator states:
            var fieldOscillatorReports = (Json.parse(serviceResult._results.head) \ "rfo").as[Seq[JsObject]]
            assert(fieldOscillatorReports.size == 2)
            for (report <- fieldOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedFieldOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldOscillator1Name)
                    case `expectedFieldOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldOscillator2Name)
                    case _ => assert(false)
                }

            // Update field oscillators:
            val fieldOscillator1Renamed = fieldOscillator1Name + "_renamed"
            val fieldOscillator2Renamed = fieldOscillator2Name + "_renamed"
            val fieldOscillator1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedFieldOscillator1Key,
                    "n" -> fieldOscillator1Renamed))
            val fieldOscillator2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedFieldOscillator2Key,
                    "n" -> fieldOscillator2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateFieldOscillators",
                expectedField1Key,
                fieldOscillator1Update,
                fieldOscillator2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field oscillator reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedFieldOscillator1Key,
                    expectedFieldOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportFieldOscillators",   // limited
                expectedField1Key,
                expectedFieldEmitter1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field oscillator states:
            fieldOscillatorReports = (Json.parse(serviceResult._results.head) \ "rfo").as[Seq[JsObject]]
            assert(fieldOscillatorReports.size == 2)
            for (report <- fieldOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedFieldOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldOscillator1Renamed)
                    case `expectedFieldOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldOscillator2Renamed)
                    case _ => assert(false)
                }
            
            // Call field oscillators:
            val fieldOscillator1Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_loudness_poles" -> Json.arr("5aa50")))
            val fieldOscillator2Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_loudness_poles" -> Json.arr("a")))
            val fieldOscillator3Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_loudness_poles" -> Json.arr("000000a")))
            val fieldOscillator4Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_loudness_poles" -> Json.arr("0000000a")))
            val fieldOscillator5Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_loudness_poles" -> Json.arr("a000000a")))
            val fieldOscillator6Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_loudness_poles" -> Json.arr("a112233a")))
            val fieldOscillator7Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_shape_poles" -> Json.arr("5aa50")))
            val fieldOscillator8Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_shape_poles" -> Json.arr("a")))
            val fieldOscillator9Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_shape_poles" -> Json.arr("000000a")))
            val fieldOscillator10Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_shape_poles" -> Json.arr("0000000a")))
            val fieldOscillator11Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_shape_poles" -> Json.arr("a000000a")))
            val fieldOscillator12Call = Json.obj(
                "k" -> expectedFieldOscillator1Key,
                "m" -> Json.obj(
                    "set_shape_poles" -> Json.arr("a112233a")))
            serviceResult = f.cellDirector.CallService(
                "CallFieldOscillators",
                expectedField1Key,
                fieldOscillator1Call,
                fieldOscillator2Call,
                fieldOscillator3Call,
                fieldOscillator4Call,
                fieldOscillator5Call,
                fieldOscillator6Call,
                fieldOscillator7Call,
                fieldOscillator8Call,
                fieldOscillator9Call,
                fieldOscillator10Call,
                fieldOscillator11Call,
                fieldOscillator12Call)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field oscillator reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedFieldOscillator1Key,
                    expectedFieldOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportFieldOscillators",   // limited
                expectedField1Key,
                expectedFieldEmitter1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm field oscillator states:
            fieldOscillatorReports = (Json.parse(serviceResult._results.head) \ "rfo").as[Seq[JsObject]]
            assert(fieldOscillatorReports.size == 2)
            for (report <- fieldOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedFieldOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldOscillator1Renamed)
                    case `expectedFieldOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == fieldOscillator2Renamed)
                    case _ => assert(false)
                }
            
            // Destroy field oscillators:
            val fieldOscillator1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedFieldOscillator1Key))
            val fieldOscillator2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedFieldOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroyFieldOscillators",
                expectedField1Key,
                fieldOscillator1Destructor,
                fieldOscillator2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get field oscillator reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedFieldOscillator1Key,
                    expectedFieldOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportFieldOscillators",   // unlimited
                expectedField1Key,
                "~~fe",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            fieldOscillatorReports = (Json.parse(serviceResult._results.head) \ "rfo").as[Seq[JsObject]]
            assert(fieldOscillatorReports.isEmpty)
        }

    //
    // Subjects:
    //
    it must "pass basic subject tests" in
        { f =>
            val subject1Name = "s1"
            val subject1Tag = "!s1"
            val subject2Name = "s2"
            val subject2Tag = "!s2"
            val expectedSubject1Key = "s1~s"
            val expectedSubject2Key = "s2~s"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subjects:
            val subject1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subject1Tag,
                    "n" -> subject1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            val subject2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subject2Tag,
                    "n" -> subject2Name),
                "s" -> Json.obj(
                    "p" -> Json.arr((-0.5).toString, (-0.5).toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjects",
                expectedField1Key,
                subject1Constructor,
                subject2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject states:
            var subjectReports = (Json.parse(serviceResult._results.head) \ "rs").as[Seq[JsObject]]
            assert(subjectReports.size == 2)
            for (report <- subjectReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubject1Key` =>
                        assert((report \ "m" \ "n").as[String] == subject1Name)
                    case `expectedSubject2Key` =>
                        assert((report \ "m" \ "n").as[String] == subject2Name)
                    case _ => assert(false)
                }

            // Update subjects:
            val subject1Renamed = subject1Tag + "_renamed"
            val subject2Renamed = subject2Tag + "_renamed"
            val subject1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubject1Key,
                    "n" -> subject1Renamed))
            val subject2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubject2Key,
                    "n" -> subject2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSubjects",
                expectedField1Key,
                subject1Update,
                subject2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedSubject1Key,
                    expectedSubject2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjects",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject states:
            subjectReports = (Json.parse(serviceResult._results.head) \ "rs").as[Seq[JsObject]]
            assert(subjectReports.size == 2)
            for (report <- subjectReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubject1Key` =>
                        assert((report \ "m" \ "n").as[String] == subject1Renamed)
                    case `expectedSubject2Key` =>
                        assert((report \ "m" \ "n").as[String] == subject2Renamed)
                    case _ => assert(false)
                }

            // Destroy subjects:
            val subject1Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubject1Key))
            val subject2Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubject2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySubjects",
                expectedField1Key,
                subject1Destructor,
                subject2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSubject1Key,
                    expectedSubject2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjects",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            subjectReports = (Json.parse(serviceResult._results.head) \ "rs").as[Seq[JsObject]]
            assert(subjectReports.isEmpty)
        }

    //
    // Subject Emitters:
    //
    it must "pass basic subject emitter tests" in
        { f =>
            val subject1Name = "s1"
            val subject1Tag = "!s1"
            val subjectEmitter1Name = "se1"
            val subjectEmitter1Tag = "!se1"
            val subjectEmitter2Name = "se2"
            val subjectEmitter2Tag = "!se2"
            val expectedSubject1Key = "s1~s"
            val expectedSubjectEmitter1Key = "se1~se"
            val expectedSubjectEmitter2Key = "se2~se"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject:
            val subject1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subject1Tag,
                    "n" -> subject1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjects",
                expectedField1Key,
                subject1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject emitters:
            val subjectEmitter1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectEmitter1Tag,
                    "n" -> subjectEmitter1Name))
            val subjectEmitter2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectEmitter2Tag,
                    "n" -> subjectEmitter2Name))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjectEmitters",
                expectedField1Key,
                expectedSubject1Key,
                subjectEmitter1Constructor,
                subjectEmitter2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject emitter states:
            var subjectEmitterReports = (Json.parse(serviceResult._results.head) \ "rse").as[Seq[JsObject]]
            assert(subjectEmitterReports.size == 2)
            for (report <- subjectEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubjectEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter1Name)
                    case `expectedSubjectEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter2Name)
                    case _ => assert(false)
                }

            // Update subject emitters:
            val subjectEmitter1Renamed = subjectEmitter1Name + "_renamed"
            val subjectEmitter2Renamed = subjectEmitter2Name + "_renamed"
            val subjectEmitter1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubjectEmitter1Key,
                    "n" -> subjectEmitter1Renamed))
            val subjectEmitter2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubjectEmitter2Key,
                    "n" -> subjectEmitter2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSubjectEmitters",
                expectedField1Key,
                subjectEmitter1Update,
                subjectEmitter2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject emitter reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedSubjectEmitter1Key,
                    expectedSubjectEmitter2Key),
                "s" -> Json.arr("asc"))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjectEmitters", // limited
                expectedField1Key,
                expectedSubject1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject emitter states:
            subjectEmitterReports = (Json.parse(serviceResult._results.head) \ "rse").as[Seq[JsObject]]
            assert(subjectEmitterReports.size == 2)
            for (report <- subjectEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubjectEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter1Renamed)
                    case `expectedSubjectEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter2Renamed)
                    case _ => assert(false)
                }

            // Destroy subject emitters:
            val subjectEmitter1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedSubjectEmitter1Key))
            val subjectEmitter2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedSubjectEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySubjectEmitters",
                expectedField1Key,
                subjectEmitter1Destructor,
                subjectEmitter2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject emitter reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSubjectEmitter1Key,
                    expectedSubjectEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjectEmitters", // unlimited
                expectedField1Key,
                "~~s",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            subjectEmitterReports = (Json.parse(serviceResult._results.head) \ "rse").as[Seq[JsObject]]
            assert(subjectEmitterReports.isEmpty)
        }

    //
    // Subject Oscillators:
    //
    it must "pass basic subject oscillator tests" in
        { f =>
            val expectedSubject1Key = "s1~s"
            val expectedSubjectEmitter1Key = "se1~se"
            val expectedSubjectOscillator1Key = "so1~so"
            val expectedSubjectOscillator2Key = "so2~so"
            val subject1Tag = "!s1"
            val subject1Name = "s1"
            val subjectEmitter1Tag = "!se1"
            val subjectEmitter1Name = "se1"
            val subjectOscillator1Tag = "!so1"
            val subjectOscillator1Name = "so1"
            val subjectOscillator2Tag = "!so2"
            val subjectOscillator2Name = "so2"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject:
            val subject1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subject1Tag,
                    "n" -> subject1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjects",
                expectedField1Key,
                subject1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject emitter:
            val subjectEmitterConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectEmitter1Tag,
                    "n" -> subjectEmitter1Name))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjectEmitters",
                expectedField1Key,
                expectedSubject1Key,
                subjectEmitterConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject oscillators:
            val subjectOscillator1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectOscillator1Tag,
                    "n" -> subjectOscillator1Name),
                "a" -> Json.obj(
                    "dc" -> Json.obj(
                        "tc" -> "0",
                        "dpo" -> Json.obj(
                            "w" -> "default.tws"))))
            val subjectOscillator2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectOscillator2Tag,
                    "n" -> subjectOscillator2Name),
                "a" -> Json.obj(
                    "dc" -> Json.obj(
                        "tc" -> "1",
                        "dpo" -> Json.obj(
                            "w" -> "default.tws"))))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjectOscillators",
                expectedField1Key,
                expectedSubjectEmitter1Key,
                subjectOscillator1Constructor,
                subjectOscillator2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject oscillator states:
            var subjectOscillatorReports = (Json.parse(serviceResult._results.head) \ "rso").as[Seq[JsObject]]
            assert(subjectOscillatorReports.size == 2)
            for (report <- subjectOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubjectOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectOscillator1Name)
                    case `expectedSubjectOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectOscillator2Name)
                    case _ => assert(false)
                }

            // Update subject oscillators:
            val subjectOscillator1Renamed = subjectOscillator1Name + "_renamed"
            val subjectOscillator2Renamed = subjectOscillator2Name + "_renamed"
            val subjectOscillator1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubjectOscillator1Key,
                    "n" -> subjectOscillator1Renamed))
            val subjectOscillator2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubjectOscillator2Key,
                    "n" -> subjectOscillator2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSubjectOscillators",
                expectedField1Key,
                subjectOscillator1Update,
                subjectOscillator2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject oscillator reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedSubjectOscillator1Key,
                    expectedSubjectOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjectOscillators",   // limited
                expectedField1Key,
                expectedSubjectEmitter1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject oscillator states:
            subjectOscillatorReports = (Json.parse(serviceResult._results.head) \ "rso").as[Seq[JsObject]]
            assert(subjectOscillatorReports.size == 2)
            for (report <- subjectOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubjectOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectOscillator1Renamed)
                    case `expectedSubjectOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectOscillator2Renamed)
                    case _ => assert(false)
                }

            // Destroy subject oscillators:
            val subjectOscillator1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedSubjectOscillator1Key))
            val subjectOscillator2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedSubjectOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySubjectOscillators",
                expectedField1Key,
                subjectOscillator1Destructor,
                subjectOscillator2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject oscillator reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSubjectOscillator1Key,
                    expectedSubjectOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjectOscillators",   // unlimited
                expectedField1Key,
                "~~se",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            subjectOscillatorReports = (Json.parse(serviceResult._results.head) \ "rso").as[Seq[JsObject]]
            assert(subjectOscillatorReports.isEmpty)
        }

    //
    // Probes:
    //
    it must "pass basic probe tests" in
        { f =>
            val probe1Name = "p1"
            val probe1Tag = "!p1"
            val probe2Name = "p2"
            val probe2Tag = "!p2"
            val expectedProbe1Key = "p1~p"
            val expectedProbe2Key = "p2~p"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probes:
            val probe1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probe1Tag,
                    "n" -> probe1Name),
                "a" -> Json.obj(
                    "aa" -> 1.0.toString),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            val probe2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probe2Tag,
                    "n" -> probe2Name),
                "s" -> Json.obj(
                    "p" -> Json.arr((-0.5).toString, (-0.5).toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateProbes",
                expectedField1Key,
                probe1Constructor,
                probe2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe states:
            var probeReports = (Json.parse(serviceResult._results.head) \ "rp").as[Seq[JsObject]]
            assert(probeReports.size == 2)
            for (report <- probeReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbe1Key` =>
                        assert((report \ "m" \ "n").as[String] == probe1Name)
                    case `expectedProbe2Key` =>
                        assert((report \ "m" \ "n").as[String] == probe2Name)
                    case _ => assert(false)
                }

            // Update probes:
            val probe1Renamed = probe1Name + "_renamed"
            val probe2Renamed = probe2Name + "_renamed"
            val probe1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbe1Key,
                    "n" -> probe1Renamed))
            val probe2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbe2Key,
                    "n" -> probe2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateProbes",
                expectedField1Key,
                probe1Update,
                probe2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedProbe1Key,
                    expectedProbe2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbes",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe states:
            probeReports = (Json.parse(serviceResult._results.head) \ "rp").as[Seq[JsObject]]
            assert(probeReports.size == 2)
            for (report <- probeReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbe1Key` =>
                        assert((report \ "m" \ "n").as[String] == probe1Renamed)
                    case `expectedProbe2Key` =>
                        assert((report \ "m" \ "n").as[String] == probe2Renamed)
                    case _ => assert(false)
                }

            // Destroy probes:
            val probe1Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbe1Key))
            val probe2Destructor = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbe2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroyProbes",
                expectedField1Key,
                probe1Destructor,
                probe2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedProbe1Key,
                    expectedProbe2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbes",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            probeReports = (Json.parse(serviceResult._results.head) \ "rp").as[Seq[JsObject]]
            assert(probeReports.isEmpty)
        }

    //
    // Probe Emitters:
    //
    it must "pass basic probe emitter tests" in
        { f =>
            val probe1Name = "p1"
            val probe1Tag = "!p1"
            val probeEmitter1Name = "pe1"
            val probeEmitter1Tag = "!pe1"
            val probeEmitter2Name = "pe2"
            val probeEmitter2Tag = "!pe2"
            val expectedProbe1Key = "p1~p"
            val expectedProbeEmitter1Key = "pe1~pe"
            val expectedProbeEmitter2Key = "pe2~pe"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe:
            val probe1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probe1Tag,
                    "n" -> probe1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateProbes",
                expectedField1Key,
                probe1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe emitters:
            val probeEmitter1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeEmitter1Tag,
                    "n" -> probeEmitter1Name))
            val probeEmitter2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeEmitter2Tag,
                    "n" -> probeEmitter2Name))
            serviceResult = f.cellDirector.CallService(
                "CreateProbeEmitters",
                expectedField1Key,
                expectedProbe1Key,
                probeEmitter1Constructor,
                probeEmitter2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe emitter states:
            var probeEmitterReports = (Json.parse(serviceResult._results.head) \ "rpe").as[Seq[JsObject]]
            assert(probeEmitterReports.size == 2)
            for (report <- probeEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbeEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == probeEmitter1Name)
                    case `expectedProbeEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == probeEmitter2Name)
                    case _ => assert(false)
                }

            // Update probe emitters:
            val probeEmitter1Renamed = probeEmitter1Name + "_renamed"
            val probeEmitter2Renamed = probeEmitter2Name + "_renamed"
            val probeEmitter1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbeEmitter1Key,
                    "n" -> probeEmitter1Renamed))
            val probeEmitter2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbeEmitter2Key,
                    "n" -> probeEmitter2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateProbeEmitters",
                expectedField1Key,
                probeEmitter1Update,
                probeEmitter2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe emitter reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedProbeEmitter1Key,
                    expectedProbeEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbeEmitters", // limited
                expectedField1Key,
                expectedProbe1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe emitter states:
            probeEmitterReports = (Json.parse(serviceResult._results.head) \ "rpe").as[Seq[JsObject]]
            assert(probeEmitterReports.size == 2)
            for (report <- probeEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbeEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == probeEmitter1Renamed)
                    case `expectedProbeEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == probeEmitter2Renamed)
                    case _ => assert(false)
                }

            // Destroy probe emitters:
            val probeEmitter1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedProbeEmitter1Key))
            val probeEmitter2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedProbeEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroyProbeEmitters",
                expectedField1Key,
                probeEmitter1Destructor,
                probeEmitter2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe emitter reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedProbeEmitter1Key,
                    expectedProbeEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbeEmitters", // unlimited
                expectedField1Key,
                "~~p",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            probeEmitterReports = (Json.parse(serviceResult._results.head) \ "rpe").as[Seq[JsObject]]
            assert(probeEmitterReports.isEmpty)
        }

    //
    // Probe Oscillators:
    //
    it must "pass basic probe oscillator tests" in
        { f =>
            val expectedProbe1Key = "p1~p"
            val expectedProbeEmitter1Key = "pe1~pe"
            val expectedProbeOscillator1Key = "po1~po"
            val expectedProbeOscillator2Key = "po2~po"
            val probe1Tag = "!p1"
            val probe1Name = "p1"
            val probeEmitter1Tag = "!pe1"
            val probeEmitter1Name = "pe1"
            val probeOscillator1Tag = "!po1"
            val probeOscillator1Name = "po1"
            val probeOscillator2Tag = "!po2"
            val probeOscillator2Name = "po2"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe:
            val probe1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probe1Tag,
                    "n" -> probe1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateProbes",
                expectedField1Key,
                probe1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe emitter:
            val probeEmitterConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeEmitter1Tag,
                    "n" -> probeEmitter1Name))
            serviceResult = f.cellDirector.CallService(
                "CreateProbeEmitters",
                expectedField1Key,
                expectedProbe1Key,
                probeEmitterConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe oscillators:
            val probeOscillator1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeOscillator1Tag,
                    "n" -> probeOscillator1Name),
                "a" -> Json.obj(
                    "dc" -> Json.obj(
                        "tc" -> "0",
                        "dpo" -> Json.obj(
                            "w" -> "default.tws"))))
            val probeOscillator2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeOscillator2Tag,
                    "n" -> probeOscillator2Name),
                "a" -> Json.obj(
                    "dc" -> Json.obj(
                        "tc" -> "1",
                        "dpo" -> Json.obj(
                            "w" -> "default.tws"))))
            serviceResult = f.cellDirector.CallService(
                "CreateProbeOscillators",
                expectedField1Key,
                expectedProbeEmitter1Key,
                probeOscillator1Constructor,
                probeOscillator2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe oscillator states:
            var probeOscillatorReports = (Json.parse(serviceResult._results.head) \ "rpo").as[Seq[JsObject]]
            assert(probeOscillatorReports.size == 2)
            for (report <- probeOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbeOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == probeOscillator1Name)
                    case `expectedProbeOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == probeOscillator2Name)
                    case _ => assert(false)
                }

            // Update probe oscillators:
            val probeOscillator1Renamed = probeOscillator1Name + "_renamed"
            val probeOscillator2Renamed = probeOscillator2Name + "_renamed"
            val probeOscillator1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbeOscillator1Key,
                    "n" -> probeOscillator1Renamed))
            val probeOscillator2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbeOscillator2Key,
                    "n" -> probeOscillator2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateProbeOscillators",
                expectedField1Key,
                probeOscillator1Update,
                probeOscillator2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe oscillator reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedProbeOscillator1Key,
                    expectedProbeOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbeOscillators",   // limited
                expectedField1Key,
                expectedProbeEmitter1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe oscillator states:
            probeOscillatorReports = (Json.parse(serviceResult._results.head) \ "rpo").as[Seq[JsObject]]
            assert(probeOscillatorReports.size == 2)
            for (report <- probeOscillatorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbeOscillator1Key` =>
                        assert((report \ "m" \ "n").as[String] == probeOscillator1Renamed)
                    case `expectedProbeOscillator2Key` =>
                        assert((report \ "m" \ "n").as[String] == probeOscillator2Renamed)
                    case _ => assert(false)
                }

            // Destroy probe oscillators:
            val probeOscillator1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedProbeOscillator1Key))
            val probeOscillator2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedProbeOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroyProbeOscillators",
                expectedField1Key,
                probeOscillator1Destructor,
                probeOscillator2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe oscillator reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedProbeOscillator1Key,
                    expectedProbeOscillator2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbeOscillators",   // unlimited
                expectedField1Key,
                "~~pe",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            probeOscillatorReports = (Json.parse(serviceResult._results.head) \ "rpo").as[Seq[JsObject]]
            assert(probeOscillatorReports.isEmpty)
        }

    //
    // Probe Collectors:
    //
    it must "pass basic probe collector tests" in
        { f =>
            val probe1Name = "p1"
            val probe1Tag = "!p1"
            val probeCollector1Name = "pc1"
            val probeCollector1Tag = "!pc1"
            val probeCollector2Name = "pc2"
            val probeCollector2Tag = "!pc2"
            val expectedProbe1Key = "p1~p"
            val expectedProbeCollector1Key = "pc1~pc"
            val expectedProbeCollector2Key = "pc2~pc"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe:
            val probe1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probe1Tag,
                    "n" -> probe1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateProbes",
                expectedField1Key,
                probe1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create probe collectors:
            val probeCollector1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeCollector1Tag,
                    "n" -> probeCollector1Name),
                "a" -> Json.obj(
                    "aa" -> 1.0.toString,
                    "dt" -> 1.0.toString,
                    "st" -> 1.0.toString))
            val probeCollector2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> probeCollector2Tag,
                    "n" -> probeCollector2Name),
                "a" -> Json.obj(
                    "aa" -> 1.0.toString,
                    "dt" -> 1.0.toString,
                    "st" -> 1.0.toString))
            serviceResult = f.cellDirector.CallService(
                "CreateProbeCollectors",
                expectedField1Key,
                expectedProbe1Key,
                probeCollector1Constructor,
                probeCollector2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe collector states:
            var probeCollectorReports = (Json.parse(serviceResult._results.head) \ "rpc").as[Seq[JsObject]]
            assert(probeCollectorReports.size == 2)
            for (report <- probeCollectorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbeCollector1Key` =>
                        assert((report \ "m" \ "n").as[String] == probeCollector1Name)
                    case `expectedProbeCollector2Key` =>
                        assert((report \ "m" \ "n").as[String] == probeCollector2Name)
                    case _ => assert(false)
                }

            // Update probe collectors:
            val probeCollector1Renamed = probeCollector1Name + "_renamed"
            val probeCollector2Renamed = probeCollector2Name + "_renamed"
            val probeCollector1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbeCollector1Key,
                    "n" -> probeCollector1Renamed))
            val probeCollector2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedProbeCollector2Key,
                    "n" -> probeCollector2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateProbeCollectors",
                expectedField1Key,
                probeCollector1Update,
                probeCollector2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe collector reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedProbeCollector1Key,
                    expectedProbeCollector2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbeCollectors", // limited
                expectedField1Key,
                expectedProbe1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm probe collector states:
            probeCollectorReports = (Json.parse(serviceResult._results.head) \ "rpc").as[Seq[JsObject]]
            assert(probeCollectorReports.size == 2)
            for (report <- probeCollectorReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedProbeCollector1Key` =>
                        assert((report \ "m" \ "n").as[String] == probeCollector1Renamed)
                    case `expectedProbeCollector2Key` =>
                        assert((report \ "m" \ "n").as[String] == probeCollector2Renamed)
                    case _ => assert(false)
                }

            // Destroy probe collectors:
            val probeCollector1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedProbeCollector1Key))
            val probeCollector2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedProbeCollector2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroyProbeCollectors",
                expectedField1Key,
                probeCollector1Destructor,
                probeCollector2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get probe collector reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedProbeCollector1Key,
                    expectedProbeCollector2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportProbeCollectors", // unlimited
                expectedField1Key,
                "~~p",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            probeCollectorReports = (Json.parse(serviceResult._results.head) \ "rpc").as[Seq[JsObject]]
            assert(probeCollectorReports.isEmpty)
        }

    //
    // Emitter Patches:
    //
    it must "pass basic emitter patch tests" in
        { f =>
            val subject1Name = "s1"
            val subject1Tag = "!s1"
            val subjectEmitter1Name = "se1"
            val subjectEmitter1Tag = "!se1"
            val subjectEmitter2Name = "se2"
            val subjectEmitter2Tag = "!se2"
            val expectedSubject1Key = "s1~s"
            val expectedSubjectEmitter1Key = "se1~se"
            val expectedSubjectEmitter2Key = "se2~se"

            // Attempt to destroy any prior field (might fail):
            val fieldDestructor = Json.obj(
                "m" -> Json.obj("k" -> expectedField1Key),
                "s" -> "s")
            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
            assert(
                serviceResult._status == Cell.ErrorCodes.Ok ||
                    serviceResult._status == Cell.ErrorCodes.FieldUnknown)

            // Create field:
            val fieldConstructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> field1Tag,
                    "n" -> field1Name),
                "a" -> Json.obj(
                    "ac" -> 0.0.toString,
                    "ar" -> 0.0.toString,
                    "ad" -> 1000.0.toString,
                    "g" -> "2S"))
            serviceResult = f.cellDirector.CallService("CreateFields", fieldConstructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject:
            val subject1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subject1Tag,
                    "n" -> subject1Name),
                "s" -> Json.obj(
                    "p" -> Json.arr(0.5.toString, 0.5.toString),
                    "r" -> Json.arr(0.0.toString)))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjects",
                expectedField1Key,
                subject1Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Create subject emitters:
            val subjectEmitter1Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectEmitter1Tag,
                    "n" -> subjectEmitter1Name))
            val subjectEmitter2Constructor = Json.obj(
                "m" -> Json.obj(
                    "t" -> subjectEmitter2Tag,
                    "n" -> subjectEmitter2Name))
            serviceResult = f.cellDirector.CallService(
                "CreateSubjectEmitters",
                expectedField1Key,
                expectedSubject1Key,
                subjectEmitter1Constructor,
                subjectEmitter2Constructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject emitter states:
            var subjectEmitterReports = (Json.parse(serviceResult._results.head) \ "rse").as[Seq[JsObject]]
            assert(subjectEmitterReports.size == 2)
            for (report <- subjectEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubjectEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter1Name)
                    case `expectedSubjectEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter2Name)
                    case _ => assert(false)
                }

            // Update subject emitters:
            val subjectEmitter1Renamed = subjectEmitter1Name + "_renamed"
            val subjectEmitter2Renamed = subjectEmitter2Name + "_renamed"
            val subjectEmitter1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubjectEmitter1Key,
                    "n" -> subjectEmitter1Renamed))
            val subjectEmitter2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedSubjectEmitter2Key,
                    "n" -> subjectEmitter2Renamed))
            serviceResult = f.cellDirector.CallService(
                "UpdateSubjectEmitters",
                expectedField1Key,
                subjectEmitter1Update,
                subjectEmitter2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject emitter reports:
            var query = Json.obj(
                "k" -> Json.arr(
                    expectedSubjectEmitter1Key,
                    expectedSubjectEmitter2Key),
                "s" -> Json.arr("asc"))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjectEmitters", // limited
                expectedField1Key,
                expectedSubject1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm subject emitter states:
            subjectEmitterReports = (Json.parse(serviceResult._results.head) \ "rse").as[Seq[JsObject]]
            assert(subjectEmitterReports.size == 2)
            for (report <- subjectEmitterReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedSubjectEmitter1Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter1Renamed)
                    case `expectedSubjectEmitter2Key` =>
                        assert((report \ "m" \ "n").as[String] == subjectEmitter2Renamed)
                    case _ => assert(false)
                }


            val expectedEmitterPatch1Key = "se1.smpe~smpe"
            val expectedEmitterPatch2Key = "se2.smpe~smpe"

            // Get emitter patch reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedEmitterPatch1Key,
                    expectedEmitterPatch2Key),
                "s" -> Json.arr("asc"))
            serviceResult = f.cellDirector.CallService(
                "ReportEmitterPatches",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm emitter patch states:
            var emitterPatchReports = (Json.parse(serviceResult._results.head) \ "rsmpe").as[Seq[JsObject]]
            assert(emitterPatchReports.size == 2)
            for (report <- emitterPatchReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedEmitterPatch1Key` | `expectedEmitterPatch2Key` =>
                        val channel = (report \ "a" \ "dpe" \ "dc" \ "_").as[JsObject]
                        assert((channel \ "dpo" \ "w").as[String] == "default.tws")
                    case _ => assert(false)
                }

            val emitterPatch1Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedEmitterPatch1Key),
                "a" -> Json.obj(
                    "dpe" -> Json.obj(
                        "dc" -> Json.obj(
                            "_" ->Json.obj(
                                "de" -> Patch.kDefaultPatchEnvelopeDef,
                                "dpo" -> Json.obj(
                                    "w" -> "test.tws"))))))
            val emitterPatch2Update = Json.obj(
                "m" -> Json.obj(
                    "k" -> expectedEmitterPatch2Key),
                "a" -> Json.obj(
                    "dpe" -> Json.obj(
                        "dc" -> Json.obj(
                            "_" ->Json.obj(
                                "de" -> Patch.kDefaultPatchEnvelopeDef,
                                "dpo" -> Json.obj(
                                    "w" -> "test.tws"))))))
            serviceResult = f.cellDirector.CallService(
                "UpdateEmitterPatches",
                expectedField1Key,
                emitterPatch1Update,
                emitterPatch2Update)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get emitter patch reports:
            query = Json.obj(
                "k" -> Json.arr(
                    expectedEmitterPatch1Key,
                    expectedEmitterPatch2Key),
                "s" -> Json.arr("asc"))
            serviceResult = f.cellDirector.CallService(
                "ReportEmitterPatches",
                expectedField1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Confirm emitter patch states:
            emitterPatchReports = (Json.parse(serviceResult._results.head) \ "rsmpe").as[Seq[JsObject]]
            assert(emitterPatchReports.size == 2)
            for (report <- emitterPatchReports)
                (report \ "m" \ "k").as[String] match
                {
                    case `expectedEmitterPatch1Key` | `expectedEmitterPatch2Key` =>
                        val channel = (report \ "s" \ "c" \ "_").as[JsObject]
                        assert((channel \ "dpo" \ "w").as[String] == "test.tws")
                    case _ => assert(false)
                }

            // Get subject emitter patch report:
            query = Json.obj(
                "s" -> Json.arr("as"))
            serviceResult = f.cellDirector.CallService(
                "ReportPatchOfSubjectEmitter",
                expectedField1Key,
                expectedSubjectEmitter1Key,
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Destroy subject emitters:
            val subjectEmitter1Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedSubjectEmitter1Key))
            val subjectEmitter2Destructor = Json.obj(
                "m" -> Json.obj("k" -> expectedSubjectEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "DestroySubjectEmitters",
                expectedField1Key,
                subjectEmitter1Destructor,
                subjectEmitter2Destructor)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)

            // Get subject emitter reports (should be none reporting):
            query = Json.obj(
                "k" -> Json.arr(
                    expectedSubjectEmitter1Key,
                    expectedSubjectEmitter2Key))
            serviceResult = f.cellDirector.CallService(
                "ReportSubjectEmitters", // unlimited
                expectedField1Key,
                "~~s",
                query)
            assert(serviceResult._status == Cell.ErrorCodes.Ok)
            subjectEmitterReports = (Json.parse(serviceResult._results.head) \ "rse").as[Seq[JsObject]]
            assert(subjectEmitterReports.isEmpty)
        }

//    //
//    // Final cleanup:
//    //
//    it must "pass final cleanup tests" in
//        { f =>
//            // Attempt to destroy prior field (should not fail):
//            val fieldDestructor = Json.obj(
//                "m" -> Json.obj("k" -> expectedField1Key),
//                "s" -> "s")
//            var serviceResult = f.cellDirector.CallService("DestroyFields", fieldDestructor)
//            assert(serviceResult.status == Cell.ErrorCodes.Ok)
//        }

}
