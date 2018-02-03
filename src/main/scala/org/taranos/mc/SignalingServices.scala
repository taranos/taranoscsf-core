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

package org.taranos.mc

import org.taranos.common.ServiceResult
import org.taranos.mc.trunk.intraprocess._
import play.api.libs.json.Json


object SignalingServices
{
    //
    // Signal Inputs:
    //

    def ReportSignalInputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalInput.DecodeQuery(queryEncoded)

        // Report signal inputs:
        val inputReports = _trunkModel.ReportSignalInputs(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalInputs -> inputReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalInputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalInput.DecodeUpdate)

        // Update signal ports:
        _trunkModel.UpdateSignalInputs(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalInputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalInput.DecodeConstructor)

        // Create inputs and make list of their keys:
        val inputs = _trunkModel.CreateSignalInputs(trunkKey, constructors)
        val inputKeys = inputs.map(_.GetKey)

        // Report inputs including state and children sections:
        val query = SignalInput.Query(inputKeys, Some(TrunkModel.Glossary.kRModulatorCreationSections))
        val inputReports = _trunkModel.ReportSignalInputs(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalInputs -> inputReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalInputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalInput.DecodeDestructor)

        // Destroy signal ports:
        _trunkModel.DestroySignalInputs(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Bridges:
    //

    def ReportSignalBridges (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalBridge.DecodeQuery(queryEncoded)

        // Report signal bridges:
        val bridgeReports = _trunkModel.ReportSignalBridges(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalBridges -> bridgeReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalBridges (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalBridge.DecodeUpdate)

        // Update signal ports:
        _trunkModel.UpdateSignalBridges(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalBridges (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalBridge.DecodeConstructor)

        // Create bridges and make list of their keys:
        val bridges = _trunkModel.CreateSignalBridges(trunkKey, constructors)
        val bridgeKeys = bridges.map(_.GetKey)

        // Report bridges including state and children sections:
        val query = SignalBridge.Query(bridgeKeys, Some(TrunkModel.Glossary.kRModulatorCreationSections))
        val bridgeReports = _trunkModel.ReportSignalBridges(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalBridges -> bridgeReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalBridges (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalBridge.DecodeDestructor)

        // Destroy signal ports:
        _trunkModel.DestroySignalBridges(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Outputs:
    //

    def ReportSignalOutputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalOutput.DecodeQuery(queryEncoded)

        // Report signal outputs:
        val outputReports = _trunkModel.ReportSignalOutputs(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalOutputs -> outputReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalOutputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalOutput.DecodeUpdate)

        // Update signal ports:
        _trunkModel.UpdateSignalOutputs(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalOutputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalOutput.DecodeConstructor)

        // Create outputs and make list of their keys:
        val outputs = _trunkModel.CreateSignalOutputs(trunkKey, constructors)
        val outputKeys = outputs.map(_.GetKey)

        // Report outputs including state and children sections:
        val query = SignalOutput.Query(outputKeys, Some(TrunkModel.Glossary.kRModulatorCreationSections))
        val outputReports = _trunkModel.ReportSignalOutputs(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalOutputs -> outputReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalOutputs (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalOutput.DecodeDestructor)

        // Destroy signal ports:
        _trunkModel.DestroySignalOutputs(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Trunks:
    //

    def ReportTrunks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode query:
        val queryEncoded = serviceArgs.head
        val query = Trunk.DecodeQuery(queryEncoded)

        // Report trunks:
        val trunkReports = _trunkModel.ReportTrunks(query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRTrunks -> trunkReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with trunk report:
        ServiceResult(0, results)
    }

    def UpdateTrunks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode updates:
        val updatesEncoded = serviceArgs
        val updates = updatesEncoded.map(Trunk.DecodeUpdate)

        // Update trunks:
        _trunkModel.UpdateTrunks(updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateTrunks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode constructors:
        val constructorsEncoded = serviceArgs
        val constructors = constructorsEncoded.map(Trunk.DecodeConstructor)

        // Create trunks and make list of their keys:
        val trunks = _trunkModel.CreateTrunks(constructors)
        val trunkKeys = trunks.map(_.GetKey)

        // Report trunks:
        val query = Trunk.Query(trunkKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val trunkReports = _trunkModel.ReportTrunks(query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRTrunks -> trunkReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with initial trunk report:
        ServiceResult(0, results)
    }

    def DestroyTrunks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode destructors:
        val destructorsEncoded = serviceArgs
        val destructors = destructorsEncoded.map(Trunk.DecodeDestructor)

        // Destroy trunks:
        _trunkModel.DestroyTrunks(destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Interfaces:
    //

    def ReportSignalInterfaces (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalInterface.DecodeQuery(queryEncoded)

        // Report interfaces:
        val interfaceReports = _trunkModel.ReportSignalInterfaces(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalInterfaces -> interfaceReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalInterfaces (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalInterface.DecodeUpdate)

        // Update interfaces:
        _trunkModel.UpdateSignalInterfaces(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalInterfaces (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalInterface.DecodeConstructor)

        // Create interfaces and make list of their keys:
        val interfaces = _trunkModel.CreateSignalInterfaces(trunkKey, constructors)
        val interfaceKeys = interfaces.map(_.GetKey)

        // Report interfaces including state and children sections:
        val query = SignalInterface.Query(interfaceKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val interfaceReports = _trunkModel.ReportSignalInterfaces(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalInterfaces -> interfaceReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalInterfaces (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalInterface.DecodeDestructor)

        // Destroy interfaces:
        _trunkModel.DestroySignalInterfaces(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Ports:
    //

    def LookupSignalPort (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode alias:
        val alias = serviceArgs(1)

        // Report lookup:
        val lookupReport = _trunkModel.LookupSignalPort(trunkKey, alias)

        // Wrap report:
        val wrapper = Json.obj(TrunkModel.Glossary.kLSignalPorts -> lookupReport)

        // Stringify wrapper:
        val result = Vector(Json.stringify(wrapper))

        // Reply with report:
        ServiceResult(0, result)
    }

    def ReportSignalPorts (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode interface key:
        val interfaceKeyEncoded = serviceArgs(1)
        val interfaceKey = TrunkElement.DecodeKey[SignalInterface.Key](interfaceKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = SignalPort.DecodeQuery(queryEncoded)

        // Report signal ports:
        val portReports = _trunkModel.ReportSignalPorts(
            trunkKey,
            interfaceKey,
            query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalPorts -> portReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalPorts (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalPort.DecodeUpdate)

        // Update signal ports:
        _trunkModel.UpdateSignalPorts(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalPorts (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode interface key:
        val interfaceKeyEncoded = serviceArgs(1)
        val interfaceKey = TrunkElement.DecodeKey[SignalInterface.Key](interfaceKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(SignalPort.DecodeConstructor)

        // Create ports and make list of their keys:
        val ports = _trunkModel.CreateSignalPorts(
            trunkKey,
            interfaceKey,
            constructors)
        val portKeys = ports.map(_.GetKey)

        // Report ports including state and children sections:
        val query = SignalPort.Query(portKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val portReports = _trunkModel.ReportSignalPorts(
            trunkKey,
            interfaceKey,
            query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalPorts -> portReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalPorts (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalPort.DecodeDestructor)

        // Destroy signal ports:
        _trunkModel.DestroySignalPorts(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Sources:
    //

    def ReportSignalSources (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalSource.DecodeQuery(queryEncoded)

        // Report signal sources:
        val signalSourceReports = _trunkModel.ReportSignalSources(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalSources -> signalSourceReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalSources (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalSource.DecodeUpdate)

        // Update signal sources:
        _trunkModel.UpdateSignalSources(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalSources (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalSource.DecodeConstructor)

        // Create sources and make list of their keys:
        val signalSources = _trunkModel.CreateSignalSources(trunkKey, constructors)
        val signalSourceKeys = signalSources.map(_.GetKey)

        // Report sources including state and children sections:
        val query = SignalSource.Query(signalSourceKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val signalSourceReports = _trunkModel.ReportSignalSources(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalSources -> signalSourceReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalSources (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalSource.DecodeDestructor)

        // Destroy signal sources:
        _trunkModel.DestroySignalSources(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Sinks:
    //

    def ReportSignalSinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalSink.DecodeQuery(queryEncoded)

        // Report signal sinks:
        val signalSinkReports = _trunkModel.ReportSignalSinks(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalSinks -> signalSinkReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalSinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalSink.DecodeUpdate)

        // Update signal sinks:
        _trunkModel.UpdateSignalSinks(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalSinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalSink.DecodeConstructor)

        // Create sinks and make list of their keys:
        val signalSinks = _trunkModel.CreateSignalSinks(trunkKey, constructors)
        val signalSinkKeys = signalSinks.map(_.GetKey)

        // Report sinks including state and children sections:
        val query = SignalSink.Query(signalSinkKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val signalSinkReports = _trunkModel.ReportSignalSinks(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalSinks -> signalSinkReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalSinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalSink.DecodeDestructor)

        // Destroy signal sinks:
        _trunkModel.DestroySignalSinks(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Links:
    //

    def ReportSignalLinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalLink.DecodeQuery(queryEncoded)

        // Report signal links:
        val signalLinkReports = _trunkModel.ReportSignalLinks(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalLinks -> signalLinkReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalLinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalLink.DecodeUpdate)

        // Update signal sinks:
        _trunkModel.UpdateSignalLinks(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalLinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalLink.DecodeConstructor)

        // Create links and make list of their keys:
        val signalLinks = _trunkModel.CreateSignalLinks(trunkKey, constructors)
        val signalLinkKeys = signalLinks.map(_.GetKey)

        // Report links including state and children sections:
        val query = SignalLink.Query(signalLinkKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val signalLinkReports = _trunkModel.ReportSignalLinks(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalLinks -> signalLinkReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalLinks (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalLink.DecodeDestructor)

        // Destroy signal sinks:
        _trunkModel.DestroySignalLinks(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Signal Taps:
    //

    def ReportSignalTaps (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = SignalTap.DecodeQuery(queryEncoded)

        // Report signal taps:
        val signalTapReports = _trunkModel.ReportSignalTaps(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalTaps -> signalTapReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSignalTaps (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SignalTap.DecodeUpdate)

        // Update signal taps:
        _trunkModel.UpdateSignalTaps(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSignalTaps (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(SignalTap.DecodeConstructor)

        // Create taps and make list of their keys:
        val signalTaps = _trunkModel.CreateSignalTaps(trunkKey, constructors)
        val signalTapKeys = signalTaps.map(_.GetKey)

        // Report taps including state and children sections:
        val query = SignalTap.Query(signalTapKeys, Some(TrunkModel.Glossary.kRStandardCreationSections))
        val signalTapReports = _trunkModel.ReportSignalTaps(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kRSignalTaps -> signalTapReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySignalTaps (serviceArgs: Vector[String])
        (implicit _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode trunk key:
        val trunkKeyEncoded = serviceArgs.head
        val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SignalTap.DecodeDestructor)

        // Destroy signal taps:
        _trunkModel.DestroySignalTaps(trunkKey, destructors)

        // Reply nothing:
        ServiceResult()
    }
}
