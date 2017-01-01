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

package org.taranos.mc

import org.taranos.common.ServiceResult
import org.taranos.mc.field._
import org.taranos.mc.trunk.intraprocess._
import play.api.libs.json.Json


object RenderingServices
{
    //
    // Fields:
    //

    def ReportFields (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode query:
        val queryEncoded = serviceArgs.head
        val query = Field.DecodeQuery(queryEncoded)

        // Report fields:
        val fieldReports = _fieldModel.ReportFields(query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRFields -> fieldReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateFields (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode updates:
        val updatesEncoded = serviceArgs
        val updates = updatesEncoded.map(Field.DecodeUpdate)

        // Update fields:
        _fieldModel.UpdateFields(updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateFields (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode constructors:
        val constructorsEncoded = serviceArgs
        val constructors = constructorsEncoded.map(Field.DecodeConstructor)

        // Create fields and make list of their keys:
        val fields = _fieldModel.CreateFields(constructors)
        val fieldKeys = fields.map(_.GetKey)

        // Report fields:
        val query = new Field.Query(fieldKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val fieldReports = _fieldModel.ReportFields(query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRFields -> fieldReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyFields (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode destructors:
        val destructorsEncoded = serviceArgs
        val destructors = destructorsEncoded.map(Field.DecodeDestructor)

        // Destroy fields:
        _fieldModel.DestroyFields(destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Field Emitters:
    //

    def ReportFieldEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = FieldEmitter.DecodeQuery(queryEncoded)

        // Report field emitters:
        val fieldEmitterReports = _fieldModel.ReportFieldEmitters(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRFieldEmitters -> fieldEmitterReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateFieldEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(FieldEmitter.DecodeUpdate)

        // Update field emitters:
        _fieldModel.UpdateFieldEmitters(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CallFieldEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode calls:
        val callsEncoded = serviceArgs.drop(1)
        val calls = callsEncoded.map(FieldEmitter.DecodeCall)

        // Update field emitters:
        _fieldModel.CallFieldEmitters(fieldKey, calls)

        // Reply nothing:
        ServiceResult()
    }

    def CreateFieldEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(FieldEmitter.DecodeConstructor)

        // Create field emitters and make list of their keys:
        val fieldEmitters = _fieldModel.CreateFieldEmitters(fieldKey, constructors)
        val fieldEmitterKeys = fieldEmitters.map(_.GetKey)

        // Report field emitters:
        val query = new FieldEmitter.Query(fieldEmitterKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val fieldEmitterReports = _fieldModel.ReportFieldEmitters(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRFieldEmitters -> fieldEmitterReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyFieldEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(FieldEmitter.DecodeDestructor)

        // Destroy field emitters:
        _fieldModel.DestroyFieldEmitters(fieldKey, destructors, isForcedDestroy = false)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Field Oscillators:
    //

    def ReportFieldOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode field emitter key:
        val fieldEmitterKeyEncoded = serviceArgs(1)
        val fieldEmitterKey = FieldElement.DecodeKey[FieldEmitter.Key](fieldEmitterKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = FieldOscillator.DecodeQuery(queryEncoded)

        // Report field oscillators:
        val fieldOscillatorReports = _fieldModel.ReportFieldOscillators(fieldKey, fieldEmitterKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRFieldOscillators -> fieldOscillatorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateFieldOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(FieldOscillator.DecodeUpdate)

        // Update field oscillators:
        _fieldModel.UpdateFieldOscillators(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CallFieldOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode calls:
        val callsEncoded = serviceArgs.drop(1)
        val calls = callsEncoded.map(FieldOscillator.DecodeCall)

        // Update field oscillators:
        _fieldModel.CallFieldOscillators(fieldKey, calls)

        // Reply nothing:
        ServiceResult()
    }

    def CreateFieldOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode field emitter key:
        val fieldEmitterKeyEncoded = serviceArgs(1)
        val fieldEmitterKey = FieldElement.DecodeKey[FieldEmitter.Key](fieldEmitterKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(FieldOscillator.DecodeConstructor)

        // Create field emitters and make list of their keys:
        val fieldOscillators = _fieldModel.CreateFieldOscillators(fieldKey, fieldEmitterKey, constructors)
        val fieldOscillatorKeys = fieldOscillators.map(_.GetKey)

        // Report field oscillators:
        val query = new FieldOscillator.Query(fieldOscillatorKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val fieldOscillatorReports = _fieldModel.ReportFieldOscillators(fieldKey, fieldEmitterKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRFieldOscillators -> fieldOscillatorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyFieldOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(FieldOscillator.DecodeDestructor)

        // Destroy field oscillators:
        _fieldModel.DestroyFieldOscillators(fieldKey, destructors, isForcedDestroy = false)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Subjects:
    //

    def ReportSubjects (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = Subject.DecodeQuery(queryEncoded)

        // Report subjects:
        val subjectReports = _fieldModel.ReportSubjects(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRSubjects -> subjectReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSubjects (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(Subject.DecodeUpdate)

        // Update subjects:
        _fieldModel.UpdateSubjects(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSubjects (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(Subject.DecodeConstructor)

        // Create subjects and make list of their keys:
        val subjects = _fieldModel.CreateSubjects(fieldKey, constructors)
        val subjectKeys = subjects.map(_.GetKey)

        // Report subjects including state and children sections:
        val query = new Subject.Query(subjectKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val subjectReports = _fieldModel.ReportSubjects(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRSubjects -> subjectReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySubjects (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(Subject.DecodeDestructor)

        // Destroy subjects:
        _fieldModel.DestroySubjects(fieldKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Subject Emitters:
    //

    def ReportSubjectEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode subject key:
        val subjectKeyEncoded = serviceArgs(1)
        val subjectKey = FieldElement.DecodeKey[Subject.Key](subjectKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = SubjectEmitter.DecodeQuery(queryEncoded)

        // Report subject emitters:
        val subjectEmitterReports = _fieldModel.ReportSubjectEmitters(fieldKey, subjectKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRSubjectEmitters -> subjectEmitterReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSubjectEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SubjectEmitter.DecodeUpdate)

        // Update subject emitters:
        _fieldModel.UpdateSubjectEmitters(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CallSubjectEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode calls:
        val callsEncoded = serviceArgs.drop(1)
        val calls = callsEncoded.map(SubjectEmitter.DecodeCall)

        // Update subject emitters:
        _fieldModel.CallSubjectEmitters(fieldKey, calls)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSubjectEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode subject key:
        val subjectKeyEncoded = serviceArgs(1)
        val subjectKey = FieldElement.DecodeKey[Subject.Key](subjectKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(SubjectEmitter.DecodeConstructor)

        // Create subject emitters and make list of their keys:
        val subjectEmitters = _fieldModel.CreateSubjectEmitters(fieldKey, subjectKey, constructors)
        val subjectEmitterKeys = subjectEmitters.map(_.GetKey)

        // Report subject emitters:
        val query = SubjectEmitter.Query(subjectEmitterKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val subjectEmitterReports = _fieldModel.ReportSubjectEmitters(fieldKey, subjectKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRSubjectEmitters -> subjectEmitterReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySubjectEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SubjectEmitter.DecodeDestructor)

        // Destroy subject emitters:
        _fieldModel.DestroySubjectEmitters(fieldKey, destructors, isForcedDestroy = false)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Subject Oscillators:
    //

    def ReportSubjectOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode subject emitter key:
        val subjectEmitterKeyEncoded = serviceArgs(1)
        val subjectEmitterKey = FieldElement.DecodeKey[SubjectEmitter.Key](subjectEmitterKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = SubjectOscillator.DecodeQuery(queryEncoded)

        // Report subject oscillators:
        val subjectOscillatorReports = _fieldModel.ReportSubjectOscillators(fieldKey, subjectEmitterKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRSubjectOscillators -> subjectOscillatorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateSubjectOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(SubjectOscillator.DecodeUpdate)

        // Update field oscillators:
        _fieldModel.UpdateSubjectOscillators(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CallSubjectOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode calls:
        val callsEncoded = serviceArgs.drop(1)
        val calls = callsEncoded.map(SubjectOscillator.DecodeCall)

        // Update subject oscillators:
        _fieldModel.CallSubjectOscillators(fieldKey, calls)

        // Reply nothing:
        ServiceResult()
    }

    def CreateSubjectOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode subject emitter key:
        val subjectEmitterKeyEncoded = serviceArgs(1)
        val subjectEmitterKey = FieldElement.DecodeKey[SubjectEmitter.Key](subjectEmitterKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(SubjectOscillator.DecodeConstructor)

        // Create subject emitters and make list of their keys:
        val subjectOscillators = _fieldModel.CreateSubjectOscillators(fieldKey, subjectEmitterKey, constructors)
        val subjectOscillatorKeys = subjectOscillators.map(_.GetKey)

        // Report subject oscillators:
        val query = new SubjectOscillator.Query(subjectOscillatorKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val subjectOscillatorReports = _fieldModel.ReportSubjectOscillators(fieldKey, subjectEmitterKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRSubjectOscillators -> subjectOscillatorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroySubjectOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(SubjectOscillator.DecodeDestructor)

        // Destroy field oscillators:
        _fieldModel.DestroySubjectOscillators(fieldKey, destructors, isForcedDestroy = false)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Probes:
    //

    def ReportProbes (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = Probe.DecodeQuery(queryEncoded)

        // Report probes:
        val probeReports = _fieldModel.ReportProbes(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbes -> probeReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateProbes (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(Probe.DecodeUpdate)

        // Update probes:
        _fieldModel.UpdateProbes(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateProbes (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(1)
        val constructors = constructorsEncoded.map(Probe.DecodeConstructor)

        // Create probes and make list of their keys:
        val probes = _fieldModel.CreateProbes(fieldKey, constructors)
        val probeKeys = probes.map(_.GetKey)

        // Report probes including state and children sections:
        val query = new Probe.Query(probeKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val probeReports = _fieldModel.ReportProbes(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbes -> probeReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyProbes (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(Probe.DecodeDestructor)

        // Destroy probes:
        _fieldModel.DestroyProbes(fieldKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Probe Emitters:
    //

    def ReportProbeEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe key:
        val probeKeyEncoded = serviceArgs(1)
        val probeKey = FieldElement.DecodeKey[Probe.Key](probeKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = ProbeEmitter.DecodeQuery(queryEncoded)

        // Report probe emitters:
        val probeEmitterReports = _fieldModel.ReportProbeEmitters(fieldKey, probeKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbeEmitters -> probeEmitterReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateProbeEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(ProbeEmitter.DecodeUpdate)

        // Update probe emitters:
        _fieldModel.UpdateProbeEmitters(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CallProbeEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode calls:
        val callsEncoded = serviceArgs.drop(1)
        val calls = callsEncoded.map(ProbeEmitter.DecodeCall)

        // Update probe emitters:
        _fieldModel.CallProbeEmitters(fieldKey, calls)

        // Reply nothing:
        ServiceResult()
    }

    def CreateProbeEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe key:
        val probeKeyEncoded = serviceArgs(1)
        val probeKey = FieldElement.DecodeKey[Probe.Key](probeKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(ProbeEmitter.DecodeConstructor)

        // Create probe emitters and make list of their keys:
        val probeEmitters = _fieldModel.CreateProbeEmitters(fieldKey, probeKey, constructors)
        val probeEmitterKeys = probeEmitters.map(_.GetKey)

        // Report probe emitters:
        val query = ProbeEmitter.Query(probeEmitterKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val probeEmitterReports = _fieldModel.ReportProbeEmitters(fieldKey, probeKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbeEmitters -> probeEmitterReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyProbeEmitters (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(ProbeEmitter.DecodeDestructor)

        // Destroy probe emitters:
        _fieldModel.DestroyProbeEmitters(fieldKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Probe Oscillators:
    //

    def ReportProbeOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe emitter key:
        val probeEmitterKeyEncoded = serviceArgs(1)
        val probeEmitterKey = FieldElement.DecodeKey[ProbeEmitter.Key](probeEmitterKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = ProbeOscillator.DecodeQuery(queryEncoded)

        // Report probe oscillators:
        val probeOscillatorReports = _fieldModel.ReportProbeOscillators(
            fieldKey,
            probeEmitterKey,
            query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbeOscillators -> probeOscillatorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateProbeOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(ProbeOscillator.DecodeUpdate)

        // Update field oscillators:
        _fieldModel.UpdateProbeOscillators(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CallProbeOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode calls:
        val callsEncoded = serviceArgs.drop(1)
        val calls = callsEncoded.map(ProbeOscillator.DecodeCall)

        // Update probe oscillators:
        _fieldModel.CallProbeOscillators(fieldKey, calls)

        // Reply nothing:
        ServiceResult()
    }

    def CreateProbeOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe emitter key:
        val probeEmitterKeyEncoded = serviceArgs(1)
        val probeEmitterKey = FieldElement.DecodeKey[ProbeEmitter.Key](probeEmitterKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(ProbeOscillator.DecodeConstructor)

        // Create probe emitters and make list of their keys:
        val probeOscillators = _fieldModel.CreateProbeOscillators(fieldKey, probeEmitterKey, constructors)
        val probeOscillatorKeys = probeOscillators.map(_.GetKey)

        // Report probe oscillators:
        val query = new ProbeOscillator.Query(probeOscillatorKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val probeOscillatorReports = _fieldModel.ReportProbeOscillators(fieldKey, probeEmitterKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbeOscillators -> probeOscillatorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyProbeOscillators (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(ProbeOscillator.DecodeDestructor)

        // Destroy field oscillators:
        _fieldModel.DestroyProbeOscillators(fieldKey, destructors, isForcedDestroy = false)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Probe Collectors:
    //

    def LookupProbeCollector (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode alias:
        val alias = serviceArgs(1)

        // Report lookup:
        val lookupReport = _fieldModel.LookupProbeCollector(fieldKey, alias)

        // Wrap report:
        val wrapper = Json.obj(FieldModel.Glossary.kLProbeCollectors -> lookupReport)

        // Stringify wrapper:
        val result = Vector(Json.stringify(wrapper))

        // Reply with report:
        ServiceResult(0, result)
    }

    def ReportProbeCollectors (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe key:
        val probeKeyEncoded = serviceArgs(1)
        val probeKey = FieldElement.DecodeKey[Probe.Key](probeKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = ProbeCollector.DecodeQuery(queryEncoded)

        // Report probe collectors:
        val probeCollectorReports = _fieldModel.ReportProbeCollectors(fieldKey, probeKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbeCollectors -> probeCollectorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateProbeCollectors (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(ProbeCollector.DecodeUpdate)

        // Update probe collectors:
        _fieldModel.UpdateProbeCollectors(fieldKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    def CreateProbeCollectors (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe key:
        val probeKeyEncoded = serviceArgs(1)
        val probeKey = FieldElement.DecodeKey[Probe.Key](probeKeyEncoded)

        // Decode constructors:
        val constructorsEncoded = serviceArgs.drop(2)
        val constructors = constructorsEncoded.map(ProbeCollector.DecodeConstructor)

        // Create probe collectors and make list of their keys:
        val probeCollectors = _fieldModel.CreateProbeCollectors(fieldKey, probeKey, constructors)
        val probeCollectorKeys = probeCollectors.map(_.GetKey)

        // Report probe collectors:
        val query = ProbeCollector.Query(probeCollectorKeys, Some(FieldModel.Glossary.kRStandardCreationSections))
        val probeCollectorReports = _fieldModel.ReportProbeCollectors(fieldKey, probeKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRProbeCollectors -> probeCollectorReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def DestroyProbeCollectors (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode destructors:
        val destructorsEncoded = serviceArgs.drop(1)
        val destructors = destructorsEncoded.map(ProbeCollector.DecodeDestructor)

        // Destroy probe collectors:
        _fieldModel.DestroyProbeCollectors(fieldKey, destructors)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Waveforms:
    //

    def ReportWaveformsAtProbeCollector (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode probe collector key:
        val probeCollectorKeyEncoded = serviceArgs(1)
        val probeCollectorKey = FieldElement.DecodeKey[ProbeCollector.Key](probeCollectorKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = Simpler.DecodeQuery(queryEncoded)

        // Report waveforms:
        val waveformReports = _fieldModel.ReportWaveformsAtProbeCollector(fieldKey, probeCollectorKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRWaveforms -> waveformReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportWaveformsAtSampler (serviceArgs: Vector[String])
        (implicit _fieldModel: FieldModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = Sampler.DecodeQuery(queryEncoded)

        // Report waveforms:
        val waveformReports = _fieldModel.ReportWaveformsAtSampler(fieldKey, query)

        // Wrap reports:
        val wrapper = Json.obj(FieldModel.Glossary.kRWaveforms -> waveformReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    //
    // Emitter Patches:
    //

    def ReportEmitterPatches (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode query:
        val queryEncoded = serviceArgs(1)
        val query = EmitterPatch.DecodeQuery(queryEncoded)

        // Report emitter patches:
        val emitterPatchReports = _trunkModel.ReportEmitterPatches(trunkKey, query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kREmitterPatches -> emitterPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportPatchOfFieldEmitter (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode field emitter key:
        val fieldEmitterKeyEncoded = serviceArgs(1)
        val fieldEmitterKey = FieldElement.DecodeKey[FieldEmitter.Key](fieldEmitterKeyEncoded)
        val fieldEmitterOpt = _fieldModel.GetFieldEmitterOpt(fieldKey, fieldEmitterKey)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = EmitterPatch.DecodeQuery(queryEncoded)
        val newQuery = new EmitterPatch.Query(Vector(fieldEmitterOpt.get.GetPatchKey), query._sectionsOpt)

        // Report emitter patches:
        val emitterPatchReports = _trunkModel.ReportEmitterPatches(trunkKey, newQuery)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kREmitterPatches -> emitterPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportPatchOfSubjectEmitter (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode subject emitter key:
        val subjectEmitterKeyEncoded = serviceArgs(1)
        val subjectEmitterKey = FieldElement.DecodeKey[SubjectEmitter.Key](subjectEmitterKeyEncoded)
        val subjectEmitterOpt = _fieldModel.GetSubjectEmitterOpt(fieldKey, subjectEmitterKey)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = EmitterPatch.DecodeQuery(queryEncoded)
        val newQuery = new EmitterPatch.Query(Vector(subjectEmitterOpt.get.GetPatchKey), query._sectionsOpt)

        // Report emitter patches:
        val emitterPatchReports = _trunkModel.ReportEmitterPatches(trunkKey, newQuery)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kREmitterPatches -> emitterPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportPatchOfProbeEmitter (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode probe emitter key:
        val probeEmitterKeyEncoded = serviceArgs(1)
        val probeEmitterKey = FieldElement.DecodeKey[ProbeEmitter.Key](probeEmitterKeyEncoded)
        val probeEmitterOpt = _fieldModel.GetProbeEmitterOpt(fieldKey, probeEmitterKey)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = EmitterPatch.DecodeQuery(queryEncoded)
        val newQuery = new EmitterPatch.Query(Vector(probeEmitterOpt.get.GetPatchKey), query._sectionsOpt)

        // Report emitter patches:
        val emitterPatchReports = _trunkModel.ReportEmitterPatches(trunkKey, newQuery)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kREmitterPatches -> emitterPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateEmitterPatches (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(EmitterPatch.DecodeUpdate)

        // Update field oscillators:
        _trunkModel.UpdateEmitterPatches(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Oscillator Patches:
    //

    def ReportOscillatorPatches (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode emitter patch key:
        val emitterPatchKeyEncoded = serviceArgs(1)
        val emitterPatchKey = TrunkElement.DecodeKey[EmitterPatch.Key](emitterPatchKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = OscillatorPatch.DecodeQuery(queryEncoded)

        // Report oscillator patches:
        val oscillatorPatchReports = _trunkModel.ReportOscillatorPatches(
            trunkKey,
            emitterPatchKey,
            query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kROscillatorPatches -> oscillatorPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportPatchOfFieldOscillator (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode field oscillator key:
        val fieldOscillatorKeyEncoded = serviceArgs(1)
        val fieldOscillatorKey = FieldElement.DecodeKey[FieldOscillator.Key](fieldOscillatorKeyEncoded)
        val fieldOscillatorOpt = _fieldModel.GetFieldOscillatorOpt(fieldKey, fieldOscillatorKey)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = OscillatorPatch.DecodeQuery(queryEncoded)
        val newQuery = new OscillatorPatch.Query(Vector(fieldOscillatorOpt.get.GetPatchKey), query._sectionsOpt)

        // Report oscillator patches:
        val oscillatorPatchReports = _trunkModel.ReportOscillatorPatches(
            trunkKey,
            EmitterPatch.kAnyKey,
            newQuery)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kROscillatorPatches -> oscillatorPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportPatchOfSubjectOscillator (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode subject oscillator key:
        val subjectOscillatorKeyEncoded = serviceArgs(1)
        val subjectOscillatorKey = FieldElement.DecodeKey[SubjectOscillator.Key](subjectOscillatorKeyEncoded)
        val subjectOscillatorOpt = _fieldModel.GetSubjectOscillatorOpt(fieldKey, subjectOscillatorKey)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = OscillatorPatch.DecodeQuery(queryEncoded)
        val newQuery = new OscillatorPatch.Query(Vector(subjectOscillatorOpt.get.GetPatchKey), query._sectionsOpt)

        // Report oscillator patches:
        val oscillatorPatchReports = _trunkModel.ReportOscillatorPatches(
            trunkKey,
            EmitterPatch.kAnyKey,
            newQuery)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kROscillatorPatches -> oscillatorPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def ReportPatchOfProbeOscillator (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode probe oscillator key:
        val probeOscillatorKeyEncoded = serviceArgs(1)
        val probeOscillatorKey = FieldElement.DecodeKey[ProbeOscillator.Key](probeOscillatorKeyEncoded)
        val probeOscillatorOpt = _fieldModel.GetProbeOscillatorOpt(fieldKey, probeOscillatorKey)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = OscillatorPatch.DecodeQuery(queryEncoded)
        val newQuery = new OscillatorPatch.Query(Vector(probeOscillatorOpt.get.GetPatchKey), query._sectionsOpt)

        // Report oscillator patches:
        val oscillatorPatchReports = _trunkModel.ReportOscillatorPatches(
            trunkKey,
            EmitterPatch.kAnyKey,
            newQuery)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kROscillatorPatches -> oscillatorPatchReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateOscillatorPatches (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(1)
        val updates = updatesEncoded.map(OscillatorPatch.DecodeUpdate)

        // Update field oscillators:
        _trunkModel.UpdateOscillatorPatches(trunkKey, updates)

        // Reply nothing:
        ServiceResult()
    }

    //
    // Oscillator Patch Envelopes:
    //

    def ReportOscillatorPatchEnvelopes (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode oscillator patch key:
        val oscillatorPatchKeyEncoded = serviceArgs(1)
        val oscillatorPatchKey = TrunkElement.DecodeKey[OscillatorPatch.Key](oscillatorPatchKeyEncoded)

        // Decode query:
        val queryEncoded = serviceArgs(2)
        val query = OscillatorEnvelope.DecodeQuery(queryEncoded)

        // Report oscillator patch envelopes:
        val envelopeReports = _trunkModel.ReportOscillatorPatchEnvelopes(
            trunkKey,
            oscillatorPatchKey,
            query)

        // Wrap reports:
        val wrapper = Json.obj(TrunkModel.Glossary.kROscillatorPatchEnvelopes -> envelopeReports)

        // Stringify wrapper:
        val results = Vector(Json.stringify(wrapper))

        // Reply with reports:
        ServiceResult(0, results)
    }

    def UpdateOscillatorPatchEnvelopes (serviceArgs: Vector[String])
        (implicit
            _fieldModel: FieldModel,
            _trunkModel: TrunkModel): ServiceResult =
    {
        // Decode field key:
        val fieldKeyEncoded = serviceArgs.head
        val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)
        val field = _fieldModel.GetField(fieldKey)
        val trunkKey = field.GetTrunkKey

        // Decode oscillator patch key:
        val oscillatorPatchKeyEncoded = serviceArgs(1)
        val oscillatorPatchKey = TrunkElement.DecodeKey[OscillatorPatch.Key](oscillatorPatchKeyEncoded)

        // Decode updates:
        val updatesEncoded = serviceArgs.drop(2)
        val updates = updatesEncoded.map(OscillatorEnvelope.DecodeUpdate)

        // Update field oscillators:
        _trunkModel.UpdateOscillatorPatchEnvelopes(
            trunkKey,
            oscillatorPatchKey,
            updates)

        // Reply nothing:
        ServiceResult()
    }
}
