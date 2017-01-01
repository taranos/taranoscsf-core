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

package org.taranos.mc.field

import org.taranos.mc.Common.{Real, ReportSectionsParser, Reporter}
import org.taranos.mc.field.FieldElement.CommonQueryDecoder
import org.taranos.mc.field.FieldModel.FieldModelReporter
import org.taranos.mc.trunk.intraprocess.Signal.{Continuous, Discrete}
import org.taranos.mc.trunk.intraprocess._
import org.taranos.mc.{Cell, CellLogger}
import play.api.libs.json.{JsObject, Json}


object FieldModel
{
    case class Query (
        sectionsOpt: Option[String] = None)

    def DeserializeFromCQL (json: String): Unit =
    {}

    def DeserializeFromJSON (json: String): Unit =
    {}

    def SerializeToJSON (fieldInstance: FieldModel): String =
    {
        ""
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[FieldElement.Key](query, isKeysRequired = false)

        new Query(commonQuery._sectionsOpt)
    }

    trait FieldModelReporter
        extends Reporter
    {
        def Report (sectionsOpt: Option[String]): JsObject = Json.obj()

        protected[mc]
        def Report (fieldKey: Field.Key, sectionsOpt: Option[String]): JsObject
    }

    val kMidiMaxFrequency = math.pow(2f, (127 - 69) / 12f) * 440f
    val kMaxPitch = math.ceil(kMidiMaxFrequency)

    // String glossary:
    object Glossary
    {
        // punctuation
        val kDefaultPrefix = "_"
        val kBang = "!"
        val kTagSeparator = "."
        val kTagSubSeparator = "-"
        val kPartSeparator = "~"

        // elements
        val kEField = "f"
        val kEWaveform = "w"
        val kEEmitter = "e"
        val kEOscillator = "o"
        val kECollector = "c"
        val kEBody = "b"
        val kESubject = "s"
        val kEProbe = "p"
        val kEFieldEmitter = kEField + kEEmitter
        val kEFieldOscillator = kEField + kEOscillator
        val kESubjectEmitter = kESubject + kEEmitter
        val kESubjectOscillator = kESubject + kEOscillator
        val kEProbeEmitter = kEProbe + kEEmitter
        val kEProbeOscillator = kEProbe + kEOscillator
        val kEProbeCollector = kEProbe + kECollector
        val kEDefaultField = kDefaultPrefix + kEField
        val kEDefaultEmitter = kDefaultPrefix + kEEmitter
        val kEDefaultCollector = kDefaultPrefix + kECollector
        val kEDefaultFieldEmitter = kDefaultPrefix + kEFieldEmitter
        val kEDefaultFieldOscillator = kDefaultPrefix + kEFieldOscillator
        val kEDefaultSubjectEmitter = kDefaultPrefix + kESubjectEmitter
        val kEDefaultSubjectOscillator = kDefaultPrefix + kESubjectOscillator
        val kEDefaultProbeEmitter = kDefaultPrefix + kEProbeEmitter
        val kEDefaultProbeOscillator = kDefaultPrefix + kEProbeOscillator
        val kEDefaultProbeCollector = kDefaultPrefix + kEProbeCollector

        // subelements
        val kChannel = "c"

        // property sets and properties
        val kPSMeta = "m"
        val kPMetaKey = "k"
        val kPMetaTag = "t"
        val kPMetaBadge = "b"
        val kPMetaName = "n"
        val kPMetaDescription = "d"
        val kPMetaAlias = "a"
        val kPSAttrs = "a"
        val kPAttrsAcoustic_a = "aa"
        val kPAttrsAcoustic_c = "ac"
        val kPAttrsAcoustic_r = "ar"
        val kPAttrsAntipodeDistance = "ad"
        val kPAttrsFieldGeometry = "fg"
        val kPAttrsLobeBearingPoles = "lbp"
        val kPAttrsLobeRange = "lr"
        val kPAttrsLobeRangePoles = "lrp"
        val kPAttrsSquelchThreshold = "st"
        val kPSRefs = "r"
        val kPSState = "s"
        val kPStatePosition = "p"
        val kPStateRotation = "r"
        val kPStateBearingEnvelope = "eb"
        val kPStateDistanceEnvelope = "ed"
        val kPSChildren = "c"
        val kPSPeers = "p"
        val kPSGeo = "g"
        val kPGeoCollectorPosition = "cp"
        val kPGeoCollectorRotation = "cr"
        val kPGeoEmitterPosition = "ep"
        val kPGeoEmitterBearingAbsolute = "eba"
        val kPGeoEmitterBearingRelative = "ebr"
        val kPGeoEmitterDistance = "ed"
        val kPDestructorScope = "s"
        val kPDestructorScopeDeep = "d"
        val kPDestructorScopeShallow = "s"
        val kPQuerySections = "s"

        // reports
        val kReports = "r"
        val kRFields = kReports + kEField
        val kRFieldEmitters = kReports + kEFieldEmitter
        val kRFieldOscillators = kReports + kEFieldOscillator
        val kRSubjects = kReports + kESubject
        val kRSubjectEmitters = kReports + kESubjectEmitter
        val kRSubjectOscillators = kReports + kESubjectOscillator
        val kRProbes = kReports + kEProbe
        val kRProbeEmitters = kReports + kEProbeEmitter
        val kRProbeOscillators = kReports + kEProbeOscillator
        val kRProbeCollectors = kReports + kEProbeCollector
        val kRWaveforms = kReports + kEWaveform
        val kRQualities = "q"
        val kRStandardCreationSections = kPSAttrs + kPSRefs + kPSState + kPSChildren

        val kRElementCount = "ec"

        // lookups
        val kLookups = "l"
        val kLProbeCollectors = kLookups + kEProbeCollector

        // qualities
        val kQWavesetId = "w"
        val kQLoudness = "l"
        val kQPitch = "p"
        val kQPeriod = "r"
        val kQShape = "s"
        val kQTone = "t"

        // miscellaneous
        val kAnyKeyBase = "~"
        val kNoneKeyBase = ""
        val kDefinition = "d"
        val kChannelTag = "t" + kChannel
        val kDefaultChannelTag = kDefaultPrefix
        val kChannelDef = kDefinition + kChannel
        val kEnvelope = "e"
        val kEnvelopeDef = kDefinition + kEnvelope
        val kEnvelopeCeiling = "c"
        val kEnvelopeFloor = "f"
        val kEnvelopePoles = "p"
        val kEnvelopePolesDef = kDefinition + kEnvelopePoles
        val kEnvelopePolesDefX = "x"
        val kEnvelopePolesDefY = "y"
        val kEnvelopePolesDefMin = "min"
        val kEnvelopePolesDefMax = "max"
        val kEnvelopePolesPacked = kEnvelopePoles + "p"
        val kEnvelopePolesPackedDef = kDefinition + kEnvelopePolesPacked
        val kEnvelopeSegment = "s"
        val kEnvelopeSegmentX = "x"
        val kEnvelopeSegmentY = "y"
        val kEnvelopeSegmentSlope = "s"
        val kMacro = "m"
        val kPatchDef = kDefinition + "p"
        val kEmitterPatchDef = kPatchDef + kEEmitter
        val kOscillatorPatchDef = kPatchDef + kEOscillator
        val kGeometrySpherical = "2S"
        val kGeometryPlanar = "2P"
        val kDefaultWavesetId = "default.tws"
    }
}

class FieldModel (
    val _cell: Cell,
    trunkModel: TrunkModel)
    (implicit
        val _logger: CellLogger,
        val _trunkModel: TrunkModel)
    extends FieldModelReporter
{
    implicit
    val _fieldModel = this

    private
    var _defaultFieldOpt: Option[Field] = None

    private
    val _fieldPlant = new FieldPlant

    private
    val _fieldEmitterPlant = new FieldEmitterPlant

    def GetFieldEmitterPlant = _fieldEmitterPlant

    private
    val _fieldOscillatorPlant = new FieldOscillatorPlant

    private
    val _subjectPlant = new SubjectPlant

    private
    val _subjectEmitterPlant = new SubjectEmitterPlant

    def GetSubjectEmitterPlant = _subjectEmitterPlant

    private
    val _subjectOscillatorPlant = new SubjectOscillatorPlant

    private
    val _probePlant = new ProbePlant

    private
    val _probeCollectorPlant = new ProbeCollectorPlant

    private
    val _probeEmitterPlant = new ProbeEmitterPlant

    def GetProbeEmitterPlant = _probeEmitterPlant

    private
    val _probeOscillatorPlant = new ProbeOscillatorPlant

    private
    def CommonCreater[ConstructorType, ElementType <: FieldElement[_ <: FieldElement.Key]] (
        fieldKey: Field.Key,
        constructors: Vector[ConstructorType],
        createrFunc: (Field, ConstructorType) => ElementType): Vector[ElementType] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate constructors creating elements:
        constructors.map(createrFunc(field, _))
    }

    private
    def CommonReporter[KeyType <: FieldElement.Key] (
        fieldKey: Field.Key,
        query: FieldElement.Query[KeyType],
        getKeysFunc: => Set[KeyType],
        getReporterFunc: (Field.Key, KeyType) => Option[Reporter]): Vector[JsObject] =
    {
        // Get field (validate):
        GetField(fieldKey)

        // Calculate valid keys as intersect of requested keys and keys known by field:
        val knownKeys = getKeysFunc
        val validKeys = if (query._keys.isEmpty) knownKeys else knownKeys.intersect(query.GetKeys.toSet)

        // Iterate valid keys:
        val reports = validKeys.map(key =>
        {
            val reporter = getReporterFunc(fieldKey, key).get
            reporter.Report(query._sectionsOpt)
        })

        // Return reports vector:
        reports.toVector
    }

    def Report (fieldKey: Field.Key = Field.kAnyKey, sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        if (_defaultFieldOpt.isDefined)
            report ++=
                Json.obj(FieldModel.Glossary.kEDefaultField -> FieldElement.EncodeKey(_defaultFieldOpt.get.GetKey))

        // Add child reports:
        if (sections.HasChildReports)
        {
            // Add plant reports:
            var plantReports = Json.obj()
            val fieldKeys: Vector[Field.Key] =
                if (fieldKey == Field.kAnyKey)
                    _fieldPlant.GetFieldKeys
                else
                    Vector(fieldKey)
            fieldKeys.foreach(fieldKey =>
            {
                plantReports = plantReports ++ Json.obj(
                    FieldElement.EncodeKey(fieldKey) -> Json.obj(
                        FieldModel.Glossary.kEField -> _fieldPlant.Report(fieldKey),
                        FieldModel.Glossary.kEFieldEmitter -> _fieldEmitterPlant.Report(fieldKey),
                        FieldModel.Glossary.kEFieldOscillator -> _fieldOscillatorPlant.Report(fieldKey),
                        FieldModel.Glossary.kESubject -> _subjectPlant.Report(fieldKey),
                        FieldModel.Glossary.kESubjectEmitter -> _subjectEmitterPlant.Report(fieldKey),
                        FieldModel.Glossary.kESubjectOscillator -> _subjectOscillatorPlant.Report(fieldKey),
                        FieldModel.Glossary.kEProbe -> _probePlant.Report(fieldKey),
                        FieldModel.Glossary.kEProbeCollector -> _probeCollectorPlant.Report(fieldKey, sectionsOpt),
                        FieldModel.Glossary.kEProbeEmitter -> _probeEmitterPlant.Report(fieldKey),
                        FieldModel.Glossary.kEProbeOscillator -> _probeOscillatorPlant.Report(fieldKey)))
            })
            report ++= Json.obj("rp" -> plantReports)
        }

        report
    }

    //
    // Fields:
    //

    def CreateFields (constructors: Vector[Field.Constructor]): Vector[Field] =
    {
        // Iterate constructors creating fields:
        constructors.map(constructor =>
        {
            // Determine field's associated trunk:
            val trunk = constructor._trunkKeyOpt match
            {
                case Some(trunkKey) => _trunkModel.GetTrunk(trunkKey)

                case None => _trunkModel.GetDefaultTrunk
            }
            _fieldPlant.CreateField(constructor, trunk)
        })
    }

    def DestroyFields (destructors: Vector[Field.Destructor]): Unit =
    {
        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.FieldDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                val fieldKeys: Vector[Field.Key] =
                    if (destructor._key == Field.kAnyKey)
                    {
                        _fieldPlant.GetFieldKeys
                    }
                    else
                    {
                        // Get field (validate):
                        GetField(destructor._key)

                        Vector(destructor._key)
                    }
                fieldKeys.foreach(fieldKey =>
                {
                    val field = GetField(fieldKey)

                    _fieldEmitterPlant.DestroyAllFieldEmitters(field, destructor._scope)
                    _fieldOscillatorPlant.DestroyAllFieldOscillators(field, destructor._scope)
                    _subjectPlant.DestroyAllSubjects(field, destructor._scope)
                    _subjectEmitterPlant.DestroyAllSubjectEmitters(field, destructor._scope)
                    _subjectOscillatorPlant.DestroyAllSubjectOscillators(field, destructor._scope)
                    _probePlant.DestroyAllProbes(field, destructor._scope)
                    _probeCollectorPlant.DestroyAllProbeCollectors(field, destructor._scope)
                    _probeEmitterPlant.DestroyAllProbeEmitters(field, destructor._scope)
                    _probeOscillatorPlant.DestroyAllProbeOscillators(field, destructor._scope)
                    _fieldPlant.DestroyField(Field.Destructor(fieldKey, destructor._scope))

                    val elementCount = DumpElementCounts(fieldKey)
                    if (elementCount > 0) assert(false)
                })
            })
        }
    }

    def DumpElementCounts (fieldKey: Field.Key): Int =
    {
        val elementCount =
            _fieldPlant.GetElementCount(fieldKey) +
            _fieldEmitterPlant.GetElementCount(fieldKey) +
            _fieldOscillatorPlant.GetElementCount(fieldKey) +
            _subjectPlant.GetElementCount(fieldKey) +
            _subjectEmitterPlant.GetElementCount(fieldKey) +
            _subjectOscillatorPlant.GetElementCount(fieldKey) +
            _probePlant.GetElementCount(fieldKey) +
            _probeCollectorPlant.GetElementCount(fieldKey) +
            _probeEmitterPlant.GetElementCount(fieldKey) +
            _probeOscillatorPlant.GetElementCount(fieldKey)

        _logger.LogDebug(s"@ $elementCount field elements remaining")
        if (elementCount > 0)
        {
            _logger.LogDebug(s"@ f  ${_fieldPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ fe ${_fieldEmitterPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ fo ${_fieldOscillatorPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ s  ${_subjectPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ se ${_subjectEmitterPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ so ${_subjectOscillatorPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ p  ${_probePlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ pc ${_probeCollectorPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ pe ${_probeEmitterPlant.GetElementCount(fieldKey)}")
            _logger.LogDebug(s"@ po ${_probeOscillatorPlant.GetElementCount(fieldKey)}")
        }

        elementCount
    }

    def GetField (fieldKey: Field.Key): Field =
    {
        // Return field:
        _fieldPlant.GetFieldOpt(fieldKey).getOrElse{
           assert(true)
           throw new FieldException(Cell.ErrorCodes.FieldUnknown)}
    }

    def ReportFields (query: Field.Query): Vector[JsObject] =
    {
        // Calculate valid keys as intersect of requested keys and keys known by field:
        val knownKeys = _fieldPlant.GetFieldKeys.toSet
        val validKeys = if (query.keys.isEmpty) knownKeys else knownKeys.intersect(query.keys.toSet)

        // Get reports:
        val reports = validKeys.map(key =>
        {
            val field = GetField(key)
            field.Report(query.sectionsOpt)
        })

        // Return reports vector:
        reports.toVector
    }

    def UpdateFields (updates: Vector[Field.Update]): Unit =
    {
        // Iterate updates:
        updates.foreach(update =>
        {
            // Get field:
            val field = GetField(update._key)

            // Update name:
            update._nameOpt.foreach(name => field.SetNameOpt(Some(name)))

            // Update description:
            update._descriptionOpt.foreach(description => field.SetDescriptionOpt(Some(description)))

            // Update subjects:
            update._subjectUpdatesOpt.foreach(subjectUpdates => UpdateSubjects(field.GetKey, subjectUpdates.toVector))

            // Update probes:
            update._probeUpdatesOpt.foreach(probeUpdates => UpdateProbes(field.GetKey, probeUpdates.toVector))
        })
    }

    //
    // Field Emitters:
    //

    def CreateFieldEmitters (
        fieldKey: Field.Key,
        constructors: Vector[FieldEmitter.Constructor]): Vector[FieldEmitter] =
    {
        CommonCreater[FieldEmitter.Constructor, FieldEmitter](
            fieldKey,
            constructors,
            _fieldEmitterPlant.CreateFieldEmitter)
    }

    def DestroyFieldEmitters (
        fieldKey: Field.Key,
        destructors: Vector[FieldEmitter.Destructor],
        isForcedDestroy: Boolean): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.FieldEmitterDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == FieldEmitter.kAnyKey)
                {
                    // Destroy all field emitters:
                    _fieldEmitterPlant.DestroyAllFieldEmitters(field, destructor._scope)
                }
                else
                {
                    // Get field emitter (validate):
                    GetFieldEmitterOpt(field.GetKey, destructor._key)

                    // Destroy field emitter:
                    _fieldEmitterPlant.DestroyFieldEmitter(field, destructor, isForcedDestroy)
                }
            })
        }
    }

    def GetFieldEmitterOpt (
        fieldKey: Field.Key,
        key: FieldEmitter.Key): Option[FieldEmitter] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return field emitter:
        _fieldEmitterPlant.GetFieldEmitterOpt(field, key, isRequired = true)
    }

    def ReportFieldEmitters (
        fieldKey: Field.Key,
        query: FieldEmitter.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[FieldEmitter.Key](
            fieldKey,
            query,
            field.GetEmitterKeys,
            GetFieldEmitterOpt)
    }

    def UpdateFieldEmitters (
        fieldKey: Field.Key,
        updates: Vector[FieldEmitter.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _fieldEmitterPlant.GetFieldEmitterOpt(field, update._key) match
            {
                // If field emitter is known, update it:
                case Some(fieldEmitter) =>
                    // Update name:
                    update._nameOpt.foreach(name => fieldEmitter.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => fieldEmitter.SetDescriptionOpt(Some(description)))

                case None => throw new FieldException(Cell.ErrorCodes.FieldEmitterUnknown)
            }
        })
    }

    def CallFieldEmitters (
        fieldKey: Field.Key,
        calls: Vector[FieldElement.Call[FieldEmitter.Key]]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate calls:
        calls.foreach(call =>
        {
            _fieldEmitterPlant.GetFieldEmitterOpt(field, call._key) match
            {
                // If field emitter is known, invoke macro on it:
                case Some(fieldEmitter) => fieldEmitter.InvokeMacro(call._macro)

                case None => throw new FieldException(Cell.ErrorCodes.FieldEmitterUnknown)
            }
        })
    }

    //
    // Field Oscillators:
    //

    def CreateFieldOscillators (
        fieldKey: Field.Key,
        fieldEmitterKey: FieldEmitter.Key,
        constructors: Vector[FieldOscillator.Constructor]): Vector[FieldOscillator] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Get field emitter:
        val fieldEmitterOpt = GetFieldEmitterOpt(fieldKey, fieldEmitterKey)

        // Iterate constructors creating oscillators:
        constructors.map(constructor =>
        {
            _fieldOscillatorPlant.CreateFieldOscillator(
                field,
                fieldEmitterOpt.get,
                constructor)
        })
    }

    def DestroyFieldOscillators (
        fieldKey: Field.Key,
        destructors: Vector[FieldOscillator.Destructor],
        isForcedDestroy: Boolean): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.FieldOscillatorDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == FieldOscillator.kAnyKey)
                {
                    // Destroy all oscillators:
                    _fieldOscillatorPlant.DestroyAllFieldOscillators(field, destructor._scope)
                }
                else
                {
                    // Get field oscillator (validate):
                    GetFieldOscillatorOpt(field.GetKey, destructor._key)

                    // Destroy field oscillator:
                    _fieldOscillatorPlant.DestroyFieldOscillator(field, destructor, isForcedDestroy)
                }
            })
        }
    }

    def GetFieldOscillatorOpt (
        fieldKey: Field.Key,
        key: FieldOscillator.Key): Option[FieldOscillator] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return field emitter:
        _fieldOscillatorPlant.GetFieldOscillatorOpt(field, key, isRequired = true)
    }

    def ReportFieldOscillators (
        fieldKey: Field.Key,
        fieldEmitterKey: FieldEmitter.Key,
        query: FieldOscillator.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[FieldOscillator.Key](
            fieldKey,
            query,
            {
                if (fieldEmitterKey == FieldEmitter.kAnyKey || query.keys.isEmpty)
                    _fieldOscillatorPlant.GetFieldOscillatorKeys(field).toSet
                else
                {
                    val emitterOpt = GetFieldEmitterOpt(fieldKey, fieldEmitterKey)
                    emitterOpt.get.GetOscillatorKeys
                }
            },
            GetFieldOscillatorOpt)
    }

    def UpdateFieldOscillators (
        fieldKey: Field.Key,
        updates: Vector[FieldOscillator.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _fieldOscillatorPlant.GetFieldOscillatorOpt(field, update._key) match
            {
                // If field oscillator is known, update it:
                case Some(fieldOscillator) =>
                    // Update name:
                    update._nameOpt.foreach(name => fieldOscillator.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => fieldOscillator.SetDescriptionOpt(Some(description)))

                case None => throw new FieldException(Cell.ErrorCodes.FieldOscillatorUnknown)
            }
        })
    }

    def CallFieldOscillators (
        fieldKey: Field.Key,
        calls: Vector[FieldElement.Call[FieldOscillator.Key]]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate calls:
        calls.foreach(call =>
        {
            _fieldOscillatorPlant.GetFieldOscillatorOpt(field, call._key) match
            {
                // If field oscillator is known, invoke macro on it:
                case Some(fieldOscillator) => fieldOscillator.InvokeMacro(call._macro)

                case None => throw new FieldException(Cell.ErrorCodes.FieldOscillatorUnknown)
            }
        })
    }

    //
    // Subjects:
    //

    def CreateSubjects (
        fieldKey: Field.Key,
        constructors: Vector[Subject.Constructor]): Vector[Subject] =
    {
        CommonCreater[Subject.Constructor, Subject](
            fieldKey,
            constructors,
            _subjectPlant.CreateSubject)
    }

    def DestroySubjects (
        fieldKey: Field.Key,
        destructors: Vector[Subject.Destructor]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.SubjectDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == Subject.kAnyKey)
                {
                    // Destroy all subjects:
                    _subjectPlant.DestroyAllSubjects(field, destructor._scope)
                }
                else
                {
                    // Get subject (validate):
                    GetSubjectOpt(field.GetKey, destructor._key)

                    // Destroy subject:
                    _subjectPlant.DestroySubject(field, destructor)
                }
            })
        }
    }

    def GetSubjectOpt (
        fieldKey: Field.Key,
        key: Subject.Key): Option[Subject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return subject:
        _subjectPlant.GetSubjectOpt(field, key, isRequired = true)
    }

    def ReportSubjects (
        fieldKey: Field.Key,
        query: Subject.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[Subject.Key](
            fieldKey,
            query,
            field.GetSubjectKeys,
            GetSubjectOpt)
    }

    def UpdateSubjects (
        fieldKey: Field.Key,
        updates: Vector[Subject.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _subjectPlant.GetSubjectOpt(field, update._key) match
            {
                // If subject is known, update it:
                case Some(subject) =>
                    // Update name:
                    update._nameOpt.foreach(name => subject.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => subject.SetDescriptionOpt(Some(description)))

                    // Update position:
                    update._positionOpt.foreach(position => subject.SetPosition(position))

                    // Update rotation:
                    update._rotationOpt.foreach(rotation => subject.SetRotation(rotation))

                case None => throw new FieldException(Cell.ErrorCodes.SubjectUnknown)
            }
        })
    }

    //
    // Subject Emitters:
    //

    def CreateSubjectEmitters (
        fieldKey: Field.Key,
        subjectKey: Subject.Key,
        constructors: Vector[SubjectEmitter.Constructor]): Vector[SubjectEmitter] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Get subject:
        val subjectOpt = GetSubjectOpt(fieldKey, subjectKey)

        // Iterate constructors creating subject emitters:
        constructors.map(constructor =>
        {
            _subjectEmitterPlant.CreateSubjectEmitter(
                field,
                subjectOpt.get,
                constructor)
        })
    }

    def DestroySubjectEmitters (
        fieldKey: Field.Key,
        destructors: Vector[SubjectEmitter.Destructor],
        isForcedDestroy: Boolean): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.SubjectEmitterDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SubjectEmitter.kAnyKey)
                {
                    // Destroy all subject emitters:
                    _subjectEmitterPlant.DestroyAllSubjectEmitters(field, destructor._scope)
                }
                else
                {
                    // Get subject emitter (validate):
                   GetSubjectEmitterOpt(field.GetKey, destructor._key)

                    // Destroy subject emitter:
                    _subjectEmitterPlant.DestroySubjectEmitter(field, destructor, isForcedDestroy)
                }
            })
        }
    }

    def GetSubjectEmitterOpt (
        fieldKey: Field.Key,
        key: SubjectEmitter.Key): Option[SubjectEmitter] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return subject emitter:
        _subjectEmitterPlant.GetSubjectEmitterOpt(field, key, isRequired = true)
    }

    def ReportSubjectEmitters (
        fieldKey: Field.Key,
        subjectKey: Subject.Key,
        query: SubjectEmitter.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[SubjectEmitter.Key](
            fieldKey,
            query,
            {
                if (subjectKey == Subject.kAnyKey || query.keys.isEmpty)
                    _subjectEmitterPlant.GetSubjectEmitterKeys(field).toSet
                else
                {
                    val subjectOpt = GetSubjectOpt(fieldKey, subjectKey)
                    subjectOpt.get.GetEmitterKeys
                }
            },
            GetSubjectEmitterOpt)
    }

    def UpdateSubjectEmitters (
        fieldKey: Field.Key,
        updates: Vector[SubjectEmitter.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _subjectEmitterPlant.GetSubjectEmitterOpt(field, update._key) match
            {
                // If subject emitter is known, update it:
                case Some(subjectEmitter) =>
                    // Update name:
                    update._nameOpt.foreach(name => subjectEmitter.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => subjectEmitter.SetDescriptionOpt(Some(description)))

                case None => throw new FieldException(Cell.ErrorCodes.SubjectEmitterUnknown)
            }
        })
    }

    def CallSubjectEmitters (
        fieldKey: Field.Key,
        calls: Vector[FieldElement.Call[SubjectEmitter.Key]]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate calls:
        calls.foreach(call =>
        {
            _subjectEmitterPlant.GetSubjectEmitterOpt(field, call._key) match
            {
                // If subject emitter is known, invoke macro on it:
                case Some(subjectEmitter) => subjectEmitter.InvokeMacro(call._macro)

                case None => throw new FieldException(Cell.ErrorCodes.SubjectEmitterUnknown)
            }
        })
    }

    //
    // Subject Oscillators:
    //

    def CreateSubjectOscillators (
        fieldKey: Field.Key,
        subjectEmitterKey: SubjectEmitter.Key,
        constructors: Vector[SubjectOscillator.Constructor]): Vector[SubjectOscillator] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Get subject emitter:
        val subjectEmitterOpt = GetSubjectEmitterOpt(fieldKey, subjectEmitterKey)

        // Iterate constructors creating subject oscillators:
        constructors.map(constructor =>
        {
            _subjectOscillatorPlant.CreateSubjectOscillator(
                field,
                subjectEmitterOpt.get,
                constructor)
        })
    }

    def DestroySubjectOscillators (
        fieldKey: Field.Key,
        destructors: Vector[SubjectOscillator.Destructor],
        isForcedDestroy: Boolean): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.SubjectOscillatorDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SubjectOscillator.kAnyKey)
                {
                    // Destroy all oscillators:
                    _subjectOscillatorPlant.DestroyAllSubjectOscillators(field, destructor._scope)
                }
                else
                {
                    // Get subject oscillator (validate):
                    GetSubjectOscillatorOpt(field.GetKey, destructor._key)

                    // Destroy subject oscillator:
                    _subjectOscillatorPlant.DestroySubjectOscillator(field, destructor, isForcedDestroy)
                }
            })
        }
    }

    def GetSubjectOscillatorOpt (
        fieldKey: Field.Key,
        key: SubjectOscillator.Key): Option[SubjectOscillator] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return subject emitter:
        _subjectOscillatorPlant.GetSubjectOscillatorOpt(field, key, isRequired = true)
    }

    def ReportSubjectOscillators (
        fieldKey: Field.Key,
        subjectEmitterKey: SubjectEmitter.Key,
        query: SubjectOscillator.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[SubjectOscillator.Key](
            fieldKey,
            query,
            {
                if (subjectEmitterKey == SubjectEmitter.kAnyKey || query.keys.isEmpty)
                    _subjectOscillatorPlant.GetSubjectOscillatorKeys(field).toSet
                else
                {
                    val emitterOpt = GetSubjectEmitterOpt(fieldKey, subjectEmitterKey)
                    emitterOpt.get.GetOscillatorKeys
                }
            },
            GetSubjectOscillatorOpt)
    }

    def UpdateSubjectOscillators (
        fieldKey: Field.Key,
        updates: Vector[SubjectOscillator.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _subjectOscillatorPlant.GetSubjectOscillatorOpt(field, update._key) match
            {
                // If subject emitter is known, update it:
                case Some(subjectOscillator) =>
                    // Update name:
                    update._nameOpt.foreach(name => subjectOscillator.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => subjectOscillator.SetDescriptionOpt(Some(description)))

                case None => throw new FieldException(Cell.ErrorCodes.SubjectOscillatorUnknown)
            }
        })
    }

    def CallSubjectOscillators (
        fieldKey: Field.Key,
        calls: Vector[FieldElement.Call[SubjectOscillator.Key]]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate calls:
        calls.foreach(call =>
        {
            _subjectOscillatorPlant.GetSubjectOscillatorOpt(field, call._key) match
            {
                // If field oscillator is known, invoke macro on it:
                case Some(subjectOscillator) => subjectOscillator.InvokeMacro(call._macro)

                case None => throw new FieldException(Cell.ErrorCodes.SubjectOscillatorUnknown)
            }
        })
    }

    //
    // Probes:
    //

    def CreateProbes (
        fieldKey: Field.Key,
        constructors: Vector[Probe.Constructor]): Vector[Probe] =
    {
        CommonCreater[Probe.Constructor, Probe](
            fieldKey,
            constructors,
            _probePlant.CreateProbe)
    }

    def DestroyProbes (
        fieldKey: Field.Key,
        destructors: Vector[Probe.Destructor]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.ProbeDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == Probe.kAnyKey)
                {
                    // Destroy all probes:
                    _probePlant.DestroyAllProbes(field, destructor._scope)
                }
                else
                {
                    // Get probe (validate):
                    GetProbeOpt(field.GetKey, destructor._key)

                    // Destroy probe:
                    _probePlant.DestroyProbe(field, destructor)
                }
            })
        }
    }

    def GetProbeOpt (
        fieldKey: Field.Key,
        key: Probe.Key): Option[Probe] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return probe:
        _probePlant.GetProbeOpt(field, key, isRequired = true)
    }

    def ReportProbes (
        fieldKey: Field.Key,
        query: Probe.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[Probe.Key](
            fieldKey,
            query,
            field.GetProbeKeys,
            GetProbeOpt)
    }

    def UpdateProbes (
        fieldKey: Field.Key,
        updates: Vector[Probe.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _probePlant.GetProbeOpt(field, update._key) match
            {
                // If probe is known, update it:
                case Some(probe) =>
                    // Update name:
                    update._nameOpt.foreach(name => probe.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => probe.SetDescriptionOpt(Some(description)))

                    // Update position:
                    update._positionOpt.foreach(position => probe.SetPosition(position))

                    // Update rotation:
                    update._rotationOpt.foreach(rotation => probe.SetRotation(rotation))

                case None => throw new FieldException(Cell.ErrorCodes.ProbeUnknown)
            }
        })
    }

    def CallProbeEmitters (
        fieldKey: Field.Key,
        calls: Vector[FieldElement.Call[ProbeEmitter.Key]]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate calls:
        calls.foreach(call =>
        {
            _probeEmitterPlant.GetProbeEmitterOpt(field, call._key) match
            {
                // If probe emitter is known, invoke macro on it:
                case Some(probeEmitter) => probeEmitter.InvokeMacro(call._macro)

                case None => throw new FieldException(Cell.ErrorCodes.ProbeEmitterUnknown)
            }
        })
    }

    //
    // Probe Collectors:
    //

    def CreateProbeCollectors (
        fieldKey: Field.Key,
        probeKey: Probe.Key,
        constructors: Vector[ProbeCollector.Constructor]): Vector[ProbeCollector] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Get probe:
        val probeOpt = GetProbeOpt(fieldKey, probeKey)

        // Iterate constructors creating probe collectors:
        constructors.map(constructor =>
        {
            _probeCollectorPlant.CreateProbeCollector(
                field,
                probeOpt.get,
                constructor)
        })
    }

    def DestroyProbeCollectors (
        fieldKey: Field.Key,
        destructors: Vector[ProbeCollector.Destructor]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.ProbeCollectorDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == ProbeCollector.kAnyKey)
                {
                    // Destroy all probe collectors:
                    _probeCollectorPlant.DestroyAllProbeCollectors(field, destructor._scope)
                }
                else
                {
                    // Get probe collector (validate):
                    GetProbeCollectorOpt(field.GetKey, destructor._key)

                    // Destroy probe collector:
                    _probeCollectorPlant.DestroyProbeCollector(field, destructor)
                }
            })
        }
    }

    def CallProbeOscillators (
        fieldKey: Field.Key,
        calls: Vector[FieldElement.Call[ProbeOscillator.Key]]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate calls:
        calls.foreach(call =>
        {
            _probeOscillatorPlant.GetProbeOscillatorOpt(field, call._key) match
            {
                // If field oscillator is known, invoke macro on it:
                case Some(probeOscillator) => probeOscillator.InvokeMacro(call._macro)

                case None => throw new FieldException(Cell.ErrorCodes.ProbeOscillatorUnknown)
            }
        })
    }

    def GetProbeCollectorOpt (
        fieldKey: Field.Key,
        key: ProbeCollector.Key): Option[ProbeCollector] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return probe collector:
        _probeCollectorPlant.GetProbeCollectorOpt(field, key, isRequired = true)
    }

    def LookupProbeCollector(
        fieldKey: Field.Key,
        lookupAlias: String): JsObject =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        val result = _probeCollectorPlant.LookupProbeCollector(field, lookupAlias) match
        {
            case Some(collectorKey) => FieldElement.EncodeKey(collectorKey)

            case None => ""
        }
        Json.obj(lookupAlias -> result)
    }

    def ReportProbeCollectors (
        fieldKey: Field.Key,
        probeKey: Probe.Key,
        query: ProbeCollector.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[ProbeCollector.Key](
            fieldKey,
            query,
            {
                if (probeKey == Probe.kAnyKey || query.keys.isEmpty)
                    _probeCollectorPlant.GetProbeCollectorKeys(field).toSet
                else
                {
                    val probeOpt = GetProbeOpt(fieldKey, probeKey)
                    probeOpt.get.GetCollectorKeys.toSet
                }
            },
            GetProbeCollectorOpt)
    }

    def UpdateProbeCollectors (
        fieldKey: Field.Key,
        updates: Vector[ProbeCollector.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _probeCollectorPlant.GetProbeCollectorOpt(field, update._key) match
            {
                // If probe collector is known, update it:
                case Some(probeCollector) =>
                    // Update name:
                    update._nameOpt.foreach(name => probeCollector.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => probeCollector.SetDescriptionOpt(Some(description)))

                    // Update acoustic_a:
                    update._acoustic_aOpt.foreach(acoustic_a => probeCollector.SetAcoustic_a(acoustic_a))

                    // Update squelch threshold:
                    update._squelchThresholdOpt.foreach(
                        squelchThreshold => probeCollector.SetSquelchThresholdOpt(Some(squelchThreshold)))

                    // Update lobe range:
                    update._lobeRangeOpt.foreach(
                        lobeRange => probeCollector.SetLobeRangeOpt(Some(lobeRange)))

                    // Update lobe range poles:
                    update._lobeRangePolesOpt.foreach(
                        lobeRangePoles => probeCollector.SetLobeRangePolesOpt(Some(lobeRangePoles)))

                    // Update lobe bearing poles:
                    update._lobeBearingPolesOpt.foreach(
                        lobeBearingPoles => probeCollector.SetLobeBearingPolesOpt(Some(lobeBearingPoles)))

                case None => throw new FieldException(Cell.ErrorCodes.ProbeCollectorUnknown)
            }
        })
    }

    //
    // Probe Emitters:
    //

    def CreateProbeEmitters (
        fieldKey: Field.Key,
        probeKey: Probe.Key,
        constructors: Vector[ProbeEmitter.Constructor]): Vector[ProbeEmitter] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Get probe:
        val probeOpt = GetProbeOpt(fieldKey, probeKey)

        // Iterate constructors creating probe emitters:
        constructors.map(constructor =>
        {
            _probeEmitterPlant.CreateProbeEmitter(
                field,
                probeOpt.get,
                constructor)
        })
    }

    def DestroyProbeEmitters (
        fieldKey: Field.Key,
        destructors: Vector[ProbeEmitter.Destructor]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.ProbeEmitterDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == ProbeEmitter.kAnyKey)
                {
                    // Destroy all probe emitters:
                    _probeEmitterPlant.DestroyAllProbeEmitters(field, destructor._scope)
                }
                else
                {
                    // Get probe emitter (validate):
                    GetProbeEmitterOpt(field.GetKey, destructor._key)

                    // Destroy probe emitter:
                    _probeEmitterPlant.DestroyProbeEmitter(field, destructor)
                }
            })
        }
    }

    def GetProbeEmitterOpt (
        fieldKey: Field.Key,
        key: ProbeEmitter.Key): Option[ProbeEmitter] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return probe emitter:
        _probeEmitterPlant.GetProbeEmitterOpt(field, key, isRequired = true)
    }

    def ReportProbeEmitters (
        fieldKey: Field.Key,
        probeKey: Probe.Key,
        query: ProbeEmitter.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[ProbeEmitter.Key](
            fieldKey,
            query,
            {
                if (probeKey == Probe.kAnyKey || query.keys.isEmpty)
                    _probeEmitterPlant.GetProbeEmitterKeys(field).toSet
                else
                {
                    val probeOpt = GetProbeOpt(fieldKey, probeKey)
                    probeOpt.get.GetEmitterKeys
                }
            },
            GetProbeEmitterOpt)
    }

    def UpdateProbeEmitters (
        fieldKey: Field.Key,
        updates: Vector[ProbeEmitter.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _probeEmitterPlant.GetProbeEmitterOpt(field, update._key) match
            {
                // If probe emitter is known, update it:
                case Some(probeEmitter) =>
                    // Update name:
                    update._nameOpt.foreach(name => probeEmitter.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => probeEmitter.SetDescriptionOpt(Some(description)))

                case None => throw new FieldException(Cell.ErrorCodes.ProbeEmitterUnknown)
            }
        })
    }

    //
    // Probe Oscillators:
    //

    def CreateProbeOscillators (
        fieldKey: Field.Key,
        probeEmitterKey: ProbeEmitter.Key,
        constructors: Vector[ProbeOscillator.Constructor]): Vector[ProbeOscillator] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Get probe emitter:
        val probeEmitterOpt = GetProbeEmitterOpt(fieldKey, probeEmitterKey)

        // Iterate constructors creating probe oscillators:
        constructors.map(constructor =>
        {
            _probeOscillatorPlant.CreateProbeOscillator(
                field,
                probeEmitterOpt.get,
                constructor)
        })
    }

    def DestroyProbeOscillators (
        fieldKey: Field.Key,
        destructors: Vector[ProbeOscillator.Destructor],
        isForcedDestroy: Boolean): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        if (destructors.isEmpty)
            throw new FieldException(Cell.ErrorCodes.ProbeOscillatorDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == ProbeOscillator.kAnyKey)
                {
                    // Destroy all oscillators:
                    _probeOscillatorPlant.DestroyAllProbeOscillators(field, destructor._scope)
                }
                else
                {
                    // Get probe oscillator (validate):
                    GetProbeOscillatorOpt(field.GetKey, destructor._key)

                    // Destroy probe oscillator:
                    _probeOscillatorPlant.DestroyProbeOscillator(field, destructor, isForcedDestroy)
                }
            })
        }
    }

    def GetProbeOscillatorOpt (
        fieldKey: Field.Key,
        key: ProbeOscillator.Key): Option[ProbeOscillator] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return probe emitter:
        _probeOscillatorPlant.GetProbeOscillatorOpt(field, key, isRequired = true)
    }

    def ReportProbeOscillators (
        fieldKey: Field.Key,
        probeEmitterKey: ProbeEmitter.Key,
        query: ProbeOscillator.Query): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        CommonReporter[ProbeOscillator.Key](
            fieldKey,
            query,
            {
                if (probeEmitterKey == ProbeEmitter.kAnyKey || query.keys.isEmpty)
                    _probeOscillatorPlant.GetProbeOscillatorKeys(field).toSet
                else
                {
                    val emitterOpt = GetProbeEmitterOpt(fieldKey, probeEmitterKey)
                    emitterOpt.get.GetOscillatorKeys
                }
            },
            GetProbeOscillatorOpt)
    }

    def UpdateProbeOscillators (
        fieldKey: Field.Key,
        updates: Vector[ProbeOscillator.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _probeOscillatorPlant.GetProbeOscillatorOpt(field, update._key) match
            {
                // If probe emitter is known, update it:
                case Some(probeOscillator) =>
                    // Update name:
                    update._nameOpt.foreach(name => probeOscillator.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => probeOscillator.SetDescriptionOpt(Some(description)))

                case None => throw new FieldException(Cell.ErrorCodes.ProbeOscillatorUnknown)
            }
        })
    }

    //
    // Waveforms:
    //

    private
    def CalculateLoudnessQuality (
        field: Field,
        signal: Signal[Continuous],
        emitterDistanceMeters: Real,
        emitterBearingUnit: Real,
        acoustic_aOpt: Option[Real],
        lobeRangeOpt: Option[Real],
        lobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        lobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope]): Real =
    {
        val loudness:Real =
            // If signal is below the first percentile level then consider the oscillator to be noiseless:
            if (signal._scalar < 0.01f)
                0f
            else
            {
                // Determine effective sound power level (dB):
                val SWL = signal._scalar * 140f

                // Determine sound pressure wave surface area (m^2):
                val A = 4f * math.Pi * (emitterDistanceMeters * emitterDistanceMeters)

                // Determine sound pressure level:
                val SPL = acoustic_aOpt match
                {
                    case Some(acoustic_a) =>
                        // Since acoustic_a (size of collector surface area) is defined, we'll assume it is an
                        // indication that the user desires the general computation of sound pressure level from sound
                        // intensity and acoustic impedence terms.

                        val kP0 = 2f * Math.pow(10f, -5f) // sound pressure reference (Pa)
                        val kT = 1f // simulated sample time (s)

                        // Determine sound energy (J):
                        val W = Math.pow(10f, SWL / 10f - 12f)
                        // Determine sound power (W):
                        val P = W / kT
                        // Determine sound intensity (W/m^2):
                        val J = P * acoustic_a / A

                        // Determine acoustic impedence:
                        val Z = field.GetAcoustic_r * field.GetAcoustic_c

                        // Determine sound pressure (Pa):
                        val p = math.sqrt(J * Z)
                        // Determine sound pressure level (dB):
                        val SPL = 20f * math.log10(p / kP0)
                        SPL

                    case None =>
                        // Since acoustic_a (size of collector surface area) is not defined, we'll assume it is 1.0 m^2 and
                        // also use it as an indication that the user desires simple sound pressure level estimation from
                        // known sound power level instead of the more complex formula above.

                        // Estimate sound pressure level (dB):
                        val SPL = SWL - 10f * math.log10(A)
                        SPL
                }

                // Determine total lobe trim:
                val totalTrim = lobeRangeOpt match
                {
                    case Some(lobeRangeMeters) =>
                        // Clamp emitter distance to lobe range:
                        val emitterLobeDistanceMeters =
                            if (emitterDistanceMeters > lobeRangeMeters)
                                lobeRangeMeters
                            else
                                emitterDistanceMeters

                        // Determine range trim envelope, default to max gain:
                        val rangeEnvelope = lobeRangeEnvelopeOpt.getOrElse(
                            Envelope.DecodeContinuousEnvelopeDef(Envelope.kMaxGainEnvelopeDef))

                        // Determine range trim:
                        val rangeIn = emitterLobeDistanceMeters / lobeRangeMeters
                        val rangeTrim = Envelope.ModulateWithContinuousEnvelope(rangeIn, rangeEnvelope)

                        // Determine bearing trim envelope, default to max gain:
                        val bearingEnvelope = lobeBearingEnvelopeOpt.getOrElse(
                            Envelope.DecodeContinuousEnvelopeDef(Envelope.kMaxGainEnvelopeDef))

                        // Determine bearing trim:
                        val bearingIn = 1f - 2f * Math.abs(emitterBearingUnit - 0.5f)
                        val bearingTrim = Envelope.ModulateWithContinuousEnvelope(bearingIn, bearingEnvelope)

                        rangeTrim * bearingTrim

                    case _ => 1f
                }

                // Apply lobe trim:
                SPL * totalTrim
            }

        loudness
    }

    private
    def CalculatePitchQuality (signal: Signal[Continuous]): Real =
        signal._scalar * FieldModel.kMaxPitch    // hertz

    private
    def CalculatePeriodQuality (signal: Signal[Discrete]): Real =
        Math.pow(2f, signal._scalar)  // seconds

    private
    def CalculateShapeQuality (signal: Signal[Discrete]): Real =
        signal._scalar   // nominal

    private
    def CalculateToneQuality (signal: Signal[Discrete]): Real =
        signal._scalar   // nominal

    private
    def CalculateOscillatorWaveformQualities (
        field: Field,
        oscillatorPatch: OscillatorPatch,
        emitterDistanceMeters: Real,
        emitterRelativeBearingUnit: Real,
        acoustic_aOpt: Option[Real],
        lobeRangeOpt: Option[Real],
        lobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        lobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        wavesetId: String): Waveform.Qualities =
    {
        val loudnessQuality: Real =
        {
            val loudnessOutput = GetSignalOutputOpt(field.GetKey, oscillatorPatch.GetLoudnessOutputKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.SignalOutputBroken))
            loudnessOutput.GetSignalOpt match
            {
                case Some(signal) =>
                    val loudness = CalculateLoudnessQuality(
                        field,
                        signal.asInstanceOf[Signal[Continuous]],
                        emitterDistanceMeters,
                        emitterRelativeBearingUnit,
                        acoustic_aOpt,
                        lobeRangeOpt,
                        lobeRangeEnvelopeOpt,
                        lobeBearingEnvelopeOpt)
                    loudness.toDouble

                case None => 0f
            }
        }

        val pitchQuality: Real =
        {
            val pitchOutput = GetSignalOutputOpt(field.GetKey, oscillatorPatch.GetPitchOutputKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.SignalOutputBroken))
            pitchOutput.GetSignalOpt match
            {
                case Some(signal) =>
                    val pitch = CalculatePitchQuality(signal.asInstanceOf[Signal[Continuous]])
                    pitch.toDouble

                case None => 0f
            }
        }

        val periodQuality: Integer =
        {
            val periodOutput = GetSignalOutputOpt(field.GetKey, oscillatorPatch.GetPeriodOutputKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.SignalOutputBroken))
            periodOutput.GetSignalOpt match
            {
                case Some(signal) =>
                    val period = CalculatePeriodQuality(signal.asInstanceOf[Signal[Discrete]])
                    period.toInt

                case None => 0
            }
        }

        val shapeQuality: Integer =
        {
            val shapeOutput = GetSignalOutputOpt(field.GetKey, oscillatorPatch.GetShapeOutputKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.SignalOutputBroken))
            shapeOutput.GetSignalOpt match
            {
                case Some(signal) =>
                    val shape = CalculateShapeQuality(signal.asInstanceOf[Signal[Discrete]])
                    shape.toInt

                case None => 0
            }
        }

        val toneQuality: Integer =
        {
            val toneOutput = GetSignalOutputOpt(field.GetKey, oscillatorPatch.GetToneOutputKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.SignalOutputBroken))
            toneOutput.GetSignalOpt match
            {
                case Some(signal) =>
                    val tone = CalculateToneQuality(signal.asInstanceOf[Signal[Discrete]])
                    tone.toInt

                case None => 0
            }
        }

        new Waveform.Qualities(
            wavesetId,
            loudnessQuality,
            pitchQuality,
            periodQuality,
            shapeQuality,
            toneQuality)
    }

    def ReportWaveformsAtSampler (
        fieldKey: Field.Key,
        query: Sampler.Query): Vector[JsObject] =
    {
        import scala.collection.mutable

        // Get field (validate):
        val field = GetField(fieldKey)

        // Collect waveforms:
        val collectorPosition = query.collectorPositionOpt.getOrElse(new Body.Position)
        val collectorRotation = query.collectorRotationOpt.getOrElse(new Body.Rotation)
        val antipodeDistanceMeters: Real = query.antipodeDistanceOpt.getOrElse(field.GetAntipodeDistance)
        val fieldGeometry = query.fieldGeometryOpt.getOrElse(field.GetGeometry)
        val waveforms = fieldGeometry match
        {
            case FieldModel.Glossary.kGeometryPlanar =>
                CollectWaveforms_2P(
                    field,
                    antipodeDistanceMeters,
                    collectorPosition,
                    collectorRotation,
                    query.acoustic_aOpt,
                    query.lobeRangeOpt,
                    query.lobeRangeEnvelopeOpt,
                    query.lobeBearingEnvelopeOpt)

            case FieldModel.Glossary.kGeometrySpherical =>
                CollectWaveforms_2S(
                    field,
                    antipodeDistanceMeters,
                    collectorPosition,
                    collectorRotation,
                    query.acoustic_aOpt,
                    query.lobeRangeOpt,
                    query.lobeRangeEnvelopeOpt,
                    query.lobeBearingEnvelopeOpt)

            case _ => Vector.empty[Waveform]
        }

        // Determine waveform squelch threshold:
        val squelchThresholdDb: Real = query.squelchThresholdOpt.getOrElse(0f)

        // Partition waveforms into those with body geometry and those without:
        val bodyWaveforms = new mutable.ArrayBuffer[Waveform]
        val fieldWaveforms = new mutable.ArrayBuffer[Waveform]
        waveforms.foreach(waveform =>
        {
            // Apply loudness thresholding here:
            if (waveform._qualities._loudnessDecibels > 0f &&
                waveform._qualities._loudnessDecibels > squelchThresholdDb)
            {
                if (waveform._geometryOpt.isDefined)
                    bodyWaveforms.append(waveform)
                else
                    fieldWaveforms.append(waveform)
            }
        })
        // Sort body geometry waveforms by collector/emitter distance:
        val bodyWaveformsSorted = bodyWaveforms.sortWith(
            _._geometryOpt.get._emitterDistanceMeters < _._geometryOpt.get._emitterDistanceMeters)

        // Generate reports by appending body geometry wavefield reports to the end of geometry-less ones:
        val reports =
            fieldWaveforms.map(_.Report(new ReportSectionsParser(query.sectionsOpt))) ++
            bodyWaveformsSorted.map(_.Report(new ReportSectionsParser(query.sectionsOpt)))

        // Return reports vector:
        reports.toVector
    }

    def ReportWaveformsAtProbe (
        fieldKey: Field.Key,
        probeKey: Probe.Key,
        query: Simpler.Query): Vector[JsObject] =
    {
        val probe = GetProbeOpt(fieldKey, probeKey).get
        val probeCollectorKey = probe.GetDefaultCollectorKey
        ReportWaveformsAtProbeCollector(fieldKey, probeCollectorKey, query, Some(probe))
    }

    def ReportWaveformsAtProbeCollector (
        fieldKey: Field.Key,
        probeCollectorKey: ProbeCollector.Key,
        query: Simpler.Query,
        probeOpt: Option[Probe] = None): Vector[JsObject] =
    {
        val field = GetField(fieldKey)
        val probeCollector = GetProbeCollectorOpt(fieldKey, probeCollectorKey).get
        val probe: Probe = probeOpt.getOrElse
        {
            val probeKey = probeCollector.GetParentKey.asInstanceOf[Probe.Key]
            GetProbeOpt(fieldKey, probeKey).get
        }

        val samplerQuery = new Sampler.Query(
            fieldGeometryOpt = Some(field.GetGeometry),
            antipodeDistanceOpt = Some(field.GetAntipodeDistance),
            collectorPositionOpt = Some(probe.GetPosition),
            collectorRotationOpt = Some(probe.GetRotation),
            acoustic_aOpt = Some(probeCollector.GetAcoustic_a),
            squelchThresholdOpt = probeCollector.GetSquelchThresholdOpt,
            lobeRangeOpt = probeCollector.GetLobeRangeOpt,
            lobeRangeEnvelopeOpt = probeCollector.GetLobeRangeEnvelopeOpt,
            lobeBearingEnvelopeOpt = probeCollector.GetLobeBearingEnvelopeOpt,
            sectionsOpt = query.sectionsOpt)

        ReportWaveformsAtSampler(fieldKey, samplerQuery)
    }

    def CollectWaveforms_2P (
        field: Field,
        antipodeDistanceMeters: Real,
        collectorPosition: Body.Position,
        collectorRotation: Body.Rotation,
        acoustic_aOpt: Option[Real],
        lobeRangeOpt: Option[Real],
        lobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        lobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope]): Vector[Waveform] =
    {
        def CalculateEmitterAbsoluteBearingUnit (
            emitterLatMeters: Real,
            emitterLonMeters: Real,
            collectorLatMeters: Real,
            collectorLonMeters: Real): Real =
        {
            var bearingRad = math.atan2(
                emitterLatMeters - collectorLatMeters,
                emitterLonMeters - collectorLonMeters)
            if (bearingRad < 0)
                bearingRad = (math.Pi * 2) + bearingRad

            // Mirror and rotate bearing to canonical form:
            bearingRad = ((math.Pi * 2) - bearingRad) - (math.Pi * 3 / 2)
            if (bearingRad < 0)
                bearingRad += math.Pi * 2

            val bearingUnit = bearingRad / (math.Pi * 2)
            bearingUnit
        }

        def CalculateEmitterDistanceMeters (
            emitterLatMeters: Real,
            emitterLonMeters: Real,
            collectorLatMeters: Real,
            collectorLonMeters: Real): Real =
        {
            val distanceMeters = math.sqrt(
                ((emitterLonMeters - collectorLonMeters) * (emitterLonMeters - collectorLonMeters)) +
                ((emitterLatMeters - collectorLatMeters) * (emitterLatMeters - collectorLatMeters)))
            if (distanceMeters < 1f) 1f else distanceMeters
        }

        def CalculateLatitudeMeters (fieldRadiusMeters: Real, xUnit: Real): Real =
            xUnit * fieldRadiusMeters

        def CalculateLongitudeMeters (fieldRadiusMeters: Real, yUnit: Real): Real =
            yUnit * fieldRadiusMeters

        val fieldRadiusMeters = antipodeDistanceMeters

        val collectorLatMeters = CalculateLatitudeMeters(fieldRadiusMeters, collectorPosition._x)
        val collectorLonMeters = CalculateLongitudeMeters(fieldRadiusMeters, collectorPosition._y)

        var waveforms = Vector.empty[Waveform]

        // Calculate field emitter waveforms:
        val emitters = _fieldEmitterPlant.GetFieldEmitters(field)
        emitters.foreach(emitter =>
        {
            emitter.Emit()

            val oscillators = _fieldOscillatorPlant.GetFieldOscillators(field, Some(emitter))
            oscillators.foreach(oscillator =>
            {
                val oscillatorPatch = GetOscillatorPatchOpt(field.GetKey, oscillator.GetPatchKey).getOrElse(
                    throw new FieldException(Cell.ErrorCodes.OscillatorPatchBroken))

                val waveformQualities = CalculateOscillatorWaveformQualities(
                    field,
                    oscillatorPatch,
                    emitterDistanceMeters = 1f,
                    emitterRelativeBearingUnit = 0f,
                    acoustic_aOpt,
                    lobeRangeOpt = None,
                    lobeRangeEnvelopeOpt = None,
                    lobeBearingEnvelopeOpt = None,
                    oscillatorPatch.GetWavesetId)

                waveforms ++= Vector(new Waveform(
                    waveformQualities,
                    new Waveform.Refs(
                        None,
                        emitter.GetKey,
                        oscillator.GetKey),
                        None))
            })
        })

        // Calculate subject emitter waveforms:
        val subjects = _subjectPlant.GetSubjects(field)
        subjects.foreach(subject =>
        {
            val emitterLatMeters = CalculateLatitudeMeters(fieldRadiusMeters, subject.GetPosition._x)
            val emitterLonMeters = CalculateLongitudeMeters(fieldRadiusMeters, subject.GetPosition._y)

            val emitterDistanceMeters = CalculateEmitterDistanceMeters(
                emitterLatMeters,
                emitterLonMeters,
                collectorLatMeters,
                collectorLonMeters)

            val emitterAbsoluteBearingUnit = CalculateEmitterAbsoluteBearingUnit(
                emitterLatMeters,
                emitterLonMeters,
                collectorLatMeters,
                collectorLonMeters)
            val emitterRelativeBearingUnit = (emitterAbsoluteBearingUnit - collectorRotation._w + 1f) % 1f

            val emitters = _subjectEmitterPlant.GetSubjectEmitters(field, Some(subject))
            emitters.foreach(emitter =>
            {
                emitter.Emit()

                val emitterPosition = subject.GetPosition

                val oscillators = _subjectOscillatorPlant.GetSubjectOscillators(field, Some(emitter))
                oscillators.foreach(oscillator =>
                {
                    val oscillatorPatch = GetOscillatorPatchOpt(field.GetKey, oscillator.GetPatchKey).getOrElse(
                        throw new FieldException(Cell.ErrorCodes.OscillatorPatchBroken))

                    val waveformQualities = CalculateOscillatorWaveformQualities(
                        field,
                        oscillatorPatch,
                        emitterDistanceMeters,
                        emitterRelativeBearingUnit,
                        acoustic_aOpt,
                        lobeRangeOpt,
                        lobeRangeEnvelopeOpt,
                        lobeBearingEnvelopeOpt,
                        oscillatorPatch.GetWavesetId)

                    waveforms ++= Vector(new Waveform(
                        waveformQualities,
                        new Waveform.Refs(
                            Some(subject.GetKey),
                            emitter.GetKey,
                            oscillator.GetKey),
                        Some(new Waveform.Geometry(
                            collectorPosition,
                            emitterPosition,
                            emitterAbsoluteBearingUnit.toDouble,
                            emitterRelativeBearingUnit.toDouble,
                            emitterDistanceMeters.toDouble))))
                })
            })
        })

        // Calculate probe emitter waveforms:
        val probes = _probePlant.GetProbes(field)
        probes.foreach(probe =>
        {
            val emitterLatMeters = CalculateLatitudeMeters(fieldRadiusMeters, probe.GetPosition._x)
            val emitterLonMeters = CalculateLongitudeMeters(fieldRadiusMeters, probe.GetPosition._y)

            val emitterDistanceMeters = CalculateEmitterDistanceMeters(
                emitterLatMeters,
                emitterLonMeters,
                collectorLatMeters,
                collectorLonMeters)

            val emitterAbsoluteBearingUnit = CalculateEmitterAbsoluteBearingUnit(
                emitterLatMeters,
                emitterLonMeters,
                collectorLatMeters,
                collectorLonMeters)
            val emitterRelativeBearingUnit = (emitterAbsoluteBearingUnit - collectorRotation._w + 1f) % 1f

            val emitters = _probeEmitterPlant.GetProbeEmitters(field, Some(probe))
            emitters.foreach(emitter =>
            {
                emitter.Emit()

                val emitterPosition = probe.GetPosition

                val oscillators = _probeOscillatorPlant.GetProbeOscillators(field, Some(emitter))
                oscillators.foreach(oscillator =>
                {
                    val oscillatorPatch = GetOscillatorPatchOpt(field.GetKey, oscillator.GetPatchKey).getOrElse(
                        throw new FieldException(Cell.ErrorCodes.OscillatorPatchBroken))

                    val waveformQualities = CalculateOscillatorWaveformQualities(
                        field,
                        oscillatorPatch,
                        emitterDistanceMeters,
                        emitterRelativeBearingUnit,
                        acoustic_aOpt,
                        lobeRangeOpt,
                        lobeRangeEnvelopeOpt,
                        lobeBearingEnvelopeOpt,
                        oscillatorPatch.GetWavesetId)

                    waveforms ++= Vector(new Waveform(
                        waveformQualities,
                        new Waveform.Refs(
                            Some(probe.GetKey),
                            emitter.GetKey,
                            oscillator.GetKey),
                        Some(new Waveform.Geometry(
                            collectorPosition,
                            emitterPosition,
                            emitterAbsoluteBearingUnit.toDouble,
                            emitterRelativeBearingUnit.toDouble,
                            emitterDistanceMeters.toDouble))))
                })
            })
        })

        // Return waveforms vector:
        waveforms
    }

    def CollectWaveforms_2S (
        field: Field,
        antipodeDistanceMeters: Real,
        collectorPosition: Body.Position,
        collectorRotation: Body.Rotation,
        acoustic_aOpt: Option[Real],
        lobeRangeOpt: Option[Real],
        lobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        lobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope]): Vector[Waveform] =
    {
        def CalculateEmitterBearingUnit (
            emitterLatRad: Real,
            emitterLonRad: Real,
            collectorLatRad: Real,
            collectorLonRad: Real): Real =
        {
            var bearingRad = math.atan2(
                math.sin(emitterLonRad - collectorLonRad) * math.cos(emitterLatRad),
                math.cos(collectorLatRad) * math.sin(emitterLatRad) -
                math.sin(collectorLatRad) * math.cos(emitterLatRad) * math.cos(emitterLonRad - collectorLonRad))
            if (bearingRad < 0)
                bearingRad += math.Pi * 2
            val bearingUnit = bearingRad / (math.Pi * 2)
            bearingUnit
        }

        def CalculateEmitterDistanceMeters (
            emitterLatRad: Real,
            emitterLonRad: Real,
            collectorLatRad: Real,
            collectorLonRad: Real,
            fieldRadiusMeters: Double): Real =
        {
            val distanceRad = 2 * math.asin(
                math.sqrt(
                    math.sin((emitterLatRad - collectorLatRad) / 2) * math.sin((emitterLatRad - collectorLatRad) / 2) +
                    math.cos(emitterLatRad) * math.cos(collectorLatRad) *
                    math.sin((emitterLonRad - collectorLonRad) / 2) * math.sin((emitterLonRad - collectorLonRad) / 2)))
            val distanceMeters = distanceRad * fieldRadiusMeters
            if (distanceMeters < 1f) 1f else distanceMeters
        }

        def CalculateLatitudeRad (xUnit: Real): Real =
            xUnit * (math.Pi / 2)

        def CalculateLongitudeRad (yUnit: Real): Real =
            yUnit * math.Pi

        val fieldRadiusMeters = antipodeDistanceMeters / math.Pi

        val collectorLatRad = CalculateLatitudeRad(collectorPosition._x)
        val collectorLonRad = CalculateLongitudeRad(collectorPosition._y)

        var waveforms = Vector.empty[Waveform]

        // Calculate field emitter waveforms:
        val emitters = _fieldEmitterPlant.GetFieldEmitters(field)
        emitters.foreach(emitter =>
        {
            emitter.Emit()

            val oscillators = _fieldOscillatorPlant.GetFieldOscillators(field, Some(emitter))
            oscillators.foreach(oscillator =>
            {
                val oscillatorPatch = GetOscillatorPatchOpt(field.GetKey, oscillator.GetPatchKey).getOrElse(
                    throw new FieldException(Cell.ErrorCodes.OscillatorPatchBroken))

                val waveformQualities = CalculateOscillatorWaveformQualities(
                    field,
                    oscillatorPatch,
                    emitterDistanceMeters = 1f,
                    emitterRelativeBearingUnit = 0f,
                    acoustic_aOpt,
                    lobeRangeOpt = None,
                    lobeRangeEnvelopeOpt = None,
                    lobeBearingEnvelopeOpt = None,
                    oscillatorPatch.GetWavesetId)

                waveforms ++= Vector(new Waveform(
                    waveformQualities,
                    new Waveform.Refs(
                        None,
                        emitter.GetKey,
                        oscillator.GetKey),
                        None))
            })
        })

        // Calculate subject emitter waveforms:
        val subjects = _subjectPlant.GetSubjects(field)
        subjects.foreach(subject =>
        {
            val emitterLatRad = CalculateLatitudeRad(subject.GetPosition._x)
            val emitterLonRad = CalculateLongitudeRad(subject.GetPosition._y)

            val emitterDistanceMeters = CalculateEmitterDistanceMeters(
                emitterLatRad,
                emitterLonRad,
                collectorLatRad,
                collectorLonRad,
                fieldRadiusMeters)

            val emitterAbsoluteBearingUnit = CalculateEmitterBearingUnit(
                emitterLatRad,
                emitterLonRad,
                collectorLatRad,
                collectorLonRad)
            val emitterRelativeBearingUnit = (emitterAbsoluteBearingUnit - collectorRotation._w + 1f) % 1f

            val emitters = _subjectEmitterPlant.GetSubjectEmitters(field, Some(subject))
            emitters.foreach(emitter =>
            {
                emitter.Emit()

                val emitterPosition = subject.GetPosition

                val oscillators = _subjectOscillatorPlant.GetSubjectOscillators(field, Some(emitter))
                oscillators.foreach(oscillator =>
                {
                    val oscillatorPatch = GetOscillatorPatchOpt(field.GetKey, oscillator.GetPatchKey).getOrElse(
                        throw new FieldException(Cell.ErrorCodes.OscillatorPatchBroken))

                    val waveformQualities = CalculateOscillatorWaveformQualities(
                        field,
                        oscillatorPatch,
                        emitterDistanceMeters,
                        emitterRelativeBearingUnit,
                        acoustic_aOpt,
                        lobeRangeOpt,
                        lobeRangeEnvelopeOpt,
                        lobeBearingEnvelopeOpt,
                        oscillatorPatch.GetWavesetId)

                    waveforms ++= Vector(new Waveform(
                        waveformQualities,
                        new Waveform.Refs(
                            Some(subject.GetKey),
                            emitter.GetKey,
                            oscillator.GetKey),
                        Some(new Waveform.Geometry(
                            collectorPosition,
                            emitterPosition,
                            emitterAbsoluteBearingUnit,
                            emitterRelativeBearingUnit.toDouble,
                            emitterDistanceMeters.toDouble))))
                })
            })
        })

        // Calculate probe emitter waveforms:
        val probes = _probePlant.GetProbes(field)
        probes.foreach(probe =>
        {
            val emitterLatRad = CalculateLatitudeRad(probe.GetPosition._x)
            val emitterLonRad = CalculateLongitudeRad(probe.GetPosition._y)

            val emitterDistanceMeters = CalculateEmitterDistanceMeters(
                emitterLatRad,
                emitterLonRad,
                collectorLatRad,
                collectorLonRad,
                fieldRadiusMeters)

            val emitterAbsoluteBearingUnit = CalculateEmitterBearingUnit(
                emitterLatRad,
                emitterLonRad,
                collectorLatRad,
                collectorLonRad)
            val emitterRelativeBearingUnit = (emitterAbsoluteBearingUnit - collectorRotation._w + 1f) % 1f

            val emitters = _probeEmitterPlant.GetProbeEmitters(field, Some(probe))
            emitters.foreach(emitter =>
            {
                emitter.Emit()

                val emitterPosition = probe.GetPosition

                val oscillators = _probeOscillatorPlant.GetProbeOscillators(field, Some(emitter))
                oscillators.foreach(oscillator =>
                {
                    val oscillatorPatch = GetOscillatorPatchOpt(field.GetKey, oscillator.GetPatchKey).getOrElse(
                        throw new FieldException(Cell.ErrorCodes.OscillatorPatchBroken))

                    val waveformQualities = CalculateOscillatorWaveformQualities(
                        field,
                        oscillatorPatch,
                        emitterDistanceMeters,
                        emitterRelativeBearingUnit,
                        acoustic_aOpt,
                        lobeRangeOpt,
                        lobeRangeEnvelopeOpt,
                        lobeBearingEnvelopeOpt,
                        oscillatorPatch.GetWavesetId)

                    waveforms ++= Vector(new Waveform(
                        waveformQualities,
                        new Waveform.Refs(
                            Some(probe.GetKey),
                            emitter.GetKey,
                            oscillator.GetKey),
                        Some(new Waveform.Geometry(
                            collectorPosition,
                            emitterPosition,
                            emitterAbsoluteBearingUnit,
                            emitterRelativeBearingUnit.toDouble,
                            emitterDistanceMeters.toDouble))))
                })
            })
        })

        // Return waveforms vector:
        waveforms
    }

    //
    // OscillatorPatch:
    //

    def GetOscillatorPatchOpt (
        fieldKey: Field.Key,
        key: OscillatorPatch.Key): Option[OscillatorPatch] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return oscillator patch:
        _trunkModel.GetOscillatorPatchOpt(field.GetTrunkKey, key)
    }

    //
    // SignalOutputs:
    //

    def GetSignalOutputOpt (
        fieldKey: Field.Key,
        key: SignalOutput.Key): Option[SignalOutput] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return signal output:
        _trunkModel.GetSignalOutputOpt(field.GetTrunkKey, key)
    }

    //
    // EmitterPatches:
    //

    def GetEmitterPatchOpt (
        fieldKey: Field.Key,
        key: EmitterPatch.Key): Option[EmitterPatch] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return emitter patch:
        _trunkModel.GetEmitterPatchOpt(field.GetTrunkKey, key)
    }

    def ReportEmitterPatches (
        fieldKey: Field.Key,
        keys: Vector[EmitterPatch.Key],
        sectionsOpt: Option[String] = None): Vector[JsObject] =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return emitter patch reports:
        _trunkModel.ReportEmitterPatches(
            field.GetTrunkKey,
            new EmitterPatch.Query(keys, sectionsOpt))
    }

    def UpdateEmitterPatches (
        fieldKey: Field.Key,
        updates: Vector[EmitterPatch.Update]): Unit =
    {
        // Get field (validate):
        val field = GetField(fieldKey)

        // Return emitter patch reports:
        _trunkModel.UpdateEmitterPatches(
            field.GetTrunkKey,
            updates)
    }


    def Initialize (): Unit =
    {
        val fieldConstructor = Field.Constructor(_tag = FieldModel.Glossary.kBang + FieldModel.Glossary.kEDefaultField)
        _defaultFieldOpt = Some(CreateFields(Vector[Field.Constructor](fieldConstructor)).head)
    }

    def SerializeToCQL (): String =
    {
        val fieldKey = new Field.Key("default")
        val field = _fieldPlant.GetFieldOpt(fieldKey).get

        def SerializeEmitters(builder: StringBuilder): Unit =
        {
            //            for (emitter <- _subjectEmitterPlant.GetSubjectEmitters(fieldKey))
            //            {
            //                builder.append(
            //                    "insert into " +
            //                        "taranos.emitters " +
            //                        "(" +
            //                        "field_key," +
            //                        "key," +
            //                        "keyset," +
            //                        "keyset_key," +
            //                        "bk," +
            //                        "fk" +
            //                        ") " +
            //                        "values " +
            //                        "(" +
            //                        "'" + fieldKey._1 + "'," +
            //                        "'" + emitter.Key._1 + "'," +
            //                        "''," +
            //                        "''," +
            //                        "''," +
            //                        "''" +
            //                        ");")
            //            }

            for (emitter <- _fieldEmitterPlant.GetFieldEmitters(field))
            {
                builder.append(
                    "insert into " +
                        "taranos.emitters " +
                        "(" +
                        "field_key," +
                        "key," +
                        "keyset," +
                        "keyset_key," +
                        "bk," +
                        "fk" +
                        ") " +
                        "values " +
                        "(" +
                        "'" + fieldKey._uniqueKey + "'," +
                        "'" + emitter.GetKey._uniqueKey + "'," +
                        "''," +
                        "''," +
                        "''," +
                        "''" +
                        ");")
            }
        }

        def SerializeSubjects (builder: StringBuilder): Unit =
        {
            for (subject <- _subjectPlant.GetSubjects(field))
            {
                builder.append(
                    "insert into " +
                        "taranos.subjects " +
                        "(" +
                        "field_key," +
                        "key," +
                        "keyset," +
                        "keyset_key," +
                        "state_posx," +
                        "state_posy," +
                        "state_posz," +
                        "state_rotw," +
                        "state_rotx," +
                        "state_roty," +
                        "state_rotz" +
                        ") " +
                        "values " +
                        "(" +
                        "'" + fieldKey._uniqueKey + "'," +
                        "'" + subject.GetKey._uniqueKey + "'," +
                        "''," +
                        "''," +
                        subject.GetPosition._x + "," +
                        subject.GetPosition._y + "," +
                        subject.GetPosition._z + "," +
                        subject.GetRotation._w + "," +
                        subject.GetRotation._x + "," +
                        subject.GetRotation._y + "," +
                        subject.GetRotation._z +
                        ");")
            }
        }

        def SerializeField (builder: StringBuilder): Unit =
        {
            // Insert subject key rows:
            for (subject <- _subjectPlant.GetSubjects(field))
            {
                builder.append(
                    "insert into " +
                        "taranos.fields " +
                        "(" +
                        "key," +
                        "keyset," +
                        "keyset_key" +
                        ") " +
                        "values " +
                        "(" +
                        "'" + fieldKey._uniqueKey + "'," +
                        "'bk'," +
                        "'" + subject.GetKey._uniqueKey + "'" +
                        ");")
            }

            // Insert state row:
            {
                builder.append(
                    "insert into " +
                        "taranos.fields " +
                        "(" +
                        "key," +
                        "keyset," +
                        "keyset_key," +
                        "state_a_c," +
                        "state_a_rho," +
                        "state_antipodeDistance" +
                        ") " +
                        "values " +
                        "(" +
                        "'" + fieldKey._uniqueKey + "'," +
                        "''," +
                        "''," +
                        field.GetAcoustic_c + "," +
                        field.GetAcoustic_r + "," +
                        field.GetAntipodeDistance +
                        ");")
            }
        }

        val builder: StringBuilder = new StringBuilder

        builder.append("begin batch ")

        SerializeEmitters(builder)

        SerializeSubjects(builder)

        SerializeField(builder)

        builder.append("apply batch;")

        builder.toString()
    }

    def Store (self: akka.actor.ActorRef): Unit =
    {
//        val fieldKey = new Field.Key("default")
//
//        var cql = "delete from taranos.emitters where field_key='" + fieldKey._uniqueKey + "';"
//        StorageSupervisor._storageSupervisor ! new StorageSupervisor.RequestMessages.ExecuteQuery(cql, self)
//
//        cql = "delete from taranos.subjects where field_key='" + fieldKey._uniqueKey + "';"
//        StorageSupervisor._storageSupervisor ! new StorageSupervisor.RequestMessages.ExecuteQuery(cql, self)
//
//        cql = "delete from taranos.fields where key='" + fieldKey._uniqueKey + "';"
//        StorageSupervisor._storageSupervisor ! new StorageSupervisor.RequestMessages.ExecuteQuery(cql, self)
//
//        cql = SerializeToCQL()
//        StorageSupervisor._storageSupervisor ! new StorageSupervisor.RequestMessages.ExecuteQuery(cql, self)
    }
}
