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

package org.taranos.mc.field

import org.taranos.mc.Cell
import org.taranos.mc.Common.{Real, ReportSectionsParser, Reportable}
import org.taranos.mc.field.FieldElement._
import org.taranos.mc.trunk.intraprocess.{SignalModulator, Trunk, TrunkElement, TrunkModel}
import play.api.libs.json._


object Field
{
    import scala.collection.mutable

    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Field)
        extends FieldElement.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: FieldElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String] = None,
        nameOpt: Option[String] = None,
        descriptionOpt: Option[String] = None)
        extends FieldElement.Meta[Field.Key](
            new Field.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs (
        // Wavefield geometry code:
        var _geometry: String = Field.Defaults.kGeometry,

        // Antipode distance (m):
        var _antipodeDistance: Real = Field.Defaults.kAntipodeDistance,

        // Speed of wavefield pressure wave (m/s):
        var _acoustic_c: Real = Field.Defaults.kAcoustic_c,

        // Density of wavefield pressure medium (kg/m^3):
        var _acoustic_r: Real = Field.Defaults.kAcoustic_r)
        extends FieldElement.Attrs
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            super.Report(sections) ++
                Json.obj(
                    FieldModel.Glossary.kPAttrsFieldGeometry -> _geometry.toString,
                    FieldModel.Glossary.kPAttrsAntipodeDistance -> Reportable.ReportReal3(_antipodeDistance),
                    FieldModel.Glossary.kPAttrsAcoustic_c -> Reportable.ReportReal3(_acoustic_c),
                    FieldModel.Glossary.kPAttrsAcoustic_r -> Reportable.ReportReal3(_acoustic_r))
        }
    }

    class Refs (
        val _trunkKey: Trunk.Key,
        val _emitterKeys: mutable.Set[FieldEmitter.Key] = mutable.Set.empty[FieldEmitter.Key],
        var _defaultEmitterKey: Emitter.Key = Emitter.kNoneKey,
        val _subjectKeys: mutable.Set[Subject.Key] = mutable.Set.empty[Subject.Key],
        val _probeKeys: mutable.Set[Probe.Key] = mutable.Set.empty[Probe.Key])
        extends FieldElement.Refs(Field.kNoneKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            // Add trunk ref:
            report ++=
                Json.obj(TrunkModel.Glossary.kETrunk -> TrunkElement.EncodeKey(_trunkKey))

            // Add default emitter ref:
            val emitterName = _defaultEmitterKey._symbol match
            {
                case 'FieldEmitter => FieldModel.Glossary.kEDefaultFieldEmitter

                case _ => ""
            }
            if (emitterName.nonEmpty)
                report ++=
                    Json.obj(emitterName -> FieldElement.EncodeKey(_defaultEmitterKey))

            // Add child refs:
            if (!sections.HasChildReports)
            {
                // Add emitter refs:
                report ++=
                    Json.obj(FieldModel.Glossary.kEFieldEmitter -> _emitterKeys.map(FieldElement.EncodeKey))

                // Add subject refs:
                report ++=
                    Json.obj(FieldModel.Glossary.kESubject -> _subjectKeys.map(FieldElement.EncodeKey))

                // Add probe refs:
                report ++=
                    Json.obj(FieldModel.Glossary.kEProbe -> _probeKeys.map(FieldElement.EncodeKey))
            }

            report
        }
    }

    class State
        extends FieldElement.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _trunkKeyOpt: Option[Trunk.Key] = None,
        _geometryOpt: Option[String] = None,
        _antipodeDistanceOpt: Option[Real] = None,
        _acoustic_cOpt: Option[Real] = None,
        _acoustic_rOpt: Option[Real] = None,
        _patchDefOpt: Option[JsObject] = None,
        _modulatorKeyOpt: Option[SignalModulator.Key] = None)

    case class Destructor (
        _key: Field.Key,
        _scope: Symbol)

    case class Query (
        keys: Vector[Field.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[Field.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: Field.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _subjectUpdatesOpt: Option[Seq[Subject.Update]],
        _probeUpdatesOpt: Option[Seq[Probe.Update]])

    object Defaults
    {
        // Wavefield geometry code:
        val kGeometry: String = FieldModel.Glossary.kGeometrySpherical

        // Antipode distance (m):
        val kAntipodeDistance: Real = 1000f

        // Speed of wavefield pressure wave (m/s):
        val kAcoustic_c: Real = 343f

        // Density of wavefield pressure medium (kg/m^3):
        val kAcoustic_r: Real = 1.204f
    }

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(FieldModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.FieldConstructorInvalid)

        val acoustic_cOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsAcoustic_c).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val acoustic_rOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsAcoustic_r).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val antipodeDistanceOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \
                FieldModel.Glossary.kPAttrsAntipodeDistance).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val geometryOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSAttrs \
                FieldModel.Glossary.kPAttrsFieldGeometry).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val patchDefOpt: Option[JsObject] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kEmitterPatchDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val modulatorKeyOpt: Option[SignalModulator.Key] =
            (constructor \ FieldModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalModulator).validate[String] match
            {
                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[SignalModulator.Key](value))

                case JsError(errors) => None
            }

        val trunkKeyOpt: Option[Trunk.Key] =
            (constructor \ FieldModel.Glossary.kPSRefs \ TrunkModel.Glossary.kETrunk).validate[String] match
            {
                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[Trunk.Key](value))

                case JsError(errors) => None
            }

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            trunkKeyOpt,
            geometryOpt,
            antipodeDistanceOpt,
            acoustic_cOpt,
            acoustic_rOpt,
            patchDefOpt,
            modulatorKeyOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[Field.Key](
            destructor, Cell.ErrorCodes.FieldDestructorInvalid)

        new Destructor(commonMeta._key, commonMeta._scope)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[Field.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        import scala.collection.mutable.ArrayBuffer

        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[Field.Key](
            update, Cell.ErrorCodes.FieldUpdateInvalid)

        val probeUpdatesOpt: Option[Seq[Probe.Update]] =
            (update \ FieldModel.Glossary.kEProbe).validate[JsArray] match
            {
                case JsSuccess(buArray, _) =>
                    var probeUpdates = new ArrayBuffer[Probe.Update]
                    for (buValue <- buArray.value)
                    {
                        buValue.validate[JsObject] match
                        {
                            case JsSuccess(buObject, _) =>
                                val probeUpdateEncoded = Json.stringify(buObject)
                                val probeUpdate = Probe.DecodeUpdate(probeUpdateEncoded)
                                probeUpdates += probeUpdate

                            case JsError(errors) => None
                        }
                    }
                    if (probeUpdates.nonEmpty) Some(probeUpdates) else None

                case JsError(errors) => None
            }

        val subjectUpdatesOpt: Option[Seq[Subject.Update]] =
            (update \ FieldModel.Glossary.kESubject).validate[JsArray] match
            {
                case JsSuccess(buArray, _) =>
                    var subjectUpdates = new ArrayBuffer[Subject.Update]
                    for (buValue <- buArray.value)
                    {
                        buValue.validate[JsObject] match
                        {
                            case JsSuccess(buObject, _) =>
                                val subjectUpdateEncoded = Json.stringify(buObject)
                                val subjectUpdate = Subject.DecodeUpdate(subjectUpdateEncoded)
                                subjectUpdates += subjectUpdate

                            case JsError(errors) => None
                        }
                    }
                    if (subjectUpdates.nonEmpty) Some(subjectUpdates) else None

                case JsError(errors) => None
            }

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            subjectUpdatesOpt,
            probeUpdatesOpt)
    }
}

class Field (
    meta: Field.Meta,
    attrs: Field.Attrs,
    refs: Field.Refs)
    (implicit protected val _fieldModel: FieldModel)
    extends FieldElement[Field.Key]
{
    //
    // Meta:
    //
    protected
    val _meta = meta

    //
    // Attrs:
    //
    protected
    val _attrs = attrs

    def GetGeometry: String = _attrs._geometry

    def GetAntipodeDistance: Real = _attrs._antipodeDistance

    def GetAcoustic_c: Real = _attrs._acoustic_c

    def GetAcoustic_r: Real = _attrs._acoustic_r

    //
    // Refs:
    //
    protected
    val _refs = refs

    def BindEmitter (emitterKey: FieldEmitter.Key) =
    {
        // First emitter becomes the default emitter:
        if (_refs._emitterKeys.isEmpty)
            _refs._defaultEmitterKey = emitterKey
        _refs._emitterKeys += emitterKey.asInstanceOf[FieldEmitter.Key]
    }

    def BindProbe (probeKey: Probe.Key) = _refs._probeKeys += probeKey

    def BindSubject (subjectKey: Subject.Key) = _refs._subjectKeys += subjectKey

    def GetEmitterKeys = _refs._emitterKeys.toSet

    def GetProbeKeys = _refs._probeKeys.toSet

    def GetSubjectKeys  = _refs._subjectKeys.toSet

    def GetTrunkKey = _refs._trunkKey

    def UnbindEmitter (
        emitterKey: Emitter.Key,
        isForcedUnbind: Boolean): Boolean =
    {
        var isUnbound = false
        // Only a non-default emitter can be unbound unless forced:
        if (isForcedUnbind ||
            emitterKey != _refs._defaultEmitterKey)
        {
            _refs._emitterKeys -= emitterKey.asInstanceOf[FieldEmitter.Key]
            isUnbound = true
        }
        isUnbound
    }

    def UnbindProbe (probeKey: Probe.Key) = _refs._probeKeys -= probeKey

    def UnbindSubject (subjectKey: Subject.Key) = _refs._subjectKeys -= subjectKey

    //
    // State:
    //
    protected
    val _state = null

    //
    // Reports:
    //
    def Report (sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        // Add meta section:
        if (sections.HasMetaPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSMeta -> _meta.Report(sections))

        // Add attrs section:
        if (sections.HasAttrsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSAttrs -> _attrs.Report(sections))

        // Add refs section:
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSRefs -> _refs.Report(sections))

        // Add child reports:
        if (sections.HasChildReports)
        {
            // Add emitter reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRFieldEmitters -> _fieldModel.ReportFieldEmitters(
                    GetKey,
                    new FieldEmitter.Query(_refs._emitterKeys.toVector, sectionsOpt)))

            // Add subject reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRSubjects -> _fieldModel.ReportSubjects(
                    GetKey,
                    new Subject.Query(_refs._subjectKeys.toVector, sectionsOpt)))

            // Add probe reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRProbes -> _fieldModel.ReportProbes(
                    GetKey,
                    new Probe.Query(_refs._probeKeys.toVector, sectionsOpt)))
        }

        report
    }
}
