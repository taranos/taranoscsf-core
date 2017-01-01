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

import org.taranos.mc.Cell
import org.taranos.mc.Common._
import org.taranos.mc.field.FieldElement._
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object Probe
{
    import scala.collection.mutable

    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Probe)
        extends Body.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: FieldElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends Body.Meta[Probe.Key](
            new Probe.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs
        extends Body.Attrs

    class Refs (
        fieldKey: Field.Key,
        val _collectorKeys: mutable.Set[ProbeCollector.Key] = mutable.Set.empty[ProbeCollector.Key],
        var _defaultCollectorKey: ProbeCollector.Key = ProbeCollector.kNoneKey)
        extends Body.Refs[ProbeEmitter.Key](fieldKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++=
                Json.obj(FieldModel.Glossary.kEDefaultProbeCollector -> FieldElement.EncodeKey(_defaultCollectorKey))

            if (!sections.HasChildReports)
            {
                // Add probe collector refs:
                report ++=
                    Json.obj(FieldModel.Glossary.kEProbeCollector -> _collectorKeys.map(FieldElement.EncodeKey))
            }

            report
        }
    }

    class State (
        position: Body.Position,
        rotation: Body.Rotation)
        extends Body.State(
            position,
            rotation)

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _position: Body.Position,
        _rotation: Body.Rotation,
        _acoustic_aOpt: Option[Real] = None,
        _squelchThresholdOpt: Option[Real] = None,
        _lobeRangeOpt: Option[Real] = None,
        _lobeRangePolesOpt: Option[String] = None,
        _lobeBearingPolesOpt: Option[String] = None)

    case class Destructor (
        _key: Probe.Key,
        _scope: Symbol)

    case class Query (
        keys: Vector[Probe.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[Probe.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: Probe.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _positionOpt: Option[Body.Position],
        _rotationOpt: Option[Body.Rotation])

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.ProbeEmitterConstructorInvalid)

        val position: Vector3 =
            (constructor \ FieldModel.Glossary.kPSState \ FieldModel.Glossary.kPStatePosition).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    new Vector3 (
                        value.head.toDouble,
                        value(1).toDouble,
                        0f)

                case JsError(errors) =>
                    new Body.Position
            }

        val rotation: Vector4 =
            (constructor \ FieldModel.Glossary.kPSState \ FieldModel.Glossary.kPStateRotation).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    new Vector4 (
                        value.head.toDouble,
                        0f,
                        0f,
                        0f)

                case JsError(errors) =>
                    new Body.Rotation
            }

        val acoustic_aOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsAcoustic_a).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val squelchThresholdOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsSquelchThreshold).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val lobeRangeOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeRange).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val lobeRangePolesOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeRangePoles).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val lobeBearingPolesOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeBearingPoles).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            position,
            rotation,
            acoustic_aOpt,
            squelchThresholdOpt,
            lobeRangeOpt,
            lobeRangePolesOpt,
            lobeBearingPolesOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[Probe.Key](
            destructor, Cell.ErrorCodes.ProbeDestructorInvalid)

        new Destructor(commonMeta._key, commonMeta._scope)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[Probe.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[Probe.Key](update, Cell.ErrorCodes.ProbeUpdateInvalid)

        val positionOpt: Option[Vector3] =
            (update \ FieldModel.Glossary.kPSState \ FieldModel.Glossary.kPStatePosition).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    Some(new Vector3(
                        value.head.toDouble,
                        value(1).toDouble,
                        0f))

                case JsError(errors) =>
                    None
            }

        val rotationOpt: Option[Vector4] =
            (update \ FieldModel.Glossary.kPSState \ FieldModel.Glossary.kPStateRotation).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    Some(new Vector4(
                        value.head.toDouble,
                        0f,
                        0f,
                        0f))

                case JsError(errors) =>
                    None
            }

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            positionOpt,
            rotationOpt)
    }
}

class Probe (
    meta: Probe.Meta,
    attrs: Probe.Attrs,
    refs: Probe.Refs,
    state: Probe.State)
    (implicit protected val _fieldModel: FieldModel)
    extends Body[Probe.Key]
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

    //
    // Refs:
    //
    protected
    val _refs = refs

    def BindCollector (key: ProbeCollector.Key) =
    {
        // First collector becomes the default collector:
        if (_refs._collectorKeys.isEmpty)
            _refs._defaultCollectorKey = key
        _refs._collectorKeys += key
    }

    def BindEmitter (emitterKey: Emitter.Key) = _refs._emitterKeys += emitterKey.asInstanceOf[ProbeEmitter.Key]

    def GetCollectorKeys = _refs._collectorKeys

    def GetDefaultCollectorKey = _refs._defaultCollectorKey

    def GetDefaultEmitterKey = Emitter.kNoneKey

    def GetEmitterKeys = _refs._emitterKeys.toSet

    def UnbindCollector (
        key: ProbeCollector.Key,
        isForcedDestroy: Boolean = false): Boolean =
    {
        var isRemoved = false
        // Only a non-default collector can be removed unless overruled:
        if (isForcedDestroy ||
            key != _refs._defaultCollectorKey)
        {
            _refs._collectorKeys -= key
            isRemoved = true
        }
        isRemoved
    }

    def UnbindEmitter (
        emitterKey: Emitter.Key,
        isForcedUnbind: Boolean = true) =
    {
        _refs._emitterKeys -= emitterKey.asInstanceOf[ProbeEmitter.Key]
        true
    }

    //
    // State:
    //
    protected
    val _state = state

    def GetPosition = _state._position

    def GetRotation = _state._rotation

    def SetPosition (position: Body.Position) = _state._position = position

    def SetRotation (rotation: Body.Rotation) = _state._rotation = rotation

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

        // Add state section:
        if (sections.HasStatePropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSState -> _state.Report(sections))

        // Add child reports:
        if (sections.HasChildReports)
        {
            // Add probe collector reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRProbeCollectors -> _fieldModel.ReportProbeCollectors(
                    GetFieldKey,
                    GetKey,
                    new ProbeCollector.Query(_refs._collectorKeys.toVector, sectionsOpt)))

            // Add probe emitter reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRProbeEmitters -> _fieldModel.ReportProbeEmitters(
                    GetFieldKey,
                    GetKey,
                    new ProbeEmitter.Query(_refs._emitterKeys.toVector, sectionsOpt)))
        }

        report
    }
}
