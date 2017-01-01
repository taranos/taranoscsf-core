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
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.field.FieldElement._
import org.taranos.mc.trunk.intraprocess.{SignalModulator, TrunkElement, TrunkModel}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object Subject
{
    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Subject)
        extends Body.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: FieldElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String] = None,
        nameOpt: Option[String] = None,
        descriptionOpt: Option[String] = None)
        extends Body.Meta[Subject.Key](
            new Subject.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs
        extends Body.Attrs

    class Refs (
        fieldKey: Field.Key)
        extends Body.Refs[SubjectEmitter.Key](
            fieldKey)

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
        _patchDefOpt: Option[JsObject] = None,
        _modulatorKeyOpt: Option[SignalModulator.Key] = None)

    case class Destructor (
        _key: Subject.Key,
        _scope: Symbol)

    case class Query (
        keys: Vector[Subject.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[Subject.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: Subject.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _positionOpt: Option[Body.Position],
        _rotationOpt: Option[Body.Rotation])

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SubjectConstructorInvalid)

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

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            position,
            rotation,
            patchDefOpt,
            modulatorKeyOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[Subject.Key](
            destructor, Cell.ErrorCodes.SubjectDestructorInvalid)

        new Destructor(commonMeta._key, commonMeta._scope)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[Subject.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[Subject.Key](update, Cell.ErrorCodes.SubjectUpdateInvalid)

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

class Subject (
    meta: Subject.Meta,
    refs: Subject.Refs,
    state: Subject.State)
    (implicit protected val _fieldModel: FieldModel)
    extends Body[Subject.Key]
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
    val _attrs = null

    //
    // Refs:
    //
    protected
    val _refs = refs

    def BindEmitter (emitterKey: Emitter.Key) =
    {
        // First emitter becomes the default emitter:
        if (_refs._emitterKeys.isEmpty)
            _refs._defaultEmitterKey = emitterKey
        _refs._emitterKeys += emitterKey.asInstanceOf[SubjectEmitter.Key]
    }

    def GetDefaultEmitterKey = _refs._defaultEmitterKey

    def GetEmitterKeys = _refs._emitterKeys.toSet

    def UnbindEmitter (
        emitterKey: Emitter.Key,
        isForcedUnbind: Boolean): Boolean =
    {
        var isUnbound = false
        // Only a non-default emitter can be unbound unless forced:
        if (isForcedUnbind ||
            emitterKey != _refs._defaultEmitterKey)
        {
            _refs._emitterKeys -= emitterKey.asInstanceOf[SubjectEmitter.Key]
            isUnbound = true
        }
        isUnbound
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
    def Report (sectionsOpt: Option[String]): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        // Add meta section:
        if (sections.HasMetaPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSMeta -> _meta.Report(sections))

        // Add refs section:
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSRefs -> _refs.Report(sections))

        // Add state section:
        if (sections.HasStatePropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSState -> _state.Report(sections))

        // Add child reports:
        if (sections.HasChildReports)
        {
            // Add subject emitter reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRSubjectEmitters -> _fieldModel.ReportSubjectEmitters(
                    GetFieldKey,
                    GetKey,
                    new SubjectEmitter.Query(_refs._emitterKeys.toVector, sectionsOpt)))
        }

        report
    }
}
