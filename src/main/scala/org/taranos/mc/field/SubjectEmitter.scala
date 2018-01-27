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
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.field.FieldElement._
import org.taranos.mc.trunk.intraprocess._
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SubjectEmitter
{
    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'SubjectEmitter)
        extends Emitter.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: FieldElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends Emitter.Meta[SubjectEmitter.Key](
            new SubjectEmitter.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs
        extends Emitter.Attrs

    class Refs (
        fieldKey: Field.Key,
        subjectKey: Subject.Key,
        emitterPatchKey: EmitterPatch.Key = EmitterPatch.kNoneKey)
        extends Emitter.Refs[FieldElement.Key, SubjectOscillator.Key](
            fieldKey,
            subjectKey,
            emitterPatchKey)

    class State
        extends Emitter.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _patchDefOpt: Option[JsObject] = None,
        _modulatorKeyOpt: Option[SignalModulator.Key] = None)

    case class Destructor (
        _key: SubjectEmitter.Key,
        _scope: Symbol)

    case class Query (
        keys: Vector[SubjectEmitter.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[SubjectEmitter.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SubjectEmitter.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String])

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(FieldModel.Glossary.kNoneKeyBase)

    def DecodeCall (encoded: String): Call[SubjectEmitter.Key] =
    {
        val call = Json.parse(encoded)

        val commonCall = new CommonCallDecoder[SubjectEmitter.Key](
            call, Cell.ErrorCodes.SubjectEmitterCallInvalid)

        new Call(commonCall._elementKey, commonCall._macro)
    }

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SubjectEmitterConstructorInvalid)

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
            patchDefOpt,
            modulatorKeyOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SubjectEmitter.Key](
            destructor, Cell.ErrorCodes.SubjectEmitterDestructorInvalid)

        new Destructor(commonMeta._key, commonMeta._scope)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SubjectEmitter.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SubjectEmitter.Key](
            update, Cell.ErrorCodes.SubjectEmitterUpdateInvalid)

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }
}

class SubjectEmitter (
    meta: SubjectEmitter.Meta,
    refs: SubjectEmitter.Refs)
    (implicit
        protected val _fieldModel: FieldModel,
        protected val _trunkModel: TrunkModel)
    extends Emitter[SubjectEmitter.Key]
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

    def BindOscillator (oscillatorKey: Oscillator.Key) =
    {
        // First oscillator becomes the default oscillator:
        if (_refs._oscillatorKeys.isEmpty)
            _refs._defaultOscillatorKey = oscillatorKey
        _refs._oscillatorKeys += oscillatorKey.asInstanceOf[SubjectOscillator.Key]
    }

    def BindPatch (emitterPatchKey: EmitterPatch.Key) = _refs._emitterPatchKey = emitterPatchKey

    def GetDefaultOscillatorKey = _refs._defaultOscillatorKey

    def GetOscillatorKeys = _refs._oscillatorKeys.toSet

    def GetParentKey = _refs._parentKey

    def GetPatchKey = _refs._emitterPatchKey

    def MacroCreateChannel (
        channelTag: String,
        envelopeDefOpt: Option[JsObject],
        patchDefOpt: Option[JsObject]) =
    {
        val patch = _fieldModel.GetEmitterPatchOpt(_refs._fieldKey, _refs._emitterPatchKey).getOrElse(
            throw new FieldException(Cell.ErrorCodes.SubjectEmitterPatchless))
        patch.MacroCreateChannel(
            channelTag,
            envelopeDefOpt,
            patchDefOpt)
    }

    def MacroDestroyChannel (channelTag: String) =
    {
        val patch = _fieldModel.GetEmitterPatchOpt(_refs._fieldKey, _refs._emitterPatchKey).getOrElse(
            throw new FieldException(Cell.ErrorCodes.SubjectEmitterPatchless))
        patch.MacroDestroyChannel(channelTag)
    }

    def MacroSetChannelCeiling (channelTag: String, ceiling: String) =
    {
        val patch = _fieldModel.GetEmitterPatchOpt(_refs._fieldKey, _refs._emitterPatchKey).getOrElse(
            throw new FieldException(Cell.ErrorCodes.SubjectEmitterPatchless))
        patch.MacroSetChannelCeiling(channelTag, ceiling)
    }

    def MacroSetChannelPoles (channelTag: String, polesPacked: String) =
    {
        val patch = _fieldModel.GetEmitterPatchOpt(_refs._fieldKey, _refs._emitterPatchKey).getOrElse(
            throw new FieldException(Cell.ErrorCodes.SubjectEmitterPatchless))
        patch.MacroSetChannelPoles(channelTag, polesPacked)
    }

    def MacroSetChannelFloor (channelTag: String, floor: String) =
    {
        val patch = _fieldModel.GetEmitterPatchOpt(_refs._fieldKey, _refs._emitterPatchKey).getOrElse(
            throw new FieldException(Cell.ErrorCodes.SubjectEmitterPatchless))
        patch.MacroSetChannelFloor(channelTag, floor)
    }

    def UnbindOscillator (
        oscillatorKey: Oscillator.Key,
        isForcedUnbind: Boolean): Boolean =
    {
        var isUnbound = false
        // Only a non-default oscillator can be unbound unless forced:
        if (isForcedUnbind ||
            oscillatorKey != _refs._defaultOscillatorKey)
        {
            _refs._oscillatorKeys -= oscillatorKey.asInstanceOf[SubjectOscillator.Key]
            isUnbound = true
        }
        isUnbound
    }

    def UnbindPatch () = _refs._emitterPatchKey = EmitterPatch.kNoneKey

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
        Emit()

        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        // Add meta section:
        if (sections.HasMetaPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSMeta -> _meta.Report(sections))

        // Add refs section:
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSRefs -> _refs.Report(sections))

        // Add child reports:
        if (sections.HasChildReports)
        {
            // Add emitter patch report:
            report ++=
                Json.obj(TrunkModel.Glossary.kREmitterPatches -> _fieldModel.ReportEmitterPatches(
                    GetFieldKey,
                    Vector(_refs._emitterPatchKey),
                    sectionsOpt).head)

            // Add oscillator reports:
            report ++=
                Json.obj(FieldModel.Glossary.kRSubjectOscillators -> _fieldModel.ReportSubjectOscillators(
                    GetFieldKey,
                    GetKey,
                    SubjectOscillator.Query(_refs._oscillatorKeys.toVector, sectionsOpt)))
        }

        report
    }

    def Emit (): Unit =
    {
        val field = _fieldModel.GetField(GetFieldKey)

        _trunkModel.GetEmitterPatchOpt(field.GetTrunkKey, _refs._emitterPatchKey) match
        {
            case Some(patch) => patch.Trigger()

            case None => throw new FieldException(Cell.ErrorCodes.SubjectEmitterPatchless)
        }
    }
}
