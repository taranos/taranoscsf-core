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
import org.taranos.mc.trunk.intraprocess.OscillatorPatch
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SubjectOscillator
{
    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'SubjectOscillator)
        extends Oscillator.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: FieldElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends Oscillator.Meta[SubjectOscillator.Key](
            new SubjectOscillator.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs (
        channelTag: String)
        extends Oscillator.Attrs(channelTag)

    class Refs (
        fieldKey: Field.Key,
        parentKey: Emitter.Key,
        oscillatorPatchKey: OscillatorPatch.Key)
        extends Oscillator.Refs(
            fieldKey,
            parentKey,
            oscillatorPatchKey)

    class State
        extends Oscillator.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _channelDef: JsObject)

    case class Destructor (
        _key: SubjectOscillator.Key,
        _scope: Symbol)

    case class Query (
        keys: Vector[SubjectOscillator.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[SubjectOscillator.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SubjectOscillator.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String])

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(FieldModel.Glossary.kNoneKeyBase)

    def DecodeCall (encoded: String): Call[SubjectOscillator.Key] =
    {
        val call = Json.parse(encoded)

        val commonCall = new CommonCallDecoder[SubjectOscillator.Key](
            call, Cell.ErrorCodes.SubjectOscillatorCallInvalid)

        Call(commonCall._elementKey, commonCall._macro)
    }

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor,
            Cell.ErrorCodes.SubjectOscillatorConstructorInvalid)

        val channelDef: JsObject =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kChannelDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => value

                case JsError(_) =>
                    throw FieldException(
                        Cell.ErrorCodes.SubjectOscillatorConstructorInvalid,
                        "missing channel definition element")
            }

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            channelDef)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SubjectOscillator.Key](
            destructor, Cell.ErrorCodes.SubjectOscillatorDestructorInvalid)

        Destructor(commonMeta._key, commonMeta._scope)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SubjectOscillator.Key](query)

        Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SubjectOscillator.Key](
            update, Cell.ErrorCodes.SubjectOscillatorUpdateInvalid)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }
}

class SubjectOscillator (
    meta: SubjectOscillator.Meta,
    attrs: SubjectOscillator.Attrs,
    refs: SubjectOscillator.Refs)
    (implicit protected val _fieldModel: FieldModel)
    extends Oscillator[SubjectOscillator.Key]
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

    def GetParentKey: Emitter.Key =
        _refs._parentKey

    def GetPatchKey: OscillatorPatch.Key =
        _refs._oscillatorPatchKey

    def UnbindPatch (): Unit =
        _refs._oscillatorPatchKey = OscillatorPatch.kNoneKey

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
        
        report
    }

    //
    // Macros:
    //

    def MacroSetLoudnessCeiling (ceiling: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetLoudnessCeiling(ceiling)
    }

    def MacroSetLoudnessPoles (polesPacked: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetLoudnessPoles(polesPacked)
    }

    def MacroSetLoudnessFloor (floor: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetLoudnessFloor(floor)
    }

    def MacroSetPeriodPoles (polesPacked: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetPeriodPoles(polesPacked)
    }

    def MacroSetPitchCeiling (ceiling: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetPitchCeiling(ceiling)
    }

    def MacroSetPitchPoles (polesPacked: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetPitchPoles(polesPacked)
    }

    def MacroSetPitchFloor (floor: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetPitchFloor(floor)
    }

    def MacroSetShapePoles (polesPacked: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetShapePoles(polesPacked)
    }

    def MacroSetTonePoles (polesPacked: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetTonePoles(polesPacked)
    }

    def MacroSetWavesetId (wavesetId: String): Unit =
    {
        val patch = _fieldModel.GetOscillatorPatchOpt(_refs._fieldKey, _refs._oscillatorPatchKey).getOrElse(
            throw FieldException(Cell.ErrorCodes.SubjectOscillatorPatchless))
        patch.MacroSetWavesetId(wavesetId)
    }
}
