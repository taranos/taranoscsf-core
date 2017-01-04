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
import org.taranos.mc.trunk.intraprocess._
import play.api.libs.json.{JsError, JsObject, JsSuccess}


class FieldOscillatorPlant
(implicit
    protected val _fieldModel: FieldModel,
    protected val _trunkModel: TrunkModel)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _fieldOscillators = mutable.HashMap.empty[(Field.Key, FieldOscillator.Key), FieldOscillator]

    private
    def CreateOscillatorPatch (
        field: Field,
        emitter: FieldEmitter,
        tag: String,
        channelTag: String,
        oscillatorPatchDef: JsObject): OscillatorPatch =
    {
        val trunkKey = field.GetTrunkKey

        _trunkModel.CreateOscillatorPatches(
            trunkKey,
            Vector(
                new OscillatorPatch.Constructor(
                    _tag = tag,
                    _fieldKey = field.GetKey,
                    _emitterPatchKey = emitter.GetPatchKey,
                    _channelTag = channelTag,
                    _tappableKeyOpt = None,
                    _patchDef = oscillatorPatchDef))).head
    }

    private
    def DestroyOscillatorPatch (
        field: Field,
        oscillator: FieldOscillator) =
    {
        _trunkModel.DestroyOscillatorPatches(
            field.GetTrunkKey,
            Vector(new OscillatorPatch.Destructor(oscillator.GetPatchKey)))
    }

    def CreateFieldOscillator (
        field: Field,
        emitter: FieldEmitter,
        constructor: FieldOscillator.Constructor): FieldOscillator =
    {
        // Create oscillator patch (child):
        val channelTag = (constructor._channelDef \ FieldModel.Glossary.kChannelTag).validate[String] match
        {
            case JsSuccess(value, _) => value

            case JsError(errors) =>
                throw new FieldException(Cell.ErrorCodes.ChannelDefinitionInvalid)
        }
        val oscillatorPatchDef = (constructor._channelDef \
            FieldModel.Glossary.kOscillatorPatchDef).validate[JsObject] match
        {
            case JsSuccess(value, _) => value

            case JsError(errors) =>
                throw new FieldException(Cell.ErrorCodes.ChannelDefinitionInvalid)
        }
        val oscillatorPatch = CreateOscillatorPatch(
            field,
            emitter,
            constructor._tag + FieldModel.Glossary.kTagSeparator + TrunkModel.Glossary.kEOscillatorPatch,
            channelTag,
            oscillatorPatchDef)

        // 1: Create element:
        val oscillator = new FieldOscillator(
            new FieldOscillator.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new FieldOscillator.Attrs(
                channelTag),
            new FieldOscillator.Refs(
                field.GetKey,
                emitter.GetKey,
                oscillatorPatch.GetKey))

        // 2: Add element to store:
        _fieldOscillators += (field.GetKey, oscillator.GetKey) -> oscillator

        // 3: Bind with/create children:
        oscillatorPatch.BindOscillator(oscillator.GetKey)

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        emitter.BindOscillator(oscillator.GetKey)

        // 6: Activate element:
        // N/A

        // Return element:
        oscillator
    }

    def DestroyFieldOscillator (
        field: Field,
        destructor: FieldOscillator.Destructor,
        isForcedDestroy: Boolean): FieldOscillator.Key =
    {
        _fieldOscillators.get((field.GetKey, destructor._key)) match
        {
            case Some(oscillator) =>
                // 1: Unbind with parent:
                val emitter = _fieldModel.GetFieldEmitterOpt(
                    field.GetKey,
                    oscillator.GetParentKey.asInstanceOf[FieldEmitter.Key]).get
                val isUnbound = emitter.UnbindOscillator(oscillator.GetKey, isForcedDestroy)
                if (isUnbound)
                {
                    // 2: Deactivate element:
                    // N/A

                    // 3: Unbind with peers:
                    // N/A

                    // 4: Unbind with/destroy children:
                    if (destructor._scope == 'ScopeDeep)
                        DestroyOscillatorPatch(field, oscillator)

                    // 5: Remove element from store:
                    _fieldOscillators -= ((field.GetKey, oscillator.GetKey))
                }

            case None => throw new FieldException(Cell.ErrorCodes.FieldOscillatorUnknown)
        }

        // Return field oscillator key:
        destructor._key
    }

    def DestroyAllFieldOscillators (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        // Destroy each oscillator of {field, field, fieldEmitter}:
        _fieldOscillators.filter(_._1._1 == fieldKey).foreach(oscillatorPair =>
        {
            val ((_, pairOscillatorKey), _) = oscillatorPair
            val oscillatorDestructor = new FieldOscillator.Destructor(pairOscillatorKey, scope)
            DestroyFieldOscillator(field, oscillatorDestructor, isForcedDestroy = true)
        })
    }

    def GetFieldOscillatorOpt (
        field: Field,
        key: FieldOscillator.Key,
        isRequired: Boolean = true): Option[FieldOscillator] =
    {
        // Lookup field oscillator:
        key match
        {
            case _: FieldOscillator.Key =>
                val opt = _fieldOscillators.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.FieldOscillatorUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.FieldOscillatorKeyInvalid)
        }
    }

    def GetFieldOscillators (
        field: Field,
        fieldEmitterOpt: Option[FieldEmitter] = None): Vector[FieldOscillator] =
    {
        // Return field oscillators vector:
        val fieldKey = field.GetKey
        if (fieldEmitterOpt.isDefined)
        {
            val emitterKey = fieldEmitterOpt.get.GetKey
            _fieldOscillators.filter(pair =>
            {
                val ((pairFieldKey, pairOscillatorKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetFieldOscillatorOpt(fieldKey, pairOscillatorKey).get.GetParentKey == emitterKey
            }).values.toVector
        }
        else
            _fieldOscillators.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).values.toVector
    }

    def GetFieldOscillatorKeys (
        field: Field,
        fieldEmitterOpt: Option[FieldEmitter] = None): Vector[FieldOscillator.Key] =
    {
        // Return field oscillator keys vector:
        val fieldKey = field.GetKey
        if (fieldEmitterOpt.isDefined)
        {
            val emitterKey = fieldEmitterOpt.get.GetKey
            _fieldOscillators.filter(pair =>
            {
                val ((pairFieldKey, pairOscillatorKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetFieldOscillatorOpt(fieldKey, pairOscillatorKey).get.GetParentKey == emitterKey
            }).keys.map(pair => pair._2).toVector
        }
        else
            _fieldOscillators.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).keys.map(pair => pair._2).toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int = _fieldOscillators.count(_._1._1 == fieldKey)
}
