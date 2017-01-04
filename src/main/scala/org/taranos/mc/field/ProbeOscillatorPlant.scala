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


class ProbeOscillatorPlant
(implicit
    protected val _fieldModel: FieldModel,
    protected val _trunkModel: TrunkModel)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _probeOscillators = mutable.HashMap.empty[(Field.Key, ProbeOscillator.Key), ProbeOscillator]

    private
    def CreateOscillatorPatch (
        field: Field,
        emitter: ProbeEmitter,
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
        oscillator: ProbeOscillator) =
    {
        _trunkModel.DestroyOscillatorPatches(
            field.GetTrunkKey,
            Vector(new OscillatorPatch.Destructor(oscillator.GetPatchKey)))
    }

    def CreateProbeOscillator (
        field: Field,
        emitter: ProbeEmitter,
        constructor: ProbeOscillator.Constructor): ProbeOscillator =
    {
        // Create oscillator patch:
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
        val oscillator = new ProbeOscillator(
            new ProbeOscillator.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new ProbeOscillator.Attrs(
                channelTag),
            new ProbeOscillator.Refs(
                field.GetKey,
                emitter.GetKey,
                oscillatorPatch.GetKey))

        // 2: Add element to store:
        _probeOscillators += (field.GetKey, oscillator.GetKey) -> oscillator

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

    def DestroyProbeOscillator (
        field: Field,
        destructor: ProbeOscillator.Destructor,
        isForcedDestroy: Boolean): ProbeOscillator.Key =
    {
        _probeOscillators.get((field.GetKey, destructor._key)) match
        {
            case Some(oscillator) =>
                // 1: Unbind with parent:
                val emitter = _fieldModel.GetProbeEmitterOpt(
                    field.GetKey,
                    oscillator.GetParentKey.asInstanceOf[ProbeEmitter.Key]).get
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
                    _probeOscillators -= ((field.GetKey, oscillator.GetKey))
                }

            case None => throw new FieldException(Cell.ErrorCodes.ProbeOscillatorUnknown)
        }

        // Return probe oscillator key:
        destructor._key
    }

    def DestroyAllProbeOscillators (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        // Destroy each oscillator of {field, probe, probeEmitter}:
        _probeOscillators.filter(_._1._1 == fieldKey).foreach(oscillatorPair =>
        {
            val ((_, pairOscillatorKey), _) = oscillatorPair
            val oscillatorDestructor = new ProbeOscillator.Destructor(pairOscillatorKey, scope)
            DestroyProbeOscillator(field, oscillatorDestructor, isForcedDestroy = true)
        })
    }

    def GetProbeOscillatorOpt (
        field: Field,
        key: ProbeOscillator.Key,
        isRequired: Boolean = true): Option[ProbeOscillator] =
    {
        // Lookup probe oscillator:
        key match
        {
            case _: ProbeOscillator.Key =>
                val opt = _probeOscillators.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.ProbeOscillatorUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.ProbeOscillatorKeyInvalid)
        }
    }

    def GetProbeOscillators (
        field: Field,
        probeEmitterOpt: Option[ProbeEmitter] = None): Vector[ProbeOscillator] =
    {
        // Return probe oscillators vector:
        val fieldKey = field.GetKey
        if (probeEmitterOpt.isDefined)
        {
            val probeEmitterKey = probeEmitterOpt.get.GetKey
            _probeOscillators.filter(pair =>
            {
                val ((pairFieldKey, pairOscillatorKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetProbeOscillatorOpt(fieldKey, pairOscillatorKey).get.GetParentKey == probeEmitterKey
            }).values.toVector
        }
        else
            _probeOscillators.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).values.toVector
    }

    def GetProbeOscillatorKeys (
        field: Field,
        probeEmitterOpt: Option[ProbeEmitter] = None): Vector[ProbeOscillator.Key] =
    {
        // Return probe oscillator keys vector:
        val fieldKey = field.GetKey
        if (probeEmitterOpt.isDefined)
        {
            val probeEmitterKey = probeEmitterOpt.get.GetKey
            _probeOscillators.filter(pair =>
            {
                val ((pairFieldKey, pairOscillatorKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetProbeOscillatorOpt(fieldKey, pairOscillatorKey).get.GetParentKey == probeEmitterKey
            }).keys.map(pair => pair._2).toVector
        }
        else
            _probeOscillators.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).keys.map(pair => pair._2).toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int = _probeOscillators.count(_._1._1 == fieldKey)
}
