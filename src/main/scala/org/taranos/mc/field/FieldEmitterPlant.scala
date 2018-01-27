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

import org.taranos.mc.trunk.intraprocess._
import org.taranos.mc.{Cell, CellLogger}
import play.api.libs.json.{JsObject, Json}


class FieldEmitterPlant
    (implicit
        protected val _fieldModel: FieldModel,
        protected val _trunkModel: TrunkModel,
        protected val _logger: CellLogger)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _fieldEmitters = mutable.HashMap.empty[(Field.Key, FieldEmitter.Key), FieldEmitter]

    private
    def CreateEmitterPatch (
        field: Field,
        emitterKey: Emitter.Key,
        tag: String,
        modulatorKey: SignalModulator.Key,
        patchDef: JsObject): EmitterPatch =
    {
        _trunkModel.CreateEmitterPatches(
            field.GetTrunkKey,
            Vector(
                new EmitterPatch.Constructor(
                    _tag = tag,
                    _fieldKey = field.GetKey,
                    _emitterKey = emitterKey,
                    _modulatableKey = modulatorKey,
                    _tappableKeyOpt = None,
                    _patchDef = patchDef))).head
    }

    private
    def DestroyEmitterPatch (
        field: Field,
        emitter: FieldEmitter) =
    {
        _trunkModel.DestroyEmitterPatches(
            field.GetTrunkKey,
            Vector(new EmitterPatch.Destructor(emitter.GetPatchKey)))
    }

    def DeployEmitterPatch (
        field: Field,
        emitter: FieldEmitter,
        patchDefOpt: Option[JsObject] = None): Unit =
    {
        val trunkKey: Trunk.Key = field.GetTrunkKey

        // Get emitter's patch:
        val emitterPatch =
            _trunkModel.GetEmitterPatchOpt(trunkKey, emitter.GetPatchKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.EmitterPatchUnknown))

        // Tell patch to import new patch definition:
        _logger.LogDebug(s"DEP: importing emitter patch definition: $patchDefOpt")
        emitterPatch.ImportEmitterPatchDef(patchDefOpt)

        // Destroy current oscillator elements (children):
        UndeployEmitterPatch(field, emitter)

        // Create new oscillator elements (children):
        emitterPatch.GetChannelsMap.foreach(pair =>
        {
            val (channelTag, channel) = pair

            val channelDef = Json.obj(
                FieldModel.Glossary.kChannelTag -> channelTag,
                FieldModel.Glossary.kOscillatorPatchDef -> channel._oscillatorPatchDef)
            val oscillatorConstructor = new FieldOscillator.Constructor(
                _tag = emitter.GetTag + FieldModel.Glossary.kTagSeparator + channelTag,
                _channelDef = channelDef)
            _logger.LogDebug(s"DEP: creating FieldOscillator: $oscillatorConstructor")
            _fieldModel.CreateFieldOscillators(
                field.GetKey,
                emitter.GetKey,
                Vector(oscillatorConstructor))
        })
    }

    private
    def UndeployEmitterPatch (
        field: Field,
        emitter: FieldEmitter) =
    {
        val trunkKey: Trunk.Key = field.GetTrunkKey

        // Get emitter's patch:
        val emitterPatch =
            _trunkModel.GetEmitterPatchOpt(trunkKey, emitter.GetPatchKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.EmitterPatchUnknown))

        emitterPatch.GetOscillatorPatchKeysMap.foreach(pair =>
        {
            val oscillatorPatchKey = pair._2

            val oscillatorPatch =
                _trunkModel.GetOscillatorPatchOpt(trunkKey, oscillatorPatchKey).getOrElse(
                    throw new FieldException(Cell.ErrorCodes.OscillatorPatchUnknown))
            val oscillatorKey = oscillatorPatch.GetOscillatorKey

            val oscillatorDestructor = new FieldOscillator.Destructor(
                _key = oscillatorKey.asInstanceOf[FieldOscillator.Key],
                _scope = 'ScopeDeep)
            _logger.LogDebug(s"DEP: destroying FieldOscillator: $oscillatorDestructor")
            _fieldModel.DestroyFieldOscillators(
                fieldKey = field.GetKey,
                destructors = Vector(oscillatorDestructor),
                isForcedDestroy = true)
        })
        emitterPatch.ClearOscillatorPatchKeysMap()
    }

    def CreateFieldEmitter(
        field: Field,
        constructor: FieldEmitter.Constructor): FieldEmitter =
    {
        val trunkKey = field.GetTrunkKey

        // 1: Create element:
        val emitter = new FieldEmitter(
            new FieldEmitter.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new FieldEmitter.Refs(
                field.GetKey))

        // 2: Add element to store:
        _fieldEmitters += (field.GetKey, emitter.GetKey) -> emitter

        // 3: Bind with/create children:
        {
            // Get modulator key, creating a new input if necessary:
            val modulatorKey: SignalModulator.Key = constructor._modulatorKeyOpt match
            {
                case Some(key) => key

                case None =>
                    // Create a new port:
                    val (_, debangedAlias) = FieldElement.DebangTag(constructor._tag)
                    val port = _trunkModel.CreateSignalPorts(
                        trunkKey,
                        SignalInterface.kAnyKey,
                        Vector(
                            new SignalPort.Constructor(
                                _tag = constructor._tag + FieldModel.Glossary.kTagSeparator +
                                    TrunkModel.Glossary.kESignalPort,
                                _aliasOpt = Some(debangedAlias),
                                _mode = Signal.ModeEnum.Continuous))).head

                    // Create input using new port's tap:
                    val input = _trunkModel.CreateSignalInputs(
                        trunkKey,
                        Vector(
                            new SignalInput.Constructor(
                                _tag = constructor._tag + FieldModel.Glossary.kTagSeparator +
                                    TrunkModel.Glossary.kESignalInput,
                                _mode = Signal.ModeEnum.Continuous,
                                _tappableKeyOpt = Some(port.GetTapKey)))).head
                    input.GetKey
            }

            // Get emitter patch definition, using default patch if necessary:
            val patchDef: JsObject = constructor._patchDefOpt.getOrElse(Patch.kDefaultEmitterPatchDef)

            // Create emitter patch element (child):
            CreateEmitterPatch(
                field = field,
                emitterKey = emitter.GetKey,
                tag = constructor._tag + FieldModel.Glossary.kTagSeparator + TrunkModel.Glossary.kEEmitterPatch,
                modulatorKey = modulatorKey,
                patchDef = patchDef)
        }

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        field.BindEmitter(emitter.GetKey)

        // 6: Activate element:
        // Deploy emitter patch:
        DeployEmitterPatch(field, emitter)

        // Return element:
        emitter
    }

    def DestroyFieldEmitter(
        field: Field,
        destructor: FieldEmitter.Destructor,
        isForcedDestroy: Boolean): FieldEmitter.Key =
    {
        _fieldEmitters.get((field.GetKey, destructor._key)) match
        {
            case Some(emitter) =>
                // 1: Unbind with parent:
                val isUnbound = field.UnbindEmitter(emitter.GetKey, isForcedDestroy)
                if (isUnbound)
                {
                    // 2: Deactivate element:
                    UndeployEmitterPatch(field, emitter)

                    // 3: Unbind with peers:
                    // N/A

                    // 4: Unbind with/destroy children:
                    if (destructor._scope == 'ScopeDeep)
                        DestroyEmitterPatch(field, emitter)

                    // 5: Remove element from store:
                    _fieldEmitters -= ((field.GetKey, emitter.GetKey))
                }

            case None => throw new FieldException(Cell.ErrorCodes.SubjectEmitterUnknown)
        }

        // Return field emitter key:
        destructor._key
    }

    def DestroyAllFieldEmitters(
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        // Destroy each field emitter of field:
        _fieldEmitters.filter(_._1._1 == fieldKey).foreach(emitterPair =>
        {
            val ((_, pairEmitterKey), _) = emitterPair
            val fieldEmitterDestructor = new FieldEmitter.Destructor(pairEmitterKey, scope)
            DestroyFieldEmitter(field, fieldEmitterDestructor, isForcedDestroy = true)
        })
    }

    def GetFieldEmitterOpt (
        field: Field,
        key: FieldEmitter.Key,
        isRequired: Boolean = true): Option[FieldEmitter] =
    {
        // Lookup field emitter:
        key match
        {
            case _: FieldEmitter.Key =>
                val opt = _fieldEmitters.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.FieldEmitterUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.FieldEmitterKeyInvalid)
        }
    }

    def GetFieldEmitters(field: Field): Vector[FieldEmitter] =
    {
        val fieldKey = field.GetKey

        // Return field emitters vector:
        _fieldEmitters.filter(_._1._1 == fieldKey).values.toVector
    }

    def GetElementCount(fieldKey: Field.Key): Int = _fieldEmitters.count(_._1._1 == fieldKey)
}
