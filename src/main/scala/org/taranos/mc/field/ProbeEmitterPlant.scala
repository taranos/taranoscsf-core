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


class ProbeEmitterPlant
(implicit
    protected val _fieldModel: FieldModel,
    protected val _trunkModel: TrunkModel,
    protected val _logger: CellLogger)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _probeEmitters = mutable.HashMap.empty[(Field.Key, ProbeEmitter.Key), ProbeEmitter]

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
        emitter: ProbeEmitter) =
    {
        _trunkModel.DestroyEmitterPatches(
            field.GetTrunkKey,
            Vector(new EmitterPatch.Destructor(emitter.GetPatchKey)))
    }

    def DeployEmitterPatch (
        field: Field,
        emitter: ProbeEmitter,
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
            val oscillatorConstructor = new ProbeOscillator.Constructor(
                _tag = emitter.GetTag + FieldModel.Glossary.kTagSeparator + channelTag,
                _channelDef = channelDef)
            _logger.LogDebug(s"DEP: creating ProbeOscillator: $oscillatorConstructor")
            _fieldModel.CreateProbeOscillators(
                field.GetKey,
                emitter.GetKey,
                Vector(oscillatorConstructor))
        })
    }

    private
    def UndeployEmitterPatch (
        field: Field,
        emitter: ProbeEmitter) =
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

            val oscillatorDestructor = new ProbeOscillator.Destructor(
                _key = oscillatorKey.asInstanceOf[ProbeOscillator.Key],
                _scope = 'ScopeDeep)
            _logger.LogDebug(s"DEP: destroying ProbeOscillator: $oscillatorDestructor")
            _fieldModel.DestroyProbeOscillators(
                fieldKey = field.GetKey,
                destructors = Vector(oscillatorDestructor),
                isForcedDestroy = true)
        })
        emitterPatch.ClearOscillatorPatchKeysMap()
    }

    def CreateProbeEmitter (
        field: Field,
        probe: Probe,
        constructor: ProbeEmitter.Constructor): ProbeEmitter =
    {
        val trunkKey = field.GetTrunkKey

        // 1: Create element:
        val emitter = new ProbeEmitter(
            new ProbeEmitter.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new ProbeEmitter.Refs(
                field.GetKey,
                probe.GetKey))

        // 2: Add element to store:
        _probeEmitters += (field.GetKey, emitter.GetKey) -> emitter

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
        probe.BindEmitter(emitter.GetKey)

        // 6: Activate element:
        // Deploy emitter patch:
        DeployEmitterPatch(field, emitter)

        // Return element:
        emitter
    }

    def DestroyProbeEmitter (
        field: Field,
        destructor: ProbeEmitter.Destructor): ProbeEmitter.Key =
    {
        _probeEmitters.get((field.GetKey, destructor._key)) match
        {
            case Some(emitter) =>
                // 1: Unbind with parent:
                val probe = _fieldModel.GetProbeOpt(
                    field.GetKey,
                    emitter.GetParentKey.asInstanceOf[Probe.Key]).get
                probe.UnbindEmitter(emitter.GetKey)

                // 2: Deactivate element:
                UndeployEmitterPatch(field, emitter)

                // 3: Unbind with peers:
                // N/A

                // 4: Unbind with/destroy children:
                if (destructor._scope == 'ScopeDeep)
                    DestroyEmitterPatch(field, emitter)

                // 5: Remove element from store:
                _probeEmitters -= ((field.GetKey, emitter.GetKey))

            case None => throw new FieldException(Cell.ErrorCodes.ProbeEmitterUnknown)
        }

        // Return probe emitter key:
        destructor._key
    }

    def DestroyAllProbeEmitters (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        // Destroy each probe emitter of {field, probe}:
        _probeEmitters.filter(_._1._1 == fieldKey).foreach(emitterPair =>
        {
            val ((_, pairEmitterKey), _) = emitterPair
            val probeEmitterDestructor = new ProbeEmitter.Destructor(pairEmitterKey, scope)
            DestroyProbeEmitter(field, probeEmitterDestructor)
        })
    }

    def GetProbeEmitterOpt (
        field: Field,
        key: ProbeEmitter.Key,
        isRequired: Boolean = true): Option[ProbeEmitter] =
    {
        // Lookup probe emitter:
        key match
        {
            case _: ProbeEmitter.Key =>
                val opt = _probeEmitters.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.ProbeEmitterUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.ProbeEmitterKeyInvalid)
        }
    }

    def GetProbeEmitters (
        field: Field,
        probeOpt: Option[Probe] = None): Vector[ProbeEmitter] =
    {
        // Return probe emitters vector:
        val fieldKey = field.GetKey
        if (probeOpt.isDefined)
        {
            val probeKey = probeOpt.get.GetKey
            _probeEmitters.filter(pair =>
            {
                val ((pairFieldKey, pairEmitterKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetProbeEmitterOpt(fieldKey, pairEmitterKey).get.GetParentKey == probeKey
            }).values.toVector
        }
        else
            _probeEmitters.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).values.toVector
    }

    def GetProbeEmitterKeys (
        field: Field,
        probeOpt: Option[Probe] = None): Vector[ProbeEmitter.Key] =
    {
        // Return probe emitter keys vector:
        val fieldKey = field.GetKey
        if (probeOpt.isDefined)
        {
            val probeKey = probeOpt.get.GetKey
            _probeEmitters.filter(pair =>
            {
                val ((pairFieldKey, pairEmitterKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetProbeEmitterOpt(fieldKey, pairEmitterKey).get.GetParentKey == probeKey
            }).keys.map(pair => pair._2).toVector
        }
        else
            _probeEmitters.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).keys.map(pair => pair._2).toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int = _probeEmitters.count(_._1._1 == fieldKey)
}
