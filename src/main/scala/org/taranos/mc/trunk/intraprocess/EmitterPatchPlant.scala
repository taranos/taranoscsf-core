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

package org.taranos.mc.trunk.intraprocess

import org.taranos.mc.field._
import org.taranos.mc.{Cell, CellLogger}


class EmitterPatchPlant
(implicit
    protected val _trunkModel: TrunkModel,
    protected val _logger: CellLogger)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _patches = mutable.HashMap.empty[(Trunk.Key, EmitterPatch.Key), EmitterPatch]

    def CreateEmitterPatch (
        trunk: Trunk,
        constructor: EmitterPatch.Constructor): EmitterPatch =
    {
        // Get modulatable tap to bind with:
        val modulatableTap: SignalTap = constructor._modulatableKey match
        {
            case inputKey: SignalInput.Key =>
                _trunkModel.GetSignalInputOpt(trunk.GetKey, inputKey) match
                {
                    case Some(input) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, input.GetTapKey).getOrElse(
                            throw TrunkException(Cell.ErrorCodes.SignalInputTapless))

                    case None => throw TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                }

            case bridgeKey: SignalBridge.Key =>
                _trunkModel.GetSignalBridgeOpt(trunk.GetKey, bridgeKey) match
                {
                    case Some(bridge) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, bridge.GetTapKey).getOrElse(
                            throw TrunkException(Cell.ErrorCodes.SignalBridgeTapless))

                    case None => throw TrunkException(Cell.ErrorCodes.SignalBridgeUnknown)
                }
        }
        
        // Get routing tap to bind with:
        var isTapParent: Boolean = false
        val routingTap: SignalTap = constructor._tappableKeyOpt match
        {
            // Get supplied tap:
            case Some(tapKey) if tapKey.isInstanceOf[SignalTap.Key] =>
                _trunkModel.GetSignalTapOpt(trunk.GetKey, tapKey.asInstanceOf[SignalTap.Key]).getOrElse(
                    throw TrunkException(Cell.ErrorCodes.SignalTapInvalid))

            // Make a new tap:
            case None =>
                isTapParent = true
                _trunkModel.CreateSignalTaps(
                    trunk.GetKey,
                    Vector(
                        SignalTap.Constructor(
                            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator +
                                TrunkModel.Glossary.kESignalTap,
                            _mode = Signal.ModeEnum.Continuous))).head

            // Cannot bind with anything else:
            case _ =>
                throw TrunkException(Cell.ErrorCodes.EmitterPatchInvalid)
        }

        // Create link from modulatable tap to routing tap:
        _trunkModel.CreateSignalLinks(
            trunk.GetKey,
            Vector(
                SignalLink.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalLink,
                    _sourceKey = modulatableTap.GetSourceKey,
                    _sinkKey = routingTap.GetSinkKey)))

        // Create emitter patch element:
        val patch = new EmitterPatch(
            new EmitterPatch.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new EmitterPatch.Attrs(constructor._patchDef),
            new EmitterPatch.Refs(
                trunk.GetKey,
                routingTap.GetKey,
                isTapParent,
                constructor._fieldKey,
                constructor._emitterKey))

        // 1: Add element to store:
        _patches += (trunk.GetKey, patch.GetKey) -> patch

        // 2: Bind with trunk:
        trunk.BindEmitterPatch(patch.GetKey)

        // 3: Bind with parent:
        val fieldModel = _trunkModel.GetFieldModel
        val field = fieldModel.GetField(constructor._fieldKey)
        val emitter = constructor._emitterKey match
        {
            case fieldEmitterKey: FieldEmitter.Key =>
                fieldModel.GetFieldEmitterOpt(field.GetKey, fieldEmitterKey).get

            case subjectEmitterKey: SubjectEmitter.Key =>
                fieldModel.GetSubjectEmitterOpt(field.GetKey, subjectEmitterKey).get

            case probeEmitterKey: ProbeEmitter.Key =>
                fieldModel.GetProbeEmitterOpt(field.GetKey, probeEmitterKey).get

            case _ => throw TrunkException(Cell.ErrorCodes.EmitterPatchConstructorInvalid)
        }
        emitter.BindPatch(patch.GetKey)

        // 4: Bind with peers:
        // Routing tap:
        if (!isTapParent)
            routingTap.BindModulator(patch.GetKey, isListener = false)

        // 5: Bind with children:
        // Routing tap:
        if (isTapParent)
            routingTap.BindModulator(patch.GetKey, isListener = false)

        // Return patch:
        patch
    }

    def DestroyEmitterPatch (
        trunk: Trunk,
        destructor: EmitterPatch.Destructor): EmitterPatch.Key =
    {
        _patches.get((trunk.GetKey, destructor._key)) match
        {
            case Some(patch) =>
                // 1: Unbind with children:
                // Routing tap:
                if (patch.IsTapParent)
                    _trunkModel.GetSignalTapOpt(trunk.GetKey, patch.GetTapKey) match
                    {
                        case Some(tap) => tap.UnbindModulator()

                        case None =>    // We don't care...
                    }

                // 2: Unbind with peers:
                // Routing tap:
                if (!patch.IsTapParent)
                    _trunkModel.GetSignalTapOpt(trunk.GetKey, patch.GetTapKey) match
                    {
                        case Some(tap) => tap.UnbindModulator()

                        case None =>    // We don't care...
                    }

                // 3: Unbind with parent:
                val fieldModel = _trunkModel.GetFieldModel
                val field = fieldModel.GetField(patch.GetFieldKey)
                val emitter = patch.GetEmitterKey match
                {
                    case fieldEmitterKey: FieldEmitter.Key =>
                        fieldModel.GetFieldEmitterOpt(field.GetKey, fieldEmitterKey).get

                    case subjectEmitterKey: SubjectEmitter.Key =>
                        fieldModel.GetSubjectEmitterOpt(field.GetKey, subjectEmitterKey).get

                    case probeEmitterKey: ProbeEmitter.Key =>
                        fieldModel.GetProbeEmitterOpt(field.GetKey, probeEmitterKey).get

                    case _ => throw TrunkException(Cell.ErrorCodes.EmitterPatchConstructorInvalid)
                }
                emitter.UnbindPatch()

                // 4: Unbind with trunk:
                trunk.UnbindEmitterPatch(patch.GetKey)

                // 5: Destroy children:
                // Routing tap:
                if (patch.IsTapParent)
                {
                    val tapDestructor = SignalTap.Destructor(patch.GetTapKey)
                    _trunkModel.DestroySignalTaps(trunk.GetKey, Vector(tapDestructor))
                }

                // 6: Remove element from store:
                _patches -= ((trunk.GetKey, patch.GetKey))

            case None => throw TrunkException(Cell.ErrorCodes.EmitterPatchUnknown)
        }

        // Return patch key:
        destructor._key
    }

    def DestroyAllEmitterPatches (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each patch of trunk:
        _patches.filter(_._1._1 == trunkKey).foreach(patchPair =>
        {
            val ((_, pairPatchKey), _) = patchPair
            val patchDestructor = EmitterPatch.Destructor(pairPatchKey)
            DestroyEmitterPatch(trunk, patchDestructor)
        })
    }

    def GetEmitterPatchOpt (
        trunk: Trunk,
        key: EmitterPatch.Key,
        isRequired: Boolean = true): Option[EmitterPatch] =
    {
        // Lookup patch key:
        key match
        {
            case _: EmitterPatch.Key =>
                val opt = _patches.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw TrunkException(Cell.ErrorCodes.EmitterPatchUnknown)
                opt

            case _ => throw TrunkException(Cell.ErrorCodes.EmitterPatchKeyInvalid)
        }
    }

    def GetEmitterPatches (trunk: Trunk): Vector[EmitterPatch] =
    {
        val trunkKey = trunk.GetKey

        // Return patches vector:
        _patches.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetEmitterPatchKeys (trunk: Trunk): Vector[EmitterPatch.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return patch keys vector:
        _patches.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int =
        _patches.count(_._1._1 == trunkKey)
}
