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


class OscillatorPatchPlant
(implicit
    protected val _trunkModel: TrunkModel,
    protected val _logger: CellLogger)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _patches = mutable.HashMap.empty[(Trunk.Key, OscillatorPatch.Key), OscillatorPatch]

    def CreateOscillatorPatch (
        trunk: Trunk,
        constructor: OscillatorPatch.Constructor): OscillatorPatch =
    {
        // Get parent emitter patch:
        val emitterPatch: EmitterPatch = _trunkModel.GetEmitterPatchOpt(trunk.GetKey, constructor._emitterPatchKey)
            .getOrElse(throw new TrunkException(Cell.ErrorCodes.EmitterPatchUnknown))

        // Get modulatable (emitter patch) tap to bind with:
        val modulatableTap: SignalTap = _trunkModel.GetSignalTapOpt(trunk.GetKey, emitterPatch.GetTapKey).getOrElse(
            throw new TrunkException(Cell.ErrorCodes.EmitterPatchTapless))

        // Get routing tap to bind with:
        var isTapParent: Boolean = false
        val routingTap: SignalTap = constructor._tappableKeyOpt match
        {
            // Get supplied tap:
            case Some(tapKey) if tapKey.isInstanceOf[SignalTap.Key] =>
                _trunkModel.GetSignalTapOpt(trunk.GetKey, tapKey.asInstanceOf[SignalTap.Key]).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.SignalTapInvalid))

            // Make a new tap:
            case None =>
                isTapParent = true
                _trunkModel.CreateSignalTaps(
                    trunk.GetKey,
                    Vector(
                        new SignalTap.Constructor(
                            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator +
                                TrunkModel.Glossary.kESignalTap,
                            _mode = Signal.ModeEnum.Continuous))).head

            // Cannot bind with anything else:
            case _ =>
                throw new TrunkException(Cell.ErrorCodes.OscillatorPatchInvalid)
        }

        // Create link from modulatable tap to routing tap:
        _trunkModel.CreateSignalLinks(
            trunk.GetKey,
            Vector(
                new SignalLink.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalLink,
                    _sourceKey = modulatableTap.GetSourceKey,
                    _sinkKey = routingTap.GetSinkKey,
                    _partOpt = Some(constructor._channelTag))))

        // Create oscillator patch element:
        val patch = new OscillatorPatch(
            new OscillatorPatch.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new OscillatorPatch.Attrs(
                constructor._channelTag,
                constructor._patchDef),
            new OscillatorPatch.Refs(
                trunk.GetKey,
                routingTap.GetKey,
                isTapParent,
                constructor._fieldKey))

        // 1: Add element to store:
        _patches += (trunk.GetKey, patch.GetKey) -> patch

        // 2: Bind with trunk:
        trunk.BindOscillatorPatch(patch.GetKey)

        // 3: Bind with parent:

        // 4: Bind with peers:
        // Emitter patch:
        emitterPatch.BindOscillatorPatch(constructor._channelTag, patch.GetKey)
        // Routing tap:
        if (!isTapParent)
            routingTap.BindModulator(patch.GetKey, isListener = true)

        // 5: Bind with children:
        // Routing tap:
        if (isTapParent)
            routingTap.BindModulator(patch.GetKey, isListener = true)
        // Create and bind with loudness output:
        val loudnessOutput = _trunkModel.CreateSignalOutputs(
            trunk.GetKey,
            Vector(
                new SignalOutput.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalOutput +
                        TrunkModel.Glossary.kTagSubSeparator + FieldModel.Glossary.kQLoudness,
                    _mode = Signal.ModeEnum.Continuous,
                    _modulatableKey = patch.GetKey,
                    _tappableKeyOpt = None,
                    _partOpt = Some(FieldModel.Glossary.kQLoudness)))).head
        patch.BindLoudnessOutput(loudnessOutput.GetKey)
        // Create and bind with period output:
        val periodOutput = _trunkModel.CreateSignalOutputs(
            trunk.GetKey,
            Vector(
                new SignalOutput.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalOutput +
                        TrunkModel.Glossary.kTagSubSeparator + FieldModel.Glossary.kQPeriod,
                    _mode = Signal.ModeEnum.Continuous,
                    _modulatableKey = patch.GetKey,
                    _tappableKeyOpt = None,
                    _partOpt = Some(FieldModel.Glossary.kQPeriod)))).head
        patch.BindPeriodOutput(periodOutput.GetKey)
        // Create and bind with pitch output:
        val pitchOutput = _trunkModel.CreateSignalOutputs(
            trunk.GetKey,
            Vector(
                new SignalOutput.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalOutput +
                        TrunkModel.Glossary.kTagSubSeparator + FieldModel.Glossary.kQPitch,
                    _mode = Signal.ModeEnum.Continuous,
                    _modulatableKey = patch.GetKey,
                    _tappableKeyOpt = None,
                    _partOpt = Some(FieldModel.Glossary.kQPitch)))).head
        patch.BindPitchOutput(pitchOutput.GetKey)
        // Create and bind with shape output:
        val shapeOutput = _trunkModel.CreateSignalOutputs(
            trunk.GetKey,
            Vector(
                new SignalOutput.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalOutput +
                        TrunkModel.Glossary.kTagSubSeparator + FieldModel.Glossary.kQShape,
                    _mode = Signal.ModeEnum.Continuous,
                    _modulatableKey = patch.GetKey,
                    _tappableKeyOpt = None,
                    _partOpt = Some(FieldModel.Glossary.kQShape)))).head
        patch.BindShapeOutput(shapeOutput.GetKey)
        // Create and bind with tone output:
        val toneOutput = _trunkModel.CreateSignalOutputs(
            trunk.GetKey,
            Vector(
                new SignalOutput.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalOutput +
                        TrunkModel.Glossary.kTagSubSeparator + FieldModel.Glossary.kQTone,
                    _mode = Signal.ModeEnum.Continuous,
                    _modulatableKey = patch.GetKey,
                    _tappableKeyOpt = None,
                    _partOpt = Some(FieldModel.Glossary.kQTone)))).head
        patch.BindToneOutput(toneOutput.GetKey)

        // Return patch:
        patch
    }

    def DestroyOscillatorPatch (
        trunk: Trunk,
        destructor: OscillatorPatch.Destructor): OscillatorPatch.Key =
    {
        _patches.get((trunk.GetKey, destructor._key)) match
        {
            case Some(patch) =>
                // 1: Unbind with children:
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
                // Emitter patch:
                _trunkModel.GetEmitterPatchOpt(
                    trunk.GetKey,
                    patch.GetEmitterPatchKey,
                    isRequired = false) match
                {
                    case Some(emitterPatch) => emitterPatch.UnbindOscillatorPatch(patch.GetChannelTag)

                    case None =>    // We don't care...
                }

                // 3: Unbind with parent:
                val fieldModel = _trunkModel.GetFieldModel
                val field = fieldModel.GetField(patch.GetFieldKey)
                val oscillatorOpt = patch.GetOscillatorKey match
                {
                    case fieldOscillatorKey: FieldOscillator.Key =>
                        fieldModel.GetFieldOscillatorOpt(field.GetKey, fieldOscillatorKey)

                    case subjectOscillatorKey: SubjectOscillator.Key =>
                        fieldModel.GetSubjectOscillatorOpt(field.GetKey, subjectOscillatorKey)

                    case probeOscillatorKey: ProbeOscillator.Key =>
                        fieldModel.GetProbeOscillatorOpt(field.GetKey, probeOscillatorKey)

                    case _ => throw new TrunkException(Cell.ErrorCodes.OscillatorPatchConstructorInvalid)
                }
                oscillatorOpt.get.UnbindPatch()

                // 4: Unbind with trunk:
                trunk.UnbindOscillatorPatch(patch.GetKey)

                // 5: Destroy children:
                // Outputs:
                val outputDestructors = Vector(
                    new SignalOutput.Destructor(patch.GetLoudnessOutputKey),
                    new SignalOutput.Destructor(patch.GetPeriodOutputKey),
                    new SignalOutput.Destructor(patch.GetPitchOutputKey),
                    new SignalOutput.Destructor(patch.GetShapeOutputKey),
                    new SignalOutput.Destructor(patch.GetToneOutputKey))
                _trunkModel.DestroySignalOutputs(trunk.GetKey, outputDestructors)
                // Routing tap:
                if (patch.IsTapParent)
                {
                    val tapDestructor = new SignalTap.Destructor(patch.GetTapKey)
                    _trunkModel.DestroySignalTaps(trunk.GetKey, Vector(tapDestructor))
                }

                // 6: Remove element from store:
                _patches -= ((trunk.GetKey, patch.GetKey))

            case None => throw new TrunkException(Cell.ErrorCodes.OscillatorPatchUnknown)
        }

        // Return patch key:
        destructor._key
    }

    def DestroyAllOscillatorPatches (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each patch of trunk:
        _patches.filter(_._1._1 == trunkKey).foreach(patchPair =>
        {
            val ((_, pairPatchKey), _) = patchPair
            val patchDestructor = new OscillatorPatch.Destructor(pairPatchKey)
            DestroyOscillatorPatch(trunk, patchDestructor)
        })
    }

    def GetOscillatorPatchOpt (
        trunk: Trunk,
        key: OscillatorPatch.Key,
        isRequired: Boolean = true): Option[OscillatorPatch] =
    {
        // Lookup patch key:
        key match
        {
            case _: OscillatorPatch.Key =>
                val opt = _patches.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.OscillatorPatchUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.OscillatorPatchKeyInvalid)
        }
    }

    def GetOscillatorPatches (trunk: Trunk): Vector[OscillatorPatch] =
    {
        val trunkKey = trunk.GetKey

        // Return patches vector:
        _patches.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetOscillatorPatchKeys (trunk: Trunk): Vector[OscillatorPatch.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return patch keys vector:
        _patches.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _patches.count(_._1._1 == trunkKey)
}
