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

package org.taranos.mc.trunk.intraprocess

import org.taranos.mc.Cell


class SignalOutputPlant
(   implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _outputs = mutable.HashMap.empty[(Trunk.Key, SignalOutput.Key), SignalOutput]

    def CreateSignalOutput (
        trunk: Trunk,
        constructor: SignalOutput.Constructor): SignalOutput =
    {
        // Get modulatable tap to bind with:
        val modulatableTap: SignalTap = constructor._modulatableKey match
        {
            case inputKey: SignalInput.Key =>
                _trunkModel.GetSignalInputOpt(trunk.GetKey, inputKey) match
                {
                    case Some(input) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, input.GetTapKey).getOrElse(
                            throw new TrunkException(Cell.ErrorCodes.SignalInputTapless))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                }

            case oscillatorPatchKey: OscillatorPatch.Key =>
                _trunkModel.GetOscillatorPatchOpt(trunk.GetKey, oscillatorPatchKey) match
                {
                    case Some(oscillatorPatch) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, oscillatorPatch.GetTapKey).getOrElse(
                            throw new TrunkException(Cell.ErrorCodes.OscillatorPatchTapless))

                    case None => throw new TrunkException(Cell.ErrorCodes.OscillatorPatchUnknown)
                }

            case bridgeKey: SignalBridge.Key =>
                _trunkModel.GetSignalBridgeOpt(trunk.GetKey, bridgeKey) match
                {
                    case Some(bridge) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, bridge.GetTapKey).getOrElse(
                            throw new TrunkException(Cell.ErrorCodes.SignalBridgeTapless))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalBridgeUnknown)
                }
        }

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
                            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalTap,
                            _mode = constructor._mode))).head

            // Cannot bind with anything else:
            case _ =>
                throw new TrunkException(Cell.ErrorCodes.SignalOutputInvalid)
        }

        // Create link from modulatable tap to routing tap:
        _trunkModel.CreateSignalLinks(
            trunk.GetKey,
            Vector(
                new SignalLink.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kRSignalLinks,
                    _sourceKey = modulatableTap.GetSourceKey,
                    _sinkKey = routingTap.GetSinkKey,
                    _partOpt = constructor._partOpt)))

        // Create output element:
        val output = new SignalOutput(
            new SignalOutput.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._mode),
            new SignalOutput.Attrs(),
            new SignalOutput.Refs(
                trunk.GetKey,
                routingTap.GetKey,
                isTapParent))

        // 1: Add element to store:
        _outputs += (trunk.GetKey, output.GetKey) -> output

        // 2: Bind with trunk:
        trunk.BindSignalOutput(output.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        routingTap.BindModulator(output.GetKey, isListener = false)

        // 5: Bind with children:
        // N/A

        // Return output:
        output
    }

    def DestroySignalOutput (
        trunk: Trunk,
        destructor: SignalOutput.Destructor): SignalOutput.Key =
    {
        destructor.key match
        {
            case key: SignalOutput.Key =>
                _outputs.get((trunk.GetKey, key)) match
                {
                    case Some(output) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        // Routing tap:
                        if (!output.IsTapParent)
                            _trunkModel.GetSignalTapOpt(trunk.GetKey, output.GetTapKey) match
                            {
                                case Some(tap) => tap.UnbindModulator()

                                case None =>    // We don't care...
                            }

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalOutput(output.GetKey)

                        // 5: Destroy children:
                        // Routing tap:
                        if (output.IsTapParent)
                        {
                            val tapDestructor = new SignalTap.Destructor(output.GetTapKey)
                            _trunkModel.DestroySignalTaps(trunk.GetKey, Vector(tapDestructor))
                        }

                        // 6: Remove element from store:
                        _outputs -= ((trunk.GetKey, output.GetKey))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalOutputUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalOutputInvalid)
        }

        // Return output key:
        destructor.key
    }

    def DestroyAllSignalOutputs (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each output of trunk:
        _outputs.filter(pair => pair._1._1 == trunkKey).foreach(outputPair =>
        {
            val ((_, pairOutputKey), _) = outputPair
            val outputDestructor = new SignalOutput.Destructor(pairOutputKey)
            DestroySignalOutput(trunk, outputDestructor)
        })
    }

    def GetSignalOutputOpt (
        trunk: Trunk,
        key: SignalOutput.Key,
        isRequired: Boolean = true): Option[SignalOutput] =
    {
        // Lookup output key:
        key match
        {
            case _: SignalOutput.Key =>
                val opt = _outputs.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalOutputUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalOutputKeyInvalid)
        }
    }

    def GetSignalOutputs (trunk: Trunk): Vector[SignalOutput] =
    {
        val trunkKey = trunk.GetKey

        // Return outputs vector:
        _outputs.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalOutputKeys (trunk: Trunk): Vector[SignalOutput.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return subject emitter keys vector:
        _outputs.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _outputs.count(_._1._1 == trunkKey)
}
