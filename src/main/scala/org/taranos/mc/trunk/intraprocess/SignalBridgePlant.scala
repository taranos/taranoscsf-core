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

import org.taranos.mc.Cell


class SignalBridgePlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _bridges = mutable.HashMap.empty[(Trunk.Key, SignalBridge.Key), SignalBridge]

    def CreateSignalBridge (
        trunk: Trunk,
        constructor: SignalBridge.Constructor): SignalBridge =
    {
        // Get modulatable tap to bind with:
        val modulatableTap: SignalTap = constructor._modulatableKey match
        {
            case inputKey: SignalInput.Key =>
                _trunkModel.GetSignalInputOpt(trunk.GetKey, inputKey) match
                {
                    case Some(forwardInput) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, forwardInput.GetTapKey).getOrElse(
                            throw TrunkException(Cell.ErrorCodes.SignalInputTapless))

                    case None => throw TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                }

            case oscillatorPatchKey: OscillatorPatch.Key =>
                _trunkModel.GetOscillatorPatchOpt(trunk.GetKey, oscillatorPatchKey) match
                {
                    case Some(forwardOscillatorPatch) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, forwardOscillatorPatch.GetTapKey).getOrElse(
                            throw TrunkException(Cell.ErrorCodes.OscillatorPatchUnknown))

                    case None => throw TrunkException(Cell.ErrorCodes.OscillatorPatchUnknown)
                }

            case bridgeKey: SignalBridge.Key =>
                _trunkModel.GetSignalBridgeOpt(trunk.GetKey, bridgeKey) match
                {
                    case Some(forwardBridge) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, forwardBridge.GetTapKey).getOrElse(
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
                            _mode = constructor._mode))).head

            // Cannot bind with anything else:
            case _ =>
                throw TrunkException(Cell.ErrorCodes.SignalBridgeInvalid)
        }

        // Create link from modulatable tap to routing tap:
        _trunkModel.CreateSignalLinks(
            trunk.GetKey,
            Vector(
                SignalLink.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalLink,
                    _sourceKey = modulatableTap.GetSourceKey,
                    _sinkKey = routingTap.GetSinkKey,
                    _partOpt = constructor._partOpt)))

        // Create bridge element:
        val bridge = new SignalBridge(
            new SignalBridge.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._mode),
            new SignalBridge.Attrs(),
            new SignalBridge.Refs(
                trunk.GetKey,
                routingTap.GetKey,
                isTapParent))

        // 1: Add element to store:
        _bridges += (trunk.GetKey, bridge.GetKey) -> bridge

        // 2: Bind with trunk:
        trunk.BindSignalBridge(bridge.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        routingTap.BindModulator(bridge.GetKey, isListener = false)

        // 5: Bind with children:
        // N/A

        // Return bridge:
        bridge
    }

    def DestroySignalBridge (
        trunk: Trunk,
        destructor: SignalBridge.Destructor): SignalBridge.Key =
    {
        destructor._key match
        {
            case key: SignalBridge.Key =>
                _bridges.get((trunk.GetKey, key)) match
                {
                    case Some(bridge) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        // Routing tap:
                        if (!bridge.IsTapParent)
                            _trunkModel.GetSignalTapOpt(trunk.GetKey, bridge.GetTapKey) match
                            {
                                case Some(tap) => tap.UnbindModulator()

                                case None =>    // We don't care...
                            }

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalBridge(bridge.GetKey)

                        // 5: Destroy children:
                        // Routing tap:
                        if (bridge.IsTapParent)
                        {
                            val tapDestructor = SignalTap.Destructor(bridge.GetTapKey)
                            _trunkModel.DestroySignalTaps(trunk.GetKey, Vector(tapDestructor))
                        }

                        // 6: Remove element from store:
                        _bridges -= ((trunk.GetKey, bridge.GetKey))

                    case None => throw TrunkException(Cell.ErrorCodes.SignalBridgeUnknown)
                }

            case _ => throw TrunkException(Cell.ErrorCodes.SignalBridgeInvalid)
        }

        // Return bridge key:
        destructor._key
    }

    def DestroyAllSignalBridges (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each bridge of trunk:
        _bridges.filter(_._1._1 == trunkKey).foreach(bridgePair =>
        {
            val ((_, pairBridgeKey), _) = bridgePair
            val bridgeDestructor = SignalBridge.Destructor(pairBridgeKey)
            DestroySignalBridge(trunk, bridgeDestructor)
        })
    }

    def GetSignalBridgeOpt (
        trunk: Trunk,
        key: SignalBridge.Key,
        isRequired: Boolean = true): Option[SignalBridge] =
    {
        // Lookup bridge key:
        key match
        {
            case _: SignalBridge.Key =>
                val opt = _bridges.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw TrunkException(Cell.ErrorCodes.SignalBridgeUnknown)
                opt

            case _ => throw TrunkException(Cell.ErrorCodes.SignalBridgeKeyInvalid)
        }
    }

    def GetSignalBridges (trunk: Trunk): Vector[SignalBridge] =
    {
        val trunkKey = trunk.GetKey

        // Return bridges vector:
        _bridges.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalBridgeKeys (trunk: Trunk): Vector[SignalBridge.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return subject emitter keys vector:
        _bridges.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int =
        _bridges.count(_._1._1 == trunkKey)
}
