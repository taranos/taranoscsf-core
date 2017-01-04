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


class SignalInputPlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _inputs = mutable.HashMap.empty[(Trunk.Key, SignalInput.Key), SignalInput]

    def CreateSignalInput (
        trunk: Trunk,
        constructor: SignalInput.Constructor): SignalInput =
    {
        // Get routing tappable to bind with:
        val routingTap: SignalTap = constructor._tappableKeyOpt match
        {
            // Get supplied port's tap:
            case Some(portKey) if portKey.isInstanceOf[SignalPort.Key] =>
                _trunkModel.GetSignalPortOpt(trunk.GetKey, portKey.asInstanceOf[SignalPort.Key]) match
                {
                    case Some(port) =>
                        _trunkModel.GetSignalTapOpt(trunk.GetKey, port.GetTapKey).getOrElse(
                            throw new TrunkException(Cell.ErrorCodes.SignalPortTapless))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalPortUnknown)
                }

            // Get supplied tap:
            case Some(tapKey) if tapKey.isInstanceOf[SignalTap.Key] =>
                _trunkModel.GetSignalTapOpt(trunk.GetKey, tapKey.asInstanceOf[SignalTap.Key]).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.SignalTapInvalid))

            // Get tap of a new port on the default interface:
            case None =>
                val portConstructor = SignalPort.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalPort,
                    _aliasOpt = Some(constructor._tag),
                    _mode = constructor._mode)
                val port = _trunkModel.CreateSignalPorts(
                    trunk.GetKey,
                    SignalInterface.kAnyKey,
                    Vector(portConstructor)).head
                _trunkModel.GetSignalTapOpt(trunk.GetKey, port.GetTapKey).get

            // Cannot bind with anything else, including other modulators:
            case _ =>
                throw new TrunkException(Cell.ErrorCodes.SignalInputInvalid)
        }

        // Create input element:
        val input = new SignalInput(
            new SignalInput.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._mode),
            new SignalInput.Attrs(),
            new SignalInput.Refs(
                trunk.GetKey,
                routingTap.GetKey,
                isTapParent = false))

        // 1: Add element to store:
        _inputs += (trunk.GetKey, input.GetKey) -> input

        // 2: Bind with trunk:
        trunk.BindSignalInput(input.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        routingTap.BindModulator(input.GetKey, isListener = false)

        // 5: Bind with children:
        // N/A

        // Return input:
        input
    }

    def DestroySignalInput (
        trunk: Trunk,
        destructor: SignalInput.Destructor): SignalInput.Key =
    {
        destructor._key match
        {
            case key: SignalInput.Key =>
                _inputs.get((trunk.GetKey, key)) match
                {
                    case Some(input) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        _trunkModel.GetSignalTapOpt(
                            trunk.GetKey,
                            input.GetTapKey,
                            isRequired = false) match
                        {
                            case Some(tap) => tap.UnbindModulator()

                            case None =>    // We don't care...
                        }

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalInput(input.GetKey)

                        // 5: Destroy children:
                        // N/A

                        // 6: Remove element from store:
                        _inputs -= ((trunk.GetKey, input.GetKey))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalInputInvalid)
        }

        // Return input key:
        destructor._key
    }

    def DestroyAllSignalInputs (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each input of trunk:
        _inputs.filter(_._1._1 == trunkKey).foreach(inputPair =>
        {
            val ((_, pairInputKey), _) = inputPair
            val inputDestructor = new SignalInput.Destructor(pairInputKey)
            DestroySignalInput(trunk, inputDestructor)
        })
    }

    def GetSignalInputOpt (
        trunk: Trunk,
        key: SignalInput.Key,
        isRequired: Boolean = true): Option[SignalInput] =
    {
        // Lookup input key:
        key match
        {
            case _: SignalInput.Key =>
                val opt = _inputs.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalInputKeyInvalid)
        }
    }

    def GetSignalInputs (trunk: Trunk): Vector[SignalInput] =
    {
        val trunkKey = trunk.GetKey

        // Return inputs vector:
        _inputs.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalInputKeys (trunk: Trunk): Vector[SignalInput.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return subject emitter keys vector:
        _inputs.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _inputs.count(_._1._1 == trunkKey)
}
