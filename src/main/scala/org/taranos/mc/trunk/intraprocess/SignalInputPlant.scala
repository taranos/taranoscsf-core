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
        // Get routing tap to bind with:
        var isTapParent: Boolean = false
        val routingTap: SignalTap = constructor._tappableKeyOpt match
        {
            // Get supplied tap:
            case Some(tapKey) if tapKey.isInstanceOf[SignalTap.Key] =>
                _trunkModel.GetSignalTapOpt(trunk.GetKey, tapKey.asInstanceOf[SignalTap.Key]).getOrElse(
                    throw TrunkException(Cell.ErrorCodes.SignalTapInvalid))

            // Make a new tap (child):
            case None =>
                isTapParent = true
                _trunkModel.CreateSignalTaps(
                    trunk.GetKey,
                    Vector(
                        SignalTap.Constructor(
                            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator +
                                TrunkModel.Glossary.kESignalTap,
                            _mode = constructor._mode))).head

            // Cannot bind with anything else, including other modulators:
            case _ =>
                throw TrunkException(Cell.ErrorCodes.SignalInputInvalid)
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
                isTapParent))

        // 1: Add element to store:
        _inputs += (trunk.GetKey, input.GetKey) -> input

        // 2: Bind with trunk:
        trunk.BindSignalInput(input.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        // Routing tap:
        if (!input.IsTapParent)
            routingTap.BindModulator(input.GetKey, isListener = false)

        // 5: Bind with children:

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
                        // Routing tap:
                        if (!input.IsTapParent)
                            _trunkModel.GetSignalTapOpt(trunk.GetKey, input.GetTapKey) match
                            {
                                case Some(tap) => tap.UnbindModulator()

                                case None =>    // We don't care...
                            }

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalInput(input.GetKey)

                        // 5: Destroy children:
                        // Routing tap:
                        if (input.IsTapParent)
                        {
                            val tapDestructor = SignalTap.Destructor(input.GetTapKey)
                            _trunkModel.DestroySignalTaps(trunk.GetKey, Vector(tapDestructor))
                        }

                        // 6: Remove element from store:
                        _inputs -= ((trunk.GetKey, input.GetKey))

                    case None => throw TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                }

            case _ => throw TrunkException(Cell.ErrorCodes.SignalInputInvalid)
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
            val inputDestructor = SignalInput.Destructor(pairInputKey)
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
                    throw TrunkException(Cell.ErrorCodes.SignalInputUnknown)
                opt

            case _ => throw TrunkException(Cell.ErrorCodes.SignalInputKeyInvalid)
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

    def GetElementCount (trunkKey: Trunk.Key): Int =
        _inputs.count(_._1._1 == trunkKey)
}
