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


class SignalInterfacePlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _interfaces = mutable.HashMap.empty[(Trunk.Key, SignalInterface.Key), SignalInterface]

    def CreateSignalInterface (
        trunk: Trunk,
        constructor: SignalInterface.Constructor): SignalInterface =
    {
        // Create interface element:
        val interface = new SignalInterface(
            new SignalInterface.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new SignalInterface.Attrs(),
            new SignalInterface.Refs(trunk.GetKey))

        // 1: Add element to store:
        _interfaces += (trunk.GetKey, interface.GetKey) -> interface

        // 2: Bind with trunk:
        trunk.BindSignalInterface(interface.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // Ports are created by the model later.

        // Return interface:
        interface
    }

    def DestroySignalInterface (
        trunk: Trunk,
        destructor: SignalInterface.Destructor): SignalInterface.Key =
    {
        _interfaces.get((trunk.GetKey, destructor.key)) match
        {
            case Some(interface) =>
                // 1: Unbind with children:
                // N/A

                // 2: Unbind with peers:
                // N/A

                // 3: Unbind with parent:
                // N/A

                // 4: Unbind with trunk:
                trunk.UnbindSignalInterface(interface.GetKey)

                // 5: Destroy children:
                val portDestructors = interface.GetPortKeys.map(new SignalPort.Destructor(_))
                if (portDestructors.nonEmpty)
                    _trunkModel.DestroySignalPorts(trunk.GetKey, portDestructors.toVector)

                // 6: Remove element from store:
                _interfaces -= ((trunk.GetKey, interface.GetKey))

            case None => throw new TrunkException(Cell.ErrorCodes.SignalInterfaceUnknown)
        }

        // Return interface key:
        destructor.key
    }

    def DestroyAllSignalInterfaces (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each interface of trunk:
        _interfaces.filter(_._1._1 == trunkKey).foreach(interfacePair =>
        {
            val ((_, pairInterfaceKey), _) = interfacePair
            val interfaceDestructor = new SignalInterface.Destructor(pairInterfaceKey)
            DestroySignalInterface(trunk, interfaceDestructor)
        })
    }

    def GetSignalInterfaceOpt (
        trunk: Trunk,
        key: SignalInterface.Key,
        isRequired: Boolean = true): Option[SignalInterface] =
    {
        // Lookup interface key:
        key match
        {
            case _: SignalInterface.Key =>
                val opt = _interfaces.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalInterfaceUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalInterfaceKeyInvalid)
        }
    }

    def GetSignalInterfaces (trunk: Trunk): Vector[SignalInterface] =
    {
        val trunkKey = trunk.GetKey

        // Return interfaces vector:
        _interfaces.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalInterfaceKeys (trunk: Trunk): Vector[SignalInterface.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return interface keys vector:
        _interfaces.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _interfaces.count(_._1._1 == trunkKey)
}
