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


class SignalPortPlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _ports = mutable.HashMap.empty[(Trunk.Key, SignalPort.Key), SignalPort]

    private
    def FormatTagForKey (tag: String): String =
    {
        // De-append any signal port substring:
        if (tag.endsWith(TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalPort))
            tag.dropRight(3)
        else
            tag
    }

    def CreateSignalPort (
        trunk: Trunk,
        interface: SignalInterface,
        constructor: SignalPort.Constructor): SignalPort =
    {
        // Create tap (child):
        val tap = _trunkModel.CreateSignalTaps(
            trunk.GetKey,
            Vector(
                new SignalTap.Constructor(
                    _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalTap,
                    _mode = constructor._mode))).head

        // Create port element:
        val port = new SignalPort(
            new SignalPort.Meta(
                TrunkElement.MakeUniqueKey(FormatTagForKey(constructor._tag), isObscured = false),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._aliasOpt,
                constructor._mode),
            new SignalPort.Attrs(),
            new SignalPort.Refs(
                trunk.GetKey,
                interface.GetKey,
                tap.GetKey))

        // 1: Add element to store:
        _ports += (trunk.GetKey, port.GetKey) -> port

        // 2: Bind with trunk:
        trunk.BindSignalPort(port.GetKey)

        // 3: Bind with parent:
        interface.BindPort(port.GetKey)

        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // N/A

        // Return port:
        port
    }

    def DestroySignalPort (
        trunk: Trunk,
        destructor: SignalPort.Destructor): SignalPort.Key =
    {
        destructor._key match
        {
            case key: SignalPort.Key =>
                _ports.get((trunk.GetKey, key)) match
                {
                    case Some(port) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        // N/A

                        // 3: Unbind with parent:
                        _trunkModel.GetSignalInterfaceOpt(
                            trunk.GetKey,
                            port.GetInterfaceKey,
                            isRequired = false) match
                        {
                            case Some(interface) => interface.UnbindPort(port.GetKey)

                            case None =>    // Ok, interface must have been destroyed already.
                        }

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalPort(port.GetKey)

                        // 5: Destroy children:
                        val tapDestructor = new SignalTap.Destructor(port.GetTapKey)
                        _trunkModel.DestroySignalTaps(trunk.GetKey, Vector(tapDestructor))

                        // 6: Remove element from store:
                        _ports -= ((trunk.GetKey, port.GetKey))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalPortUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalPortInvalid)
        }

        // Return port key:
        destructor._key
    }

    def DestroyAllSignalPorts (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each port of trunk:
        _ports.filter(_._1._1 == trunkKey).foreach(portPair =>
        {
            val ((_, pairPortKey), _) = portPair
            val portDestructor = new SignalPort.Destructor(pairPortKey)
            DestroySignalPort(trunk, portDestructor)
        })
    }

    def GetSignalPortOpt (
        trunk: Trunk,
        key: SignalPort.Key,
        isRequired: Boolean = true): Option[SignalPort] =
    {
        // Lookup port key:
        key match
        {
            case _: SignalPort.Key =>
                val opt = _ports.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalPortUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalPortKeyInvalid)
        }
    }

    def GetSignalPorts (trunk: Trunk): Vector[SignalPort] =
    {
        val trunkKey = trunk.GetKey

        // Return ports vector:
        _ports.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalPortKeys (trunk: Trunk): Vector[SignalPort.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return signal port keys vector:
        _ports.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _ports.count(_._1._1 == trunkKey)

    def LookupSignalPort (trunk: Trunk, lookupAlias: String): Option[SignalPort.Key] =
    {
        import scala.util.control.Breaks._

        val trunkKey = trunk.GetKey

        var portKeyOpt: Option[SignalPort.Key] = None
        breakable
        {
            for (portPair <- _ports.filter(_._1._1 == trunkKey))
            {
                val ((_, pairPortKey), pairPort) = portPair
                pairPort.GetAliasOpt match
                {
                    case Some(alias) =>
                        if (alias == lookupAlias)
                        {
                            portKeyOpt = Some(pairPortKey)
                            break()
                        }

                    case None =>
                }
            }
        }

        portKeyOpt
    }

}
