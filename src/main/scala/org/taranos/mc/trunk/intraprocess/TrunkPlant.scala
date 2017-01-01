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


class TrunkPlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _trunks = mutable.HashMap.empty[Trunk.Key, Trunk]

    def CreateTrunk (constructor: Trunk.Constructor): Trunk =
    {
        // Create trunk element:
        val trunk = new Trunk(
            new Trunk.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new Trunk.Attrs(),
            new Trunk.Refs())

        // 1: Add element to store:
        _trunks += trunk.GetKey -> trunk

        // 2: Bind with trunk:
        // N/A
        
        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // N/A

        // Create default interface:
        val interfaceConstructor = new SignalInterface.Constructor(
            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kEDefaultSignalInterface)
        _trunkModel.CreateSignalInterfaces(
            trunk.GetKey,
            Vector(interfaceConstructor))

        // Return trunk:
        trunk
    }

    def DestroyTrunk (destructor: Trunk.Destructor): Unit =
    {
        destructor.key match
        {
            case key: Trunk.Key =>
                _trunks.get(key) match
                {
                    case Some(trunk) =>
                        // 1: Unbind with children:
                        // We don't bother since it's expected that the plants of children will be destroyed soon.

                        // 2: Unbind with peers:
                        // N/A

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        // N/A

                        // 5: Destroy children:

                        // 6: Remove element from store:
                        _trunks -= trunk.GetKey

                    case None => throw new TrunkException(Cell.ErrorCodes.TrunkUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.TrunkInvalid)
        }
    }

    def DestroyAllTrunks (scope: Symbol): Unit =
    {
        // Destroy each trunk of cell:
        _trunks.foreach(trunkPair =>
        {
            val (trunkKey, _) = trunkPair
            val trunkDestructor = new Trunk.Destructor(trunkKey)
            DestroyTrunk(trunkDestructor)
        })
    }

    def GetTrunkOpt (
        key: Trunk.Key,
        isRequired: Boolean = true): Option[Trunk] =
    {
        // Lookup trunk :
        key match
        {
            case _: Trunk.Key =>
                val opt = _trunks.get(key)
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.TrunkUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.TrunkKeyInvalid)
        }
    }

    def GetTrunkKeys: Vector[Trunk.Key] =
    {
        // Return trunk keys vector:
        _trunks.keys.toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _trunks.count(_._1 == trunkKey)
}
