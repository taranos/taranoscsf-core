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


class SignalLinkPlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _links = mutable.HashMap.empty[(Trunk.Key, SignalLink.Key), SignalLink]

    def CreateSignalLink (
        trunk: Trunk,
        constructor: SignalLink.Constructor): SignalLink =
    {
        // Create link element:
        val link = new SignalLink(
            new SignalLink.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._mode),
            new SignalLink.Attrs(constructor._partOpt),
            new SignalLink.Refs(
                trunk.GetKey,
                constructor._sinkKey,
                constructor._sourceKey))

        // 1: Add element to store:
        _links += (trunk.GetKey, link.GetKey) -> link

        // 2: Bind with trunk:
        trunk.BindSignalLink(link.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // N/A

        // Return link:
        link
    }

    def DestroySignalLink (
        trunk: Trunk,
        destructor: SignalLink.Destructor): SignalLink.Key =
    {
        destructor.key match
        {
            case key: SignalLink.Key =>
                _links.get((trunk.GetKey, key)) match
                {
                    case Some(link) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        link.UnbindAll(isReciprocal = true)

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalLink(link.GetKey)

                        // 5: Destroy children:
                        // N/A

                        // 6: Remove element from store:
                        _links -= ((trunk.GetKey, link.GetKey))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalLinkUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalLinkInvalid)
        }

        // Return link key:
        destructor.key
    }

    def DestroyAllSignalLinks (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each link of trunk:
        _links.filter(_._1._1 == trunkKey).foreach(linkPair =>
        {
            val ((_, pairLinkKey), _) = linkPair
            val linkDestructor = new SignalLink.Destructor(pairLinkKey)
            DestroySignalLink(trunk, linkDestructor)
        })
    }

    def GetSignalLinkOpt (
        trunk: Trunk,
        key: SignalLink.Key,
        isRequired: Boolean = true): Option[SignalLink] =
    {
        // Lookup link key:
        key match
        {
            case _: SignalLink.Key =>
                val opt = _links.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalLinkUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalLinkKeyInvalid)
        }
    }

    def GetSignalLinks (trunk: Trunk): Vector[SignalLink] =
    {
        val trunkKey = trunk.GetKey

        // Return links vector:
        _links.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalLinkKeys (trunk: Trunk): Vector[SignalLink.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return link keys vector:
        _links.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _links.count(_._1._1 == trunkKey)
}
