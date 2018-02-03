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


class SignalSourcePlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _sources = mutable.HashMap.empty[(Trunk.Key, SignalSource.Key), SignalSource]

    def CreateSignalSource (
        trunk: Trunk,
        constructor: SignalSource.Constructor): SignalSource =
    {
        // Create source element:
        val source = new SignalSource(
            new SignalSource.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new SignalSource.Attrs(),
            new SignalSource.Refs(trunk.GetKey))

        // 1: Add element to store:
        _sources += (trunk.GetKey, source.GetKey) -> source

        // 2: Bind with trunk:
        trunk.BindSignalSource(source.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // N/A

        // Return source:
        source
    }

    def DestroySignalSource (
        trunk: Trunk,
        destructor: SignalSource.Destructor): SignalSource.Key =
    {
        destructor._key match
        {
            case key: SignalSource.Key =>
                _sources.get((trunk.GetKey, key)) match
                {
                    case Some(source) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        // Unbind with links:
                        source.GetLinkKeys.clone().foreach(pair =>
                        {
                            _trunkModel.GetSignalLinkOpt(trunk.GetKey, pair._2) match
                            {
                                case Some(link) => link.UnbindSource(isReciprocal = true)

                                case None =>    // We don't care...
                            }
                        })
                        // Unbind with tap:
                        source.GetTapKeyOpt.foreach(tapKey =>
                        {
                            _trunkModel.GetSignalTapOpt(trunk.GetKey, tapKey) match
                            {
                                case Some(tap) => tap.UnbindSource(isReciprocal = true)

                                case None =>    // We don't care...
                            }
                        })

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalSource(source.GetKey)

                        // 5: Destroy children:
                        // N/A

                        // 6: Remove element from store:
                        _sources -= ((trunk.GetKey, key))

                    case None => throw TrunkException(Cell.ErrorCodes.SignalSourceUnknown)
                }

            case _ => throw TrunkException(Cell.ErrorCodes.SignalSourceInvalid)
        }

        // Return source key:
        destructor._key
    }

    def DestroyAllSignalSources (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each source of trunk:
        _sources.filter(_._1._1 == trunkKey).foreach(sourcePair =>
        {
            val ((_, pairSourceKey), _) = sourcePair
            val sourceDestructor = SignalSource.Destructor(pairSourceKey)
            DestroySignalSource(trunk, sourceDestructor)
        })
    }

    def GetSignalSourceOpt (
        trunk: Trunk,
        key: SignalSource.Key,
        isRequired: Boolean = true): Option[SignalSource] =
    {
        // Lookup source key:
        key match
        {
            case _: SignalSource.Key =>
                val opt = _sources.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw TrunkException(Cell.ErrorCodes.SignalSourceUnknown)
                opt

            case _ => throw TrunkException(Cell.ErrorCodes.SignalSourceKeyInvalid)
        }
    }

    def GetSignalSources (trunk: Trunk): Vector[SignalSource] =
    {
        val trunkKey = trunk.GetKey

        // Return sources vector:
        _sources.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalSourceKeys (trunk: Trunk): Vector[SignalSource.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return source keys vector:
        _sources.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int =
        _sources.count(_._1._1 == trunkKey)
}
