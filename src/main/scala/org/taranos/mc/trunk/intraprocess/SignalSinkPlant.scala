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


class SignalSinkPlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _sinks = mutable.HashMap.empty[(Trunk.Key, SignalSink.Key), SignalSink]

    def CreateSignalSink (
        trunk: Trunk,
        constructor: SignalSink.Constructor,
        listenerOpt: Option[ListenerElement]): SignalSink =
    {
        // Create sink element:
        val sink = new SignalSink(
            new SignalSink.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._mode),
            new SignalSink.Attrs(),
            new SignalSink.Refs(trunk.GetKey),
            new SignalSink.State(),
            listenerOpt)

        // 1: Add element to store:
        _sinks += (trunk.GetKey, sink.GetKey) -> sink

        // 2: Bind with trunk:
        trunk.BindSignalSink(sink.GetKey)

        // 3: Bind with parent:
        // N/A

        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // N/A

        // Return sink:
        sink
    }

    def DestroySignalSink (
        trunk: Trunk,
        destructor: SignalSink.Destructor): SignalSink.Key =
    {
        destructor._key match
        {
            case key: SignalSink.Key =>
                _sinks.get((trunk.GetKey, key)) match
                {
                    case Some(sink) =>
                        // 1: Unbind with children:
                        // N/A

                        // 2: Unbind with peers:
                        // Unbind with links:
                        sink.GetLinkKeys.clone().foreach(pair =>
                        {
                            _trunkModel.GetSignalLinkOpt(trunk.GetKey, pair._2) match
                            {
                                case Some(link) => link.UnbindSink(isReciprocal = true)

                                case None =>    // We don't care...
                            }
                        })
                        // Unbind with tap:
                        sink.GetTapKeyOpt.foreach(tapKey =>
                        {
                            _trunkModel.GetSignalTapOpt(trunk.GetKey, tapKey) match
                            {
                                case Some(tap) => tap.UnbindSink(isReciprocal = true)

                                case None =>    // We don't care...
                            }
                        })

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalSink(sink.GetKey)

                        // 5: Destroy children:
                        // N/A

                        // 6: Remove element from store:
                        _sinks -= ((trunk.GetKey, sink.GetKey))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalSinkUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalSinkInvalid)
        }

        // Return sink key:
        destructor._key
    }

    def DestroyAllSignalSinks (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each sink of trunk:
        _sinks.filter(_._1._1 == trunkKey).foreach(sinkPair =>
        {
            val ((_, pairSinkKey), _) = sinkPair
            val sinkDestructor = new SignalSink.Destructor(pairSinkKey)
            DestroySignalSink(trunk, sinkDestructor)
        })
    }

    def GetSignalSinkOpt (
        trunk: Trunk,
        key: SignalSink.Key,
        isRequired: Boolean = true): Option[SignalSink] =
    {
        // Lookup sink key:
        key match
        {
            case _: SignalSink.Key =>
                val opt = _sinks.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalSinkUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalSinkKeyInvalid)
        }
    }

    def GetSignalSinks (trunk: Trunk): Vector[SignalSink] =
    {
        val trunkKey = trunk.GetKey

        // Return sinks vector:
        _sinks.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalSinkKeys (trunk: Trunk): Vector[SignalSink.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return sink keys vector:
        _sinks.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _sinks.count(_._1._1 == trunkKey)
}
