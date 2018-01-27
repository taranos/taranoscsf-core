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
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.trunk.intraprocess.BiasedElement.{BiasedConstructorMetaDecoder, BiasedUpdateMetaDecoder}
import org.taranos.mc.trunk.intraprocess.Signal.SignalTypes
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder, CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json._


object SignalSink
{
    import scala.collection.mutable

    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalSink)
        extends SignalEndpoint.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased)
        extends SignalEndpoint.Meta[SignalSink.Key](
            new SignalSink.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)

    class Attrs
        extends SignalEndpoint.Attrs

    class Refs (trunkKey: Trunk.Key)
        extends SignalEndpoint.Refs(trunkKey)

    class State (
        val _traps: mutable.HashMap[TrunkElement.Key, Signal[_ >: Signal.SignalTypes]]
            = mutable.HashMap.empty[TrunkElement.Key, Signal[_ >: Signal.SignalTypes]])
        extends SignalEndpoint.State
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            import scala.language.existentials

            var report = super.Report(sections)

            var traps: JsObject = Json.obj()
            _traps.foreach(signalPair =>
            {
                val (key, signal) = signalPair
                traps = traps ++ Json.obj(
                    TrunkElement.EncodeKey(key) -> signal.Report(sections))
            })
            report ++= Json.obj(TrunkModel.Glossary.kTrap -> traps)

            report
        }
    }

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased)

    case class Destructor (
        _key: SignalSink.Key)

    case class Query (
        keys: Vector[SignalSink.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalSink.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalSink.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
//        _linkKeyOpt: Option[SignalLink.Key],
//        _tapKeyOpt: Option[SignalTap.Key],
        _modeOpt: Option[Signal.ModeEnum.Mode],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalSinkConstructorInvalid)

        val biasedMeta = new BiasedConstructorMetaDecoder(constructor)
        val mode = biasedMeta._modeOpt.getOrElse(Signal.ModeEnum.Unbiased)

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            mode)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalSink.Key](
            destructor, Cell.ErrorCodes.SignalSinkDestructorInvalid)

        new Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalSink.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalSink.Key](
            update, Cell.ErrorCodes.SignalSinkUpdateInvalid)

//        val linkKeyOpt: Option[SignalLink.Key] =
//            (update \ "r" \ "sl").validate[String] match
//            {
//                //                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[SignalLink.Key](value))
//                case JsSuccess(value, _) => assert(false); null
//                case JsError(errors) => None
//            }
//
//        val tapKeyOpt: Option[SignalTap.Key] =
//            (update \ "r" \ "st").validate[String] match
//            {
//                //                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[SignalTap.Key](value))
//                case JsSuccess(value, _) => assert(false); null
//                case JsError(errors) => None
//            }

        val biasedMeta = new BiasedUpdateMetaDecoder(update)
        
        val testableState = new TestableUpdateStateDecoder(update)

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
//            linkKeyOpt,
//            tapKeyOpt,
            biasedMeta._modeOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalSink (
    meta: SignalSink.Meta,
    attrs: SignalSink.Attrs,
    refs: SignalSink.Refs,
    state: SignalSink.State,
    listenerOpt: Option[ListenerElement])
    (implicit protected val _trunkModel: TrunkModel)
    extends SignalEndpoint[SignalSink.Key]
        with NotifierElement
{
    import scala.collection.mutable

    //
    // Meta:
    //
    protected
    val _meta = meta

    def GetMode = _meta._mode

    //
    // Attrs:
    //
    protected
    val _attrs = attrs

    //
    // Refs:
    //
    protected
    val _refs = refs

    def BindLink (
        linkKey: SignalLink.Key,
        part: String,
        isReciprocal: Boolean): Unit =
    {
        _refs._linkKeys += part -> linkKey

        if (isReciprocal)
        {
            _trunkModel.GetSignalLinkOpt(GetTrunkKey, linkKey) match
            {
                case Some(link) => link.BindSink(GetKey, isReciprocal = false)

                case None => throw new TrunkException(Cell.ErrorCodes.SignalLinkInvalid)
            }

        }
    }

    def BindTap (
        tapKey: SignalTap.Key,
        isReciprocal: Boolean): Unit =
    {
        _refs._tapKeyOpt = Some(tapKey)

        if (_refs._tapKeyOpt.isDefined && isReciprocal)
        {
            _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKeyOpt.get) match
            {
                case Some(tap) => tap.BindSink(GetKey, isReciprocal = false)

                case None => throw new TrunkException(Cell.ErrorCodes.SignalTapInvalid)
            }

        }
    }

    def GetLinkKeys: mutable.HashMap[String, SignalLink.Key] = _refs._linkKeys

    def GetTapKeyOpt = _refs._tapKeyOpt

    def UnbindLink (
        linkKeyOpt: Option[SignalLink.Key] = None,
        isReciprocal: Boolean): Unit =
    {
        linkKeyOpt.foreach(linkKey =>
        {
            if (isReciprocal)
            {
                _trunkModel.GetSignalLinkOpt(GetTrunkKey, linkKey) match
                {
                    case Some(link) => link.UnbindSink(isReciprocal = false)

                    case None =>    // We don't care...
                }
            }

            _refs._linkKeys.find(_._2 == linkKey) match
            {
                case Some(tuple) => _refs._linkKeys -= tuple._1

                case None =>        // Weird, but we don't care...
            }
        })
    }

    def UnbindTap (isReciprocal: Boolean): Unit =
    {
        if (_refs._tapKeyOpt.isDefined && isReciprocal)
        {
            _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKeyOpt.get) match
            {
                case Some(tap) => tap.UnbindSink(isReciprocal = false)

                case None =>    // We don't care...
            }
        }

        _refs._tapKeyOpt = None
    }

    //
    // State:
    //
    protected
    val _state = state

    def GetLastTrappedSignalOpt: Option[Signal[_ >: SignalTypes]] = GetTrappedSignalsOrdered.lastOption

    def GetTrappedSignalsOrdered: List[Signal[_ >: SignalTypes]] =
    {
        val signalsList = GetTrappedSignals.toList
        signalsList.sortWith(_._ordinal < _._ordinal)
    }

    def GetTrappedSignals =_state._traps.values

    def MarkAllLinks (signal: Signal[_ >: SignalTypes]) =
    {
        _refs._linkKeys.values.foreach(linkKey =>
        {
            _trunkModel.GetSignalLinkOpt(GetTrunkKey, linkKey) match
            {
                case Some(link) => link.SetMark(signal._ordinal)

                case None =>    // Weird, but we'll ignore.
            }
        })
    }

    def Propagate (
        signalOpt: Option[Signal[_ >: SignalTypes]],
        partOpt: Option[String]): Unit =
    {
        // Sink must be receiving a signal (sinks do not perform default propagation):
        val signal = signalOpt.getOrElse(
            throw new TrunkException(Cell.ErrorCodes.SignalInvalid, "signal must be valid"))

        // If signal is virtual then accept its mark:
        if (signal._scalar.isInstanceOf[Signal.Virtual])
        {
            SetMark(signal._ordinal)

            if (_refs._tapKeyOpt.isDefined)
            {
                val tapKey = _refs._tapKeyOpt.get
                _trunkModel.GetSignalTapOpt(GetTrunkKey, tapKey) match
                {
                    case Some(tap) => tap.Propagate(Some(signal))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalSinkTapless)
                }
            }
        }
        // Else propagate normally:
        else
        {
            val propagatorKey = signal._propagatorKeyOpt.getOrElse(GetKey)

            // Trap signal per propagator:
            _trunkModel.Log( s"$GetTag trapping ${signal.ToFriendly} from $propagatorKey")
            signal._propagatorKeyOpt = Some(GetKey)
            _state._traps(propagatorKey) = signal

            // If there is a listener then notify it:
            if (_listenerOpt.isDefined)
            {
                _trunkModel.Log( s"$GetTag notifying for ${signal.ToFriendly} from $propagatorKey")
                val listener = _listenerOpt.get
                listener.Notify(propagatorKey)
            }
        }
    }

    def Report (sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        // Add meta section:
        if (sections.HasMetaPropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSMeta -> _meta.Report(sections))

        // Add attrs section:
        if (sections.HasAttrsPropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSAttrs -> _attrs.Report(sections))

        // Add refs section:
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSRefs -> _refs.Report(sections))

        // Add state section:
        if (sections.HasStatePropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSState -> _state.Report(sections))

        report
    }

    override
    def SetMark (mark: Int) =
    {
        import scala.util.control.Breaks._

        // Determine if all of sink's links are marked:
        var isAllLinksMarked = true
        breakable
        {
            _refs._linkKeys.values.foreach(linkKey =>
            {
                _trunkModel.GetSignalLinkOpt(GetTrunkKey, linkKey) match
                {
                    case Some(link) =>
                        if (link.GetMark == kUnmarked)
                        {
                            isAllLinksMarked = false
                            break()
                        }

                    case None =>    // Weird, but we'll ignore since we're marking for destruction anyway.
                }
            })
        }
        // If all links are marked then accept mark:
        if (isAllLinksMarked)
            super.SetMark(mark)
    }

    def TestSignal (signal: Signal[_ >: SignalTypes]) =
    {
        Propagate(Some(signal))
    }


    SetListenerOpt(listenerOpt)
}
