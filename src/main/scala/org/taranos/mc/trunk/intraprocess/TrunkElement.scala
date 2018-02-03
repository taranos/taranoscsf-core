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
import org.taranos.mc.Common.{Propertyset, ReportSectionsParser, Reporter}
import org.taranos.mc.trunk.intraprocess.Signal._
import play.api.libs.json._


object TrunkElement
{
    type UniqueKey = String

    case class Key (
        _uniqueKey: UniqueKey,
        _symbol: Symbol)
    {
        def apply: (UniqueKey, Symbol) =
            (_uniqueKey, _symbol)
    }

    abstract
    class Meta[KeyType <: Key] (
        /**
         * Server-supplied non-mutable element identifier.
         */
        val _key: KeyType,

        /**
         * Client-supplied non-mutable element identifier (URI).  For key lookup, initial key correlation, etc.
         */
        val _tag: String,

        /**
         * Client-supplied non-mutable element description.  Can be used for branding, authentication, etc.
         */
        val _badgeOpt: Option[String],

        /**
         * Client-supplied mutable element identifier.  Can be used as a user-friendly name, etc.  Opaque to server.
         */
        var _nameOpt: Option[String],

        /**
         * Client-supplied mutable element description.  Opaque to server.
         */
        var _descriptionOpt: Option[String])

        extends Propertyset
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report: JsObject = Json.obj(
                TrunkModel.Glossary.kPMetaKey -> TrunkElement.EncodeKey(_key),
                TrunkModel.Glossary.kPMetaTag -> _tag)
            if (_badgeOpt.isDefined)
                report ++= Json.obj(TrunkModel.Glossary.kPMetaBadge -> _badgeOpt.get)
            if (_nameOpt.isDefined)
                report ++= Json.obj(TrunkModel.Glossary.kPMetaName -> _nameOpt.get)
            if (_descriptionOpt.isDefined)
                report ++= Json.obj(TrunkModel.Glossary.kPMetaDescription -> _descriptionOpt.get)
            report
        }
    }

    abstract
    class Attrs
        extends Propertyset

    abstract
    class Refs (
        /**
         * Store for the key of the trunk associated with the field element.
         */
        val _trunkKey: Trunk.Key)
        extends Propertyset

    abstract
    class State
        extends Propertyset

    abstract
    class Query[KeyType <: Key] (
        val _keys: Vector[KeyType],
        val _sectionsOpt: Option[String] = None)
    {
        def GetKeys: Vector[KeyType] =
            _keys
    }

    class CommonConstructorMetaDecoder (
        constructor: JsValue,
        errorCode: Int)
    {
        val _badgeOpt: Option[String] =
            (constructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaBadge).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }

        val _descriptionOpt: Option[String] =
            (constructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaDescription).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }

        val _nameOpt: Option[String] =
            (constructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaName).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }

        val _tag: String =
            (constructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaTag).validate[String] match
            {
                case JsSuccess(value, _) => value

                case JsError(_) =>
                    throw TrunkException(errorCode, "missing tag property")
            }
    }

    class CommonDestructorMetaDecoder[KeyType <: Key] (
        destructor: JsValue,
        errorCode: Int)
    {
        val _key =
            (destructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaKey).validate[String] match
            {
                case JsSuccess(value, _) =>
                    TrunkElement.DecodeKey[KeyType](value)

                case JsError(_) =>
                    throw TrunkException(errorCode, "missing key property")
            }
    }

    class CommonQueryDecoder[KeyType <: Key] (
        query: JsValue,
        isKeysRequired: Boolean = true)
    {
        val _keysOpt: Option[Vector[KeyType]] =
            if (isKeysRequired)
            {
                (query \ TrunkModel.Glossary.kPMetaKey).validate[Vector[String]] match
                {
                    case JsSuccess(value, _) => Some(value.map(TrunkElement.DecodeKey[KeyType]))

                    case JsError(errors) =>
                        val error = errors.head._2.head
                        error.message match
                        {
                            case "error.expected.jsarray" => Some(Vector.empty[KeyType])

                            case _ => throw TrunkException(Cell.ErrorCodes.KeysInvalid)
                        }
                }
            }
            else
                None

        val _sectionsOpt: Option[String] =
            (query \ TrunkModel.Glossary.kPQuerySections).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head)

                case JsError(_) => None
            }
    }

    class CommonUpdateMetaDecoder[KeyType <: Key] (
        update: JsValue,
        errorCode: Int)
    {
        val _key =
            (update \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaKey).validate[String] match
            {
                case JsSuccess(value, _) =>
                    TrunkElement.DecodeKey[KeyType](value)

                case JsError(_) =>
                    throw TrunkException(errorCode, "missing key property")
            }

        val _nameOpt: Option[String] =
            (update \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaName).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }

        val _descriptionOpt: Option[String] =
            (update \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaDescription).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }
    }

    def DecodeKey[KeyType <: Key] (encoded: String): KeyType =
    {
        val parts = encoded.split(TrunkModel.Glossary.kPartSeparator)
        if (parts.length < 2)
            throw TrunkException(Cell.ErrorCodes.KeyInvalid)

        val uniqueKey = if (parts.length == 3) parts(0) + TrunkModel.Glossary.kPartSeparator + parts(1) else parts(0)

        val key = parts(parts.length - 1) match
        {
            case TrunkModel.Glossary.kETrunk => new Trunk.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalInterface => new SignalInterface.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalPort => new SignalPort.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalSource => new SignalSource.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalSink => new SignalSink.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalLink => new SignalLink.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalTap => new SignalTap.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalInput => new SignalInput.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalBridge => new SignalBridge.Key(uniqueKey)

            case TrunkModel.Glossary.kESignalOutput => new SignalOutput.Key(uniqueKey)

            case TrunkModel.Glossary.kEEmitterPatch => new EmitterPatch.Key(uniqueKey)

            case TrunkModel.Glossary.kEOscillatorPatch => new OscillatorPatch.Key(uniqueKey)

            case _ => throw TrunkException(Cell.ErrorCodes.KeyInvalid)
        }

        key.asInstanceOf[KeyType]
    }

    def EncodeKey (key: TrunkElement.Key): String =
    {
        val uniqueKey = key._uniqueKey

        val symbol = key._symbol match
        {
            case 'Trunk => TrunkModel.Glossary.kETrunk

            case 'SignalInterface => TrunkModel.Glossary.kESignalInterface

            case 'SignalPort => TrunkModel.Glossary.kESignalPort

            case 'SignalSource => TrunkModel.Glossary.kESignalSource

            case 'SignalSink => TrunkModel.Glossary.kESignalSink

            case 'SignalLink => TrunkModel.Glossary.kESignalLink

            case 'SignalTap => TrunkModel.Glossary.kESignalTap

            case 'SignalInput => TrunkModel.Glossary.kESignalInput

            case 'SignalBridge => TrunkModel.Glossary.kESignalBridge

            case 'SignalOutput => TrunkModel.Glossary.kESignalOutput

            case 'EmitterPatch => TrunkModel.Glossary.kEEmitterPatch

            case 'OscillatorPatch => TrunkModel.Glossary.kEOscillatorPatch

            case _ => throw TrunkException(Cell.ErrorCodes.KeyInvalid)
        }

        uniqueKey + TrunkModel.Glossary.kPartSeparator + symbol
    }

    def DebangTag (tag: String): (Boolean, String) =
    {
        if (tag.startsWith(TrunkModel.Glossary.kBang))
            (true, tag.drop(1))
        else
            (false, tag)
    }

    def MakeUniqueKey (tag: String = "", isObscured: Boolean = true): UniqueKey =
    {
        val (wasBanged, debangedTag) = DebangTag(tag)

        if (!isObscured || wasBanged)
            debangedTag
        else
            java.util.UUID.randomUUID().toString
    }
}

trait TrunkElement[KeyType <: TrunkElement.Key]
    extends Reporter
{
    protected
    val _meta: TrunkElement.Meta[KeyType]

    def GetKey: KeyType =
        _meta._key

    def GetTag: String =
        _meta._tag

    def SetNameOpt (nameOpt: Option[String]): Unit =
        _meta._nameOpt = nameOpt

    def SetDescriptionOpt (descriptionOpt: Option[String]): Unit =
        _meta._descriptionOpt = descriptionOpt

    protected
    val _refs: TrunkElement.Refs

    def GetTrunkKey: Trunk.Key =
        _refs._trunkKey

    protected
    val _state: TrunkElement.State

    protected
    val _attrs: TrunkElement.Attrs
}


object ModulatableElement
{
    case class ModulatedSignals (
        _signals: Iterable[(Option[String], Signal[_ >: SignalTypes])] =
        Iterable.empty[(Option[String], Signal[_ >: SignalTypes])])
}

trait ModulatableElement
{
    /**
      * Modulate the given signal into zero or more new signals.
      */
    def Modulate (signal: Signal[_ >: SignalTypes]): ModulatableElement.ModulatedSignals

    /**
     * Return the key of the signal tap associated with the modulatable element.
     */
    def GetTapKey: SignalTap.Key
}

trait ModulatableElementKey


object AliasedElement
{
    class AliasedConstructorMetaDecoder (constructor: JsValue)
    {
        val _aliasOpt: Option[String] =
            (constructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaAlias).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val (_, debangedAlias) = TrunkElement.DebangTag(value)
                    Some(debangedAlias)

                case JsError(_) => None
            }
    }

    class AliasedUpdateMetaDecoder (update: JsValue)
    {
        val _aliasOpt: Option[String] =
            (update \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kPMetaAlias).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val (_, debangedAlias) = TrunkElement.DebangTag(value)
                    Some(debangedAlias)

                case JsError(_) => None
            }
    }
}

trait AliasedElement
{
    def GetAliasOpt: Option[String]

    def SetAliasOpt (aliasOpt: Option[String])
}


object BiasedElement
{
    class Key(uniqueKey: TrunkElement.UniqueKey, symbol: Symbol)
        extends TrunkElement.Key(uniqueKey, symbol)

    abstract
    class Meta[KeyType <: Key](
        key: KeyType,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],

        /**
         * Signal mode bias.
         */
        var _mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased)
        extends TrunkElement.Meta[KeyType](
            key,
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)
    {
        override
        def Report(sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++= Json.obj(TrunkModel.Glossary.kSignalMode -> Signal.ModeEnum.ToFriendly(_mode))

            report
        }
    }

    class BiasedConstructorMetaDecoder (constructor: JsValue)
    {
        val _modeOpt: Option[Signal.ModeEnum.Mode] =
            (constructor \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kSignalMode).validate[String] match
            {
                case JsSuccess(value, _) => Some(Signal.ModeEnum.FromFriendly(value))

                case JsError(_) => None
            }
    }

    class BiasedUpdateMetaDecoder (update: JsValue)
    {
        val _modeOpt: Option[Signal.ModeEnum.Mode] =
            (update \ TrunkModel.Glossary.kPSMeta \ TrunkModel.Glossary.kSignalMode).validate[String] match
            {
                case JsSuccess(value, _) => Some(Signal.ModeEnum.FromFriendly(value))

                case JsError(_) => None
            }
    }
}

trait BiasedElement[KeyType <: BiasedElement.Key]
    extends TrunkElement[KeyType]
{
    /**
     * Return a trunk element report.
     */
    protected
    def GetMode : Signal.ModeEnum.Mode
}


trait EgressElement
{
    def GetSignalOpt: Option[Signal[_ >: SignalTypes]]
}


trait IngressElement
{
    def PutSignal (signal: Signal[_ >: SignalTypes])
}


trait ListenerElement
{
    /**
     * Notify listener of a pending signal from propagator.
     * @param propagatorKey Propagator key of last signal.
     */
    def Notify (propagatorKey: TrunkElement.Key)
}


trait NotifierElement
{
    protected
    var _listenerOpt: Option[ListenerElement] = None

    /**
     * Set notifier's listener element.
     * @param listenerOpt The listener element.
     */
    def SetListenerOpt (listenerOpt: Option[ListenerElement]): Unit =
        _listenerOpt = listenerOpt
}


trait PropagatingElement
{
    protected
    val kUnmarked = -1

    private
    var _mark: Int = kUnmarked

    /**
      * Get element's path mark.
      */
    def GetMark: Int =
        _mark

    /**
      * Mark downstream path elements from current element using virtual signal.
      * @param markingSignal Virtual signal value.
      */
    def MarkPathSegment (markingSignal: Signal[Signal.Virtual]): Unit =
        Propagate(Some(markingSignal))

    /**
      * Set element's path mark.
      * @param mark Path mark value.
      */
    def SetMark (mark: Int): Unit =
        _mark = mark

    /**
     * Forward a signal to the next element of the propagation chain.
     * @param signalOpt Signal to forward.
     */
    def Propagate (
        signalOpt: Option[Signal[_ >: SignalTypes]] = None,
        partOpt: Option[String] = None)
}


trait TappableElement
{
    /**
     * Return the key of the signal source associated with the tappable element.
     */
    def GetSourceKey: SignalSource.Key
}

trait TappableElementKey


object TestableElement
{
    class TestableUpdateStateDecoder (update: JsValue)
    {
        val _signalEncodedOpt: Option[String] =
            (update \ TrunkModel.Glossary.kPSState \ TrunkModel.Glossary.kESignal).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }
    }
}

trait TestableElement
{
    /**
      * Forward a test signal to the next element of the propagation chain.
      * @param signal Signal to forward.
      */
    def PropagateTest (signal: Signal[_ >: SignalTypes])
}
