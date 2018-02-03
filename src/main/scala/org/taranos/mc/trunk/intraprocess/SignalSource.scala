
package org.taranos.mc.trunk.intraprocess

import org.taranos.mc.Cell
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.trunk.intraprocess.Signal.SignalTypes
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder,
    CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json._


object SignalSource
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalSource)
        extends SignalEndpoint.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends SignalEndpoint.Meta[SignalSource.Key](
            new SignalSource.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            Signal.ModeEnum.Unbiased)

    class Attrs
        extends SignalEndpoint.Attrs

    class Refs (trunkKey: Trunk.Key)
        extends SignalEndpoint.Refs(trunkKey)

    class State
        extends SignalEndpoint.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None)

    case class Destructor (
        _key: SignalSource.Key)

    case class Query (
        keys: Vector[SignalSource.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalSource.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalSource.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
//        _linkKeyOpt: Option[SignalLink.Key],
//        _tapKeyOpt: Option[SignalTap.Key],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalSourceConstructorInvalid)

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalSource.Key](
            destructor, Cell.ErrorCodes.SignalSourceDestructorInvalid)

        Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalSource.Key](query)

        Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalSource.Key] (
            update, Cell.ErrorCodes.SignalSourceUpdateInvalid)

//        val linkKeyOpt: Option[SignalLink.Key] =
//            (json \ "r" \ "sl").validate[String] match
//            {
//                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[SignalLink.Key](value))
//
//                case JsError(errors) => None
//            }

//        val tapKeyOpt: Option[SignalTap.Key] =
//            (json \ "r" \ "st").validate[String] match
//            {
//                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[SignalTap.Key](value))
//
//                case JsError(errors) => None
//            }

        val testableState = new TestableUpdateStateDecoder(update)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
//            linkKeyOpt,
//            tapKeyOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalSource (
    meta: SignalSource.Meta,
    attrs: SignalSource.Attrs,
    refs: SignalSource.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends SignalEndpoint[SignalSource.Key]
{
    import scala.collection.mutable

    //
    // Meta:
    //
    protected
    val _meta = meta

    def GetMode: Signal.ModeEnum.Mode =
        _meta._mode

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
                case Some(link) => link.BindSource(GetKey, isReciprocal = false)

                case None => throw TrunkException(Cell.ErrorCodes.SignalLinkInvalid)
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
                case Some(tap) => tap.BindSource(GetKey, isReciprocal = false)

                case None => throw TrunkException(Cell.ErrorCodes.SignalTapInvalid)
            }
        }
    }

    def GetLinkKeys: mutable.HashMap[String, SignalLink.Key] =
        _refs._linkKeys

    def GetTapKeyOpt: Option[SignalTap.Key] =
        _refs._tapKeyOpt

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
                    case Some(link) => link.UnbindSource(isReciprocal = false)

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
                case Some(tap) => tap.UnbindSource(isReciprocal = false)

                case None =>    // We don't care...
            }
        }

        _refs._tapKeyOpt = None
    }

    //
    // State:
    //
    protected
    val _state = null

    def Propagate (
        signalOpt: Option[Signal[_ >: SignalTypes]],
        partOpt: Option[String]): Unit =
    {
        // Source must be injecting a signal (sources do not perform default propagation):
        val signal = signalOpt.getOrElse(
            throw TrunkException(Cell.ErrorCodes.SignalInvalid, "signal must be valid"))

        // If signal is virtual then accept its mark:
        if (signal._scalar.isInstanceOf[Signal.Virtual])
            SetMark(signal._ordinal)

        // If not connected to a link then the signal has dead-ended:
        if (_refs._linkKeys.isEmpty)
        {
            _trunkModel.Log(s"$GetTag stopagating ${signal.ToFriendly}")
        }
        else
        {
            // Make list of links to propagate to:
            val linkKeys: List[SignalLink.Key] =
                // If signal is virtual then return list of all links:
                if (signal._scalar.isInstanceOf[Signal.Virtual])
                {
                    _refs._linkKeys.values.toList
                }
                else
                    partOpt match
                    {
                        // If a part name was given then try to look up its link:
                        case Some(part) =>
                            val linkKeyOpt = _refs._linkKeys.get(part)
                            linkKeyOpt match
                            {
                                case Some(linkKey) =>
                                    List(linkKey)

                                case None =>
                                    List.empty[SignalLink.Key]
                            }

                        // If a part name was not given then return list of all links:
                        case None =>
                            _refs._linkKeys.values.toList
                    }

            // Propagate to list of links:
            if (linkKeys.nonEmpty)
            {
                linkKeys.foreach(linkKey =>
                {
                    _trunkModel.GetSignalLinkOpt(GetTrunkKey, linkKey) match
                    {
                        case Some(link) =>
                            signal._propagatorKeyOpt = Some(GetKey)
                            _trunkModel.Log(
                                s"$GetTag propagating ${signal.ToFriendly} to ${link.GetTag}")
                            link.Propagate(Some(signal))

                        case None => TrunkException(Cell.ErrorCodes.SignalSourceLinkless)
                    }
                })
            }
            else
                _trunkModel.Log(s"$GetTag stopagating ${signal.ToFriendly}")
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

        report
    }

    def PropagateTest (signal: Signal[_ >: SignalTypes]): Unit =
    {
        Propagate(Some(signal))
    }
}
