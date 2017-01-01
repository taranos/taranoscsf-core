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

package org.taranos.mc.field

import org.taranos.mc.Cell
import org.taranos.mc.Common.{Propertyset, ReportSectionsParser, Reporter}
import play.api.libs.json._


object FieldElement
{
    type UniqueKey = String

    case class Key (
        _uniqueKey: UniqueKey,
        _symbol: Symbol)
    {
        def apply = (_uniqueKey, _symbol)
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
                FieldModel.Glossary.kPMetaKey -> FieldElement.EncodeKey(_key),
                FieldModel.Glossary.kPMetaTag -> _tag)

            if (_badgeOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPMetaBadge -> _badgeOpt.get)

            if (_nameOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPMetaName -> _nameOpt.get)

            if (_descriptionOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPMetaDescription -> _descriptionOpt.get)

            report
        }
    }

    abstract
    class Attrs
        extends Propertyset

    abstract
    class Refs (
        /**
         * Store for the key of the field associated with the field element.
         */
        val _fieldKey: Field.Key)
        extends Propertyset

    abstract
    class State
        extends Propertyset

    abstract
    class Query[KeyType <: Key] (
        val _keys: Vector[KeyType],
        val _sectionsOpt: Option[String] = None)
    {
        def GetKeys: Vector[KeyType] = _keys
    }

    case class Call[KeyType <: FieldElement.Key] (
        _key: KeyType,
        _macro: JsObject)

    class CommonCallDecoder[KeyType <: Key] (
        call: JsValue,
        errorCode: Int)
    {
        val _elementKey =
            (call \ FieldModel.Glossary.kPMetaKey).validate[String] match
            {
                case JsSuccess(value, _) => FieldElement.DecodeKey[KeyType](value)

                case JsError(errors) => throw new FieldException(errorCode, "missing element key property")
            }

        val _macro: JsObject =
            (call \ FieldModel.Glossary.kMacro).validate[JsObject] match
            {
                case JsSuccess(value, _) => value

                case JsError(errors) => throw new FieldException(errorCode, "missing macro property")
            }
    }

    class CommonConstructorMetaDecoder (
        constructor: JsValue,
        errorCode: Int)
    {
        val _badgeOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaBadge).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val _descriptionOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaDescription).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val _nameOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaName).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val _tag: String =
            (constructor \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaTag).validate[String] match
            {
                case JsSuccess(value, _) => value

                case JsError(errors) => throw new FieldException(errorCode, "missing tag property")
            }
    }

    class CommonDestructorMetaDecoder[KeyType <: Key] (
        destructor: JsValue,
        errorCode: Int)
    {
        val _key =
            (destructor \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaKey).validate[String] match
            {
                case JsSuccess(value, _) => FieldElement.DecodeKey[KeyType](value)

                case JsError(errors) => throw new FieldException(errorCode, "missing key property")
            }

        val _scope: Symbol =
            (destructor \ FieldModel.Glossary.kPDestructorScope).validate[String] match
            {
                case JsSuccess(value, _) => value match
                {
                    case FieldModel.Glossary.`kPDestructorScopeShallow` => 'ScopeShallow

                    case FieldModel.Glossary.`kPDestructorScopeDeep` => 'ScopeDeep

                    case _ => throw new FieldException(errorCode, "invalid scope element")
                }

                case JsError(errors) => 'ScopeDeep
            }
    }

    class CommonQueryDecoder[KeyType <: Key] (
        query: JsValue,
        isKeysRequired: Boolean = true)
    {
        val _keysOpt: Option[Vector[KeyType]] =
            if (isKeysRequired)
            {
                (query \ FieldModel.Glossary.kPMetaKey).validate[Vector[String]] match
                {
                    case JsSuccess(value, _) =>
                        Some(value.map(FieldElement.DecodeKey[KeyType]))

                    case JsError(errors) =>
                        val error = errors.head._2.head
                        error.message match
                        {
                            case "error.expected.jsarray" => Some(Vector.empty[KeyType])

                            case _ => throw new FieldException(Cell.ErrorCodes.KeysInvalid)
                        }
                }
            }
            else
                None

        val _sectionsOpt: Option[String] =
            (query \ FieldModel.Glossary.kPQuerySections).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head)

                case JsError(errors) => None
            }
    }

    class CommonUpdateMetaDecoder[KeyType <: Key] (
        update: JsValue,
        errorCode: Int)
    {
        val _key =
            (update \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaKey).validate[String] match
            {
                case JsSuccess(value, _) => FieldElement.DecodeKey[KeyType](value)

                case JsError(errors) => throw new FieldException(errorCode, "missing key property")
            }

        val _nameOpt: Option[String] =
            (update \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaName).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val _descriptionOpt: Option[String] =
            (update \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaDescription).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }
    }

    def DecodeKey[KeyType <: Key] (encoded: String): KeyType =
    {
        val parts = encoded.split(FieldModel.Glossary.kPartSeparator)
        if (parts.length < 2)
            throw new FieldException(Cell.ErrorCodes.KeyInvalid)

        val uniqueKey =
        // If known special case:
            if (parts.length == 3)
            {
                // If "any" key case:
                if (parts(0).isEmpty && parts(1).isEmpty)
                    FieldModel.Glossary.kPartSeparator
                // Else it's some weird name:
                else
                    parts(0) + FieldModel.Glossary.kPartSeparator + parts(1)
            } else
            // Else it's the first part:
                parts(0)

        val key = parts(parts.length - 1) match
        {
            case FieldModel.Glossary.kEField => new Field.Key(uniqueKey)

            case FieldModel.Glossary.kEFieldEmitter => new FieldEmitter.Key(uniqueKey)

            case FieldModel.Glossary.kEFieldOscillator => new FieldOscillator.Key(uniqueKey)

            case FieldModel.Glossary.kESubject => new Subject.Key(uniqueKey)

            case FieldModel.Glossary.kESubjectEmitter => new SubjectEmitter.Key(uniqueKey)

            case FieldModel.Glossary.kESubjectOscillator => new SubjectOscillator.Key(uniqueKey)

            case FieldModel.Glossary.kEProbe => new Probe.Key(uniqueKey)

            case FieldModel.Glossary.kEProbeCollector => new ProbeCollector.Key(uniqueKey)

            case FieldModel.Glossary.kEProbeEmitter => new ProbeEmitter.Key(uniqueKey)

            case FieldModel.Glossary.kEProbeOscillator => new ProbeOscillator.Key(uniqueKey)

            case _ => throw new FieldException(Cell.ErrorCodes.KeyInvalid)
        }

        key.asInstanceOf[KeyType]
    }

    def EncodeKey (key: FieldElement.Key): String =
    {
        val uniqueKey = key._uniqueKey

        val symbol = key._symbol match
        {
            case 'Field => FieldModel.Glossary.kEField

            case 'FieldEmitter => FieldModel.Glossary.kEFieldEmitter

            case 'FieldOscillator => FieldModel.Glossary.kEFieldOscillator

            case 'Subject => FieldModel.Glossary.kESubject

            case 'SubjectEmitter => FieldModel.Glossary.kESubjectEmitter

            case 'SubjectOscillator => FieldModel.Glossary.kESubjectOscillator

            case 'Probe => FieldModel.Glossary.kEProbe

            case 'ProbeCollector => FieldModel.Glossary.kEProbeCollector

            case 'ProbeEmitter => FieldModel.Glossary.kEProbeEmitter

            case 'ProbeOscillator => FieldModel.Glossary.kEProbeOscillator

            case _ => throw new FieldException(Cell.ErrorCodes.KeyInvalid)
        }

        uniqueKey + FieldModel.Glossary.kPartSeparator + symbol
    }

    def DebangTag (tag: String): (Boolean, String) =
    {
        if (tag.startsWith(FieldModel.Glossary.kBang))
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

trait FieldElement[KeyType <: FieldElement.Key]
    extends Reporter
{
    protected
    val _meta: FieldElement.Meta[KeyType]

    def GetKey: KeyType = _meta._key

    def GetTag = _meta._tag

    def SetNameOpt (nameOpt: Option[String]) = _meta._nameOpt = nameOpt

    def SetDescriptionOpt (descriptionOpt: Option[String]) = _meta._descriptionOpt = descriptionOpt

    protected
    val _refs: FieldElement.Refs

    def GetFieldKey = _refs._fieldKey

    protected
    val _state: FieldElement.State

    protected
    val _attrs: FieldElement.Attrs
}

object AliasedElement
{
    class AliasedConstructorMetaDecoder (constructor: JsValue)
    {
        val _aliasOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaAlias).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val (_, debangedAlias) = FieldElement.DebangTag(value)
                    Some(debangedAlias)

                case JsError(errors) => None
            }
    }

    class AliasedUpdateMetaDecoder (update: JsValue)
    {
        val _aliasOpt: Option[String] =
            (update \ FieldModel.Glossary.kPSMeta \ FieldModel.Glossary.kPMetaAlias).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val (_, debangedAlias) = FieldElement.DebangTag(value)
                    Some(debangedAlias)

                case JsError(errors) => None
            }
    }
}

trait AliasedElement
{
    def GetAliasOpt: Option[String]

    def SetAliasOpt (aliasOpt: Option[String])
}


trait CallableElement
{
    /**
     * Invoke a macro operation on the element.
     * @param makro Macro to invoke.
     */
    def InvokeMacro (makro: JsObject): Unit =
    {
        throw new FieldException(Cell.ErrorCodes.MacroUnsupported)
    }
}
