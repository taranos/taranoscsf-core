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

package org.taranos.mc.field

import org.taranos.mc.Cell
import org.taranos.mc.Common._
import play.api.libs.json.{JsObject, Json}


object Body
{
    import scala.collection.mutable

    type Position = Vector3

    type Rotation = Vector4

    abstract
    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Body)
        extends FieldElement.Key(uniqueKey, symbol)

    abstract
    class Meta[KeyType <: Key] (
        key: KeyType,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends FieldElement.Meta[KeyType](
            key,
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    abstract
    class Attrs
        extends FieldElement.Attrs

    abstract
    class Refs[ChildKeyType <: Emitter.Key] (
        fieldKey: Field.Key,
        val _emitterKeys: mutable.Set[ChildKeyType] = mutable.Set.empty[ChildKeyType],
        var _defaultEmitterKey: Emitter.Key = Emitter.kNoneKey)
        extends FieldElement.Refs(fieldKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            // Add default emitter ref:
            val emitterName = _defaultEmitterKey._symbol match
            {
                case 'SubjectEmitter => FieldModel.Glossary.kEDefaultSubjectEmitter

                case 'ProbeEmitter => FieldModel.Glossary.kEDefaultProbeEmitter

                case _ => ""
            }
            if (emitterName.nonEmpty)
                report ++=
                    Json.obj(emitterName -> FieldElement.EncodeKey(_defaultEmitterKey))

            // Add child refs:
            if (!sections.HasChildReports)
            {
                // Add emitter refs:
                val name = _defaultEmitterKey._symbol match
                {
                    case 'SubjectEmitter => FieldModel.Glossary.kESubjectEmitter

                    case 'ProbeEmitter => FieldModel.Glossary.kEProbeEmitter

                    case _ => ""
                }
                if (name.nonEmpty)
                    report ++=
                        Json.obj(name -> _emitterKeys.map(FieldElement.EncodeKey))
            }

            report
        }
    }

    abstract
    class State (
        // Position of subject in wavefield (cartesian: x, y, z):
        var _position: Body.Position = Defaults.kPosition,

        // Orientation of subject in wavefield (quaternion: w, x, y, z):
        var _rotation: Body.Rotation = Defaults.kRotation)
        extends FieldElement.State
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            super.Report(sections) ++
                Json.obj(
                    // For 2D cartesian (lat/lon):
                    FieldModel.Glossary.kPStatePosition -> Json.arr(
                        Reportable.ReportReal3(_position._x),
                        Reportable.ReportReal3(_position._y)),

                    // For unit heading:
                    FieldModel.Glossary.kPStateRotation -> Json.arr(
                        Reportable.ReportReal3(_rotation._x)))
        }
    }

    object Defaults
    {
        // Position:
        val kPosition: Position = new Position

        // Rotation:
        val kRotation: Rotation = new Rotation
    }

    def DecodePosition (encoded: String): Option[VectorN] =
    {
        val parts = encoded.split(FieldModel.Glossary.kPartSeparator)
        parts.length match
        {
            // For 2D cartesian (lat/lon):
            case 2 => Some(new Body.Position(
                parts(0).toDouble,
                parts(1).toDouble))

//            // For 3D cartesian:
//            case 3 => Some(new Body.Position(
//                parts(0).toDouble,
//                parts(1).toDouble,
//                parts(2).toDouble))

            case _ => throw FieldException(Cell.ErrorCodes.PositionInvalid)
        }
    }

    def DecodeRotation (encoded: String): Option[VectorN] =
    {
        val parts = encoded.split(FieldModel.Glossary.kPartSeparator)
        parts.length match
        {
            // For unit heading:
            case 1 => Some(new Body.Rotation(parts(0).toDouble))

//            // For quaternion:
//            case 4 => Some(new Body.Rotation(
//                parts(0).toDouble,
//                parts(1).toDouble,
//                parts(2).toDouble,
//                parts(3).toDouble))

            case _ => throw FieldException(Cell.ErrorCodes.RotationInvalid)
        }
    }
}

trait Body[KeyType <: Body.Key]
    extends FieldElement[KeyType]
{
    def BindEmitter (emitterKey: Emitter.Key)

    def GetDefaultEmitterKey: Emitter.Key

    def GetEmitterKeys: Set[_ <: Emitter.Key]

    def GetPosition: Body.Position

    def GetRotation: Body.Rotation

    def SetPosition (position: Body.Position)

    def SetRotation (rotation: Body.Rotation)

    def UnbindEmitter (
        emitterKey: Emitter.Key,
        isForcedUnbind: Boolean): Boolean
}
